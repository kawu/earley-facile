{-# LANGUAGE RecordWildCards #-}


module NLP.EarleyFacile where


import           Prelude hiding (init)
import           Control.Monad (void, forM_, when, msum)
import qualified Control.Monad.RWS.Strict   as RWS
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Options.Applicative hiding (some)
import           System.IO (hFlush, stdout)

import qualified Data.Tree as R
import           Data.Maybe (maybeToList)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Pipes as P


--------------------------------------------------
-- Item
--------------------------------------------------


-- | Position in the input sentence.
type Pos = Int


-- | Item's ID type.
type ID = Int


-- | Chart item based on some CFG rule.
data Item n t = Item
    { ihead :: n
    -- ^ Head of the underlying rule.
    , left  :: [Either n t]
    -- ^ The part of the body of the rule on the left of the dot.
    , right :: [Either n t]
    -- ^ The part on the right of the dot.
    , beg   :: Pos
    -- ^ Where the span begins.
    , end   :: Pos
    -- ^ Where the span ends.
    } deriving (Show, Eq, Ord)


-- | Deconstruct the right part of the item (i.e. non-terminals
-- still to process).
expects
    :: Item n t
    -> Maybe (Either n t, [Either n t])
expects = decoList . right


-- | Is it a passive (with the dot at the end) item?
passive :: Item n t -> Bool
passive = null .right


-- | Is it an active item?
active :: Item n t -> Bool
active = not . passive


-- | Print the item to stdout.
printItem
    :: (Ord n, Ord t, Show n, Show t)
    => Item n t -> IO ()
printItem Item{..} = do
    putStr "("
    putStr (show ihead)
    putStr " -> "
    putStr (unwords . map showLab $ reverse left)
    putStr " * "
    putStr (unwords . map showLab $ right)
    putStr ", "
    putStr (show beg)
    putStr ", "
    putStr (show end)
    putStr ")"


--------------------------------------------------
-- Traversal
--------------------------------------------------


-- | Traversal represents an action of infering a new item on the
-- basis of zero, one or two other chart items.
-- It can be seen as an application of one of the inference rules
-- specifying the parsing algorithm.
data Trav n t
    = Scan
        { _scanFrom :: Item n t
        -- ^ The input active state
        }
    | Comp
        { _pasArg   :: Item n t
        -- ^ The passive argument of the action
        , _actArg   :: Item n t
        -- ^ The active argument of the action
        }
    | Pred
    -- ^ Predicted item (we don't care how).
    deriving (Show, Eq, Ord)


-- | Print the item to stdout.
printTrav
    :: (Ord n, Ord t, Show n, Show t)
    => Trav n t -> IO ()
printTrav (Scan q) = do
    putStr "[S] " 
    printItem q
    putStr ""
printTrav (Comp p q) = do
    putStr "[C] " 
    printItem p
    putStr " + "
    printItem q
printTrav Pred = do
    putStr "[P]"


--------------------------------------------------
-- Traversal set
--------------------------------------------------


-- | Traversal set preserves information about the traversals
-- leading to the given chart item.
type TravSet n t = S.Set (Trav n t)


-- -- | Join two traversal sets.
-- joinTravSet
--     :: (Ord n, Ord t)
--     => (ID, TravSet n t)
--     -> (ID, TravSet n t)
--     -> (ID, TravSet n t)
-- joinTravSet (x1, x2) (y1, y2) = (max x1 y1, S.union x2 y2)


--------------------------------------------------
-- Earley monad
--------------------------------------------------


-- | A hypergraph dynamically constructed during parsing.
-- Plus some static information.
data Hype n t = Hype
    { gram  :: M.Map n (S.Set [Either n t])
    -- ^ The set of grammar rules
    , done  :: M.Map (Item n t) (TravSet n t) 
    -- ^ The set of *processed* chart items.
    , queue :: M.Map (Item n t) (TravSet n t) 
    -- ^ The set of *waiting* chart items.
    , idMap :: M.Map ID (Item n t)
    -- ^ The map from IDs to items.
    }


-- -- | Print the chart/hypergraph.
-- printHype   
--     :: (Ord n, Ord t, Show n, Show t)
--     => Hype n t
--     -> IO ()
-- printHype Hype{..} = do
--     forM_ (M.toList idMap) $ \(i, q) -> do
--         putStr "[" >> putStr (show i) >> putStr "] "
--         printItem q
--         putStrLn ""


-- | Print single column of the chart/hypergraph.
printColumn   
    :: (Ord n, Ord t, Show n, Show t)
    => Pos          -- ^ Which column?
    -> Bool         -- ^ Verbose?
    -> Hype n t
    -> IO ()
printColumn k verbose hype@Hype{..} = do
    forM_ (M.toList idMap) $ \(i, q) -> when (end q == k) $ do
        if M.member q done
            then do
                putStr "#"
                putStr (show i)
                putStr "# "
            else do
                putStr "<" 
                putStr (show i)
                putStr "> "
        printItem q
        putStrLn ""
        when verbose $ do
            forM_ (travList hype q) $ \t -> when (t /= Pred) $ do
                putStr "  * " >> printTrav t >> putStrLn ""


-- | Earley parser monad.  Contains the input sentence (reader)
-- and the state of the computation `Hype'.
type Earley n t = RWS.RWST [S.Set t] () (Hype n t) IO


-- | Read word from the given position of the input.
readInput :: Pos -> P.ListT (Earley n t) (S.Set t)
readInput i = do
    -- ask for the input
    xs <- RWS.ask 
    -- just a safe way to retrieve the i-th element
    each . take 1 . drop i $ xs


-- | List all rules with the given head non-terminal.
withHead :: (Ord n) => n -> P.ListT (Earley n t) [Either n t]
withHead x = do
    g <- RWS.gets gram
    each . maybe [] S.toList $ M.lookup x g


-- | Processed items which expect the given symbol and end on the
-- given position.
expectEnd :: (Ord n, Ord t) => n -> Pos -> P.ListT (Earley n t) (Item n t)
expectEnd x i = do
    m <- RWS.gets done
    q <- each (M.keys m)
    (Left nonTerm, _) <- some (expects q)
    RWS.guard (nonTerm == x && end q == i)
    return q


-- | Processed, passive items which provide the given symbol and
-- begin on the given position.
doneBeg :: (Ord n, Ord t) => n -> Pos -> P.ListT (Earley n t) (Item n t)
doneBeg x i = do
    m <- RWS.gets done
    p <- each (M.keys m)
    RWS.guard (passive p)
    RWS.guard (ihead p == x && beg p == i)
    return p


-- | Check if the given item is "done" (processed).
isDone :: (Ord n, Ord t) => Item n t -> Earley n t Bool
isDone q = M.member q <$> RWS.gets done


-- | Check if the given item is waiting.
isWait :: (Ord n, Ord t) => Item n t -> Earley n t Bool
isWait q = M.member q <$> RWS.gets queue


-- | Put an axiom item to the hypergraph's queue.
push0 :: (Ord n, Ord t) => Item n t -> Earley n t ()
push0 q = do
    i <- M.size <$> RWS.gets idMap
    RWS.modify' $ \h -> h
        { queue = M.insert q S.empty (queue h)
        , idMap = M.insert i q (idMap h) }


-- | Put an item to the hypergraph, together with the corresponding
-- traversal.
push :: (Ord n, Ord t) => Item n t -> Trav n t -> Earley n t ()
push q trav = do
    dn <- isDone q
    if dn then RWS.modify' $ \h -> h
        { done = M.insertWith
            S.union q
            (S.singleton trav)
            (done h) }
    else do
        wt <- isWait q
        RWS.modify' $ \h -> h
            { queue = M.insertWith
                S.union q
                (S.singleton trav)
                (queue h) }
        i <- M.size <$> RWS.gets idMap
        RWS.unless wt . RWS.modify' $ \h ->
            h {idMap = M.insert i q (idMap h)}


-- | Move the item from the queue to the set of processed (done)
-- items. 
shift :: (Ord n, Ord t) => Item n t -> Earley n t ()
shift q = do
    mayTrav <- M.lookup q <$> RWS.gets queue
    case mayTrav of
        Nothing -> return ()
        Just tr -> RWS.modify' $ \h -> h
            { done  = M.insert q tr (done h)
            , queue = M.delete q (queue h) }


--------------------------------------------------
-- Parsed trees
--------------------------------------------------


-- | Get the forest of parsed trees for a given item.
parsedTrees
    :: (Ord n, Ord t)
    => Hype n t
    -> Item n t
    -> [R.Forest (Either n t)]
parsedTrees h@Hype{..} = 
    fromActive
  where
    fromActive q = concatMap
        (fromActiveTrav q)
        (travList h q)
    fromActiveTrav _ (Scan q) = 
        [ mkLeaf t : ts
        | t  <- take 1 (right q)
        , ts <- fromActive q ]
    fromActiveTrav _ (Comp p q) =
        [ t : ts
        | ts <- fromActive q
        , t  <- fromPassive p ]
    fromActiveTrav _ Pred = [[]]
    fromPassive q =
        [ R.Node (Left $ ihead q) ts
        | ts <- fromActive q ]
    mkLeaf x = R.Node x []


-- | Get the list of traversals for the given item.
travList
    :: (Ord n, Ord t)
    => Hype n t
    -> Item n t
    -> [Trav n t]
travList Hype{..} q = maybe [] S.toList $
    msum [M.lookup q done, M.lookup q queue]


--------------------------------------------------
-- Inference rules
--------------------------------------------------


-- | Apply the axiom rule given the non-terminal we wish to
-- recognize.
axiom :: (Ord n) => n -> P.ListT (Earley n t) (Item n t)
axiom nonTerm = do
    body <- withHead nonTerm
    return Item
        { ihead = nonTerm
        , left  = []
        , right = body
        , beg   = 0
        , end   = 0 }


-- | Try to predict new items from the given item.
predict :: (Ord n) => Item n t -> P.ListT (Earley n t) (Item n t)
predict q = do
    (Left nonTerm, _) <- some (expects q)
    body <- withHead nonTerm
    return Item
        { ihead = nonTerm
        , left  = []
        , right = body
        , beg   = end q
        , end   = end q }


-- | Try to scan for the given item.
scan :: (Ord t) => Item n t -> P.ListT (Earley n t) (Item n t)
scan q = do
    (Right term, rest) <- some (expects q)
    termSet <- readInput (end q)
    RWS.guard $ S.member term termSet
    return q
        { left  = Right term : left q
        , right = rest
        , end   = end q + 1 }


-- | Try to complete for the given item.  Return the (active) items
-- matching on the left.
matchLeft
    :: (Ord n, Ord t)
    => Item n t
    -> P.ListT (Earley n t) (Item n t)
matchLeft p = do
    RWS.guard (passive p)
    expectEnd (ihead p) (beg p)


-- | Try to complete for the given item.  Return the (passive) items
-- matching on the right.
matchRight
    :: (Ord n, Ord t)
    => Item n t
    -> P.ListT (Earley n t) (Item n t)
matchRight q = do
    (Left nonTerm, _) <- some (expects q)
    doneBeg nonTerm (end q)


-- | Complete one item with another one.
complete
    :: (Ord n, Ord t)
    => Item n t -- ^ Active item
    -> Item n t -- ^ Passive item
    -> P.ListT (Earley n t) (Item n t)
complete q p = do
    (Left nonTerm, rest) <- some (expects q)
    RWS.guard
        (  passive p 
        && nonTerm == ihead p
        && end q == beg p )
    return q
        { left  = Left nonTerm : left q
        , right = rest
        , end   = end p }


-- | Process the item under the given ID.
proc
    :: (Ord n, Ord t, Show n, Show t)
    => ID -> Earley n t ()
proc i = do
    mayQ <- M.lookup i <$> RWS.gets idMap
    case mayQ of
      Nothing -> liftIO $ putStrLn "<<no such item>>"
      Just q  -> do
        b <- isDone q
        if b then do
            liftIO $ putStrLn "<<done>>"
        else do
            shift q
            P.runListT $ do
                p <- predict q
                lift (push p Pred)
                liftIO $ do
                    putStr "[P] "
                    printItem p
                    putStrLn ""
            P.runListT $ do
                p <- scan q
                liftIO $ do
                    putStr "[S] "
                    printItem p
                    putStrLn ""
                lift (push p $ Scan q)
            P.runListT $ do
                p <- matchLeft q
                liftIO $ do
                    putStr "[C] "
                    printItem p
                    putStr " => "
                q' <- complete p q
                liftIO $ do
                    printItem q'
                    putStrLn ""
                lift (push q' $ Comp p q)
            P.runListT $ do
                p <- matchRight q
                liftIO $ do
                    putStr "COMPLETE "
                    printItem p
                    putStr ": "
                q' <- complete q p
                liftIO $ do
                    printItem q'
                    putStrLn ""
                lift (push q' $ Comp q p)


--------------------------------------------------
-- Interactive
--------------------------------------------------


-- | Command for the interactive mode.
data Command
    = Print Pos Bool
    -- ^ Print a specific chart column
    | Quick Pos 
    -- ^ Process the entire column
    | Proc ID 
    -- ^ Process a specific item  
    | Forest ID 
    -- ^ Parse forest for a specific item  


optColumn :: Parser Pos
optColumn = argument auto
    ( metavar "POS"
   <> help "Chart column" )


optVerbose :: Parser Bool
optVerbose = switch
    ( short 'v'
   <> long "verbose"
   <> help "Verbose" )


optID :: Parser ID
optID = argument auto
    ( metavar "ID"
   <> help "Chart item ID" )


opts :: Parser Command
opts = subparser
        ( command "print"
            (info (Print <$> optColumn <*> optVerbose)
                (progDesc "Print the chart column")
                )
        <> command "process"
            (info (Proc <$> optID)
                (progDesc "Process an item")
                )
        <> command "forest"
            (info (Forest <$> optID)
                (progDesc "Print parsed forest for an item")
                )
        <> command "quick"
            (info (Quick <$> optColumn)
                (progDesc "Process the entire column")
                )
        )


-- | Run the given command.
run :: (Ord n, Ord t, Show n, Show t)
    => Command -> Earley n t ()
run (Print k v) = do
    h <- RWS.get
    liftIO $ printColumn k v h
run (Proc i) = proc i
run (Forest i) = do
    mayQ <- M.lookup i <$> RWS.gets idMap
    case mayQ of
        Nothing -> liftIO $ putStrLn "<<no such item>>"
        Just q  -> do
            h <- RWS.get
            let tss = parsedTrees h q
            forM_ tss $ \ts -> liftIO $ do
                putStr . R.drawForest . map (fmap show) $ ts
                putStrLn "----------------------------------"
run (Quick k) = do
    m <- RWS.gets idMap
    forM_ (M.toList m) $ \(i, q) -> do
        b <- isWait q
        when (b && end q == k) $ do
            liftIO $ do
                putStr "> "
                printItem q
                putStrLn ""
            proc i


-- | Main loop.
loop :: (Ord n, Ord t, Show n, Show t) => Earley n t ()
loop = do
    liftIO $ do
        putStr "> "
        hFlush stdout
    line <- liftIO getLine
    let res = execParserPure defaultPrefs
            (info opts desc) (words line)
    case getParseResult res of
        Nothing  -> liftIO $ putStrLn "<<unknown command>>"
        Just cmd -> run cmd
    loop
  where
    desc = progDesc "Earley facile"


-- | Run the parser on the given grammar and the given input.
runEarley
    :: (Ord n, Ord t, Show n, Show t)
    => n                    -- ^ Start symbol
    -> [(n, [Either n t])]  -- ^ The grammar 
    -> [[t]]                -- ^ The input
    -> IO ()
runEarley start rules input = void $
    RWS.execRWST (init >> loop) (map S.fromList input) $ Hype
        { gram = M.fromListWith S.union
            [ (hd, S.singleton bd)
            | (hd, bd) <- rules ]
        , done  = M.empty
        , queue = M.empty
        , idMap = M.empty }
  where 
    init = P.runListT $ do
        q <- axiom start
        lift (push0 q)


--------------------------------------------------
-- Utilities
--------------------------------------------------


-- | Deconstruct list.  Utility function.  Similar to `unCons`.
decoList :: [a] -> Maybe (a, [a])
decoList [] = Nothing
decoList (y:ys) = Just (y, ys)


-- -- | MaybeT transformer.
-- maybeT :: Monad m => Maybe a -> MaybeT m a
-- maybeT = MaybeT . return


-- | ListT from a list.
each :: Monad m => [a] -> P.ListT m a
each = P.Select . P.each


-- | ListT from a maybe.
some :: Monad m => Maybe a -> P.ListT m a
some = each . maybeToList


-- | Showing labels.
showLab :: (Show n, Show t) => Either n t -> String
showLab (Left x) = show x
showLab (Right x) = show x
