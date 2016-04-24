{-# LANGUAGE TupleSections #-}


-- | The module is responsible for parsing CFG grammars in which a distinction
-- between (a) non-terminals, (b) POS tags (terminals) and (c) regular words
-- is employed.
--
-- Format:
--
--  * Non-terminals are capitalized
--  * Terminals (POS tags) are written in lower-case
--  * Words are given in parentheses
--  * A line can be commented out using #
--  * Grammar rules are specified with ->
--  * Lexicon with <-


module NLP.EarleyFacile.GrammarParser
( CFG(..)
, SCFG
, parseSCFG
) where


import qualified Control.Monad.State.Strict as E
-- import           Control.Monad.Trans.Maybe (runMaybeT)

import qualified Data.Char                  as C
import qualified Data.List                  as L
import qualified Data.List.Split            as L
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S


-- | A CFG grammar with a distinguished lexicon part.
data CFG n p t = CFG
  { lexicon :: M.Map t (S.Set p)
  -- ^ Lexicon assignes a set of possible POS non-terminals to terminals
  , rules   :: S.Set (n, [Either n p])
  }


-- | Empty CFG grammar.
emptyCFG :: CFG n p t
emptyCFG = CFG M.empty S.empty


-- | A String-based CFG.
type SCFG = CFG String String String


saveRule :: (Ord n, Ord p) => (n, [Either n p]) -> E.State (CFG n p t) ()
saveRule r = E.modify' $ \cfg ->
  cfg {rules = S.insert r (rules cfg) }


saveLexe :: (Ord t, Ord p) => (t, p) -> E.State (CFG n p t) ()
saveLexe (term, pos) = E.modify' $ \cfg ->
  cfg {lexicon = M.insertWith S.union term (S.singleton pos) (lexicon cfg) }


-- | Parser a String-based representation of a SCFG grammar.
parseSCFG :: String -> SCFG
parseSCFG input = flip E.execState emptyCFG $ do
  mapM_ parseLine (lines input)


-- | Parse a single line of the grammar.
parseLine :: String -> E.State SCFG ()
parseLine =
  doit . trim
  where
    doit line
      | "#" `L.isPrefixOf` line = return ()
      | "->" `L.isInfixOf` line =
          maybe (error "parseRule") saveRule (parseRule line)
      | "<-" `L.isInfixOf` line =
          maybe (error "parseLexe") (mapM_ saveLexe) (parseLexe line)
      | otherwise = return ()


-- | A String-based rule and lexical entry.
type Rule = (String, [Either String String])
type Lexe = (String, String)


-- | Parse a grammar rule.
parseRule :: String -> Maybe Rule
parseRule line = do
  [hdStr, bdStr] <- return $
    trim <$> L.splitOn "->" line
  hd <- parseHead hdStr
  bdElems <- mapM parseElem $ words bdStr
  return (hd, bdElems)


-- | Parse a grammar lexicon entry.
parseLexe :: String -> Maybe [Lexe]
parseLexe line = do
  [term, rightStr] <- return $
    trim <$> L.splitOn "<-" line
  poss <- mapM parsePOS $ words rightStr
  return $ (term,) <$> poss


-- | Just make sure it is capitalized.
parseHead :: String -> Maybe String
parseHead [] = Nothing
parseHead xs@(x:_) = do
  E.guard $ C.isUpper x
  return xs


-- | Parse body element: non-terminal if capitalized, POS terminal otherwise.
parseElem :: String -> Maybe (Either String String)
parseElem [] = Nothing
parseElem x@(c:_) = Just $
  if C.isUpper c then Left x else Right x


-- | Just make sure it is *not* capitalized
parsePOS :: String -> Maybe String
parsePOS [] = Nothing
parsePOS x@(c:_) = do
  E.guard $ C.isLower c
  return x


-----------------
-- Misc
-----------------


-- -- | Take the `Just` value or raise error with the given error message.
-- fromJust :: String -> Maybe a -> a
-- fromJust err Nothing = error err
-- fromJust _ (Just x)  = x


-- | Trim whitespaces.
trim :: String -> String
trim = f . f
     where f = reverse . dropWhile C.isSpace
