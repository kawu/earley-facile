import NLP.EarleyFacile


-- | Main entry.
main :: IO ()
main = main0


-----------------------------------------------------------
-- Grammar 0
-----------------------------------------------------------


gram0 :: [(String, [Either String String])]
gram0 =
    [ ("S",  [Left "NP", Left "VP"])
    , ("NP", [Right "det", Right "noun"])
    , ("NP", [Right "pnoun"])
    , ("VP", [Right "verb"])
    , ("VP", [Right "verb", Left "NP"])
    , ("VP", [Right "verb", Left "PP"])
    , ("VP", [Right "verb", Left "NP", Left "PP"])
    , ("PP", [Right "prep", Left "NP"]) ]


main0 :: IO ()
main0 = do
    -- "John drove the car"
    runEarley gram0
        [ ["pnoun"]         -- John
        , ["verb"]          -- drove
        , ["det"]           -- the
        , ["noun"]          -- car
        , ["prep"]          -- to
        , ["det"]           -- the
        , ["noun"] ]        -- station

-----------------------------------------------------------
-- Grammar 1
-----------------------------------------------------------


gram1 :: [(String, [Either String String])]
gram1 =
    [ ("S", [Left "NP", Left "VP"])
    , ("S", [Left "VP"])
    , ("VP", [Right "verb"])
    , ("VP", [Right "verb", Left "NP"])
    , ("NP", [Right "noun"])
    , ("NP", [Right "det", Right "noun"])
    , ("NP", [Right "det", Right "adj", Right "noun"]) ]


main1 :: IO ()
main1 = do
    -- "A human transplant is an elaborate procedure"
    runEarley gram1
        [ ["det"]           -- a
        , ["noun", "adj"]   -- human
        , ["noun", "verb"]  -- transplant
        , ["verb"]          -- is
        , ["det"]           -- an
        , ["adj"]           -- elaborate
        , ["noun"] ]        -- procedure


-----------------------------------------------------------
-- Grammar 2
-----------------------------------------------------------


gram2 :: [(String, [Either String String])]
gram2 =
    [ ("A", [Left "A", Left "A"])
    , ("A", [Right "a"]) ]


main2 :: IO ()
main2 = do
    -- "a" times 10
    runEarley gram2
        $ replicate 10 ["a"]
