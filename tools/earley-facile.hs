import NLP.EarleyFacile


-- gram1 :: [(String, [Either String String])]
-- gram1 =
--     [ ("S", [Left "NP", Left "VP"])
--     , ("S", [Left "VP"])
--     , ("VP", [Right "verb"])
--     , ("VP", [Right "verb", Left "NP"])
--     , ("NP", [Right "noun"])
--     , ("NP", [Right "det", Right "noun"])
--     , ("NP", [Right "det", Right "adj", Right "noun"]) ]
-- 
-- 
-- main :: IO ()
-- main = do
--     -- "A human transplant is an elaborate procedure"
--     runEarley "S" gram1
--         [ ["det"]           -- a
--         , ["noun", "adj"]   -- human
--         , ["noun", "verb"]  -- transplant
--         , ["verb"]          -- is
--         , ["det"]           -- an
--         , ["adj"]           -- elaborate
--         , ["noun"] ]        -- procedure


gram2 :: [(String, [Either String String])]
gram2 =
    [ ("A", [Left "A", Left "A"])
    , ("A", [Right "a"]) ]


main :: IO ()
main = do
    -- "a a a a a a"
    runEarley "A" gram2
        . map (:[])
        $ replicate 6 "a"


