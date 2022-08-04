{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Update YAML preserving top-level key order, blank lines and comments.
module Stack.YamlUpdate
    ( encodeInOrder
    , redress
    , RawYaml(..)
    , RawYamlLine(..)
    , YamlKey(..)
    ) where

import           Stack.Prelude
import           Data.Coerce (coerce)
import qualified Data.List as L
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml
import qualified RIO.Text as T
import qualified RIO.Map as Map

newtype RawYaml = RawYaml Text deriving newtype Display
newtype RawYamlLine = RawYamlLine Text
newtype YamlKey = YamlKey Text deriving newtype (Eq, Display)
newtype YamlLineBlank = YamlLineBlank Int deriving newtype Display
newtype YamlLineComment = YamlLineComment (Int, Text)
newtype YamlLineReindex = YamlLineReindex (Int, Int)

data YamlLines =
    YamlLines
        { blanks :: ![YamlLineBlank]
        -- ^ The line numbers of blank lines.
        , wholeLineComments :: ![YamlLineComment]
        -- ^ Comments where # is the first non-space character in that line so
        -- that the comment takes up the whole line. Captured with the leading
        -- spaces.
        , partLineComments :: ![YamlLineComment]
        -- ^ Comments that have been apended to a line.
        , reindices :: ![YamlLineReindex]
        -- ^ Bumps for line numbers that will need to be moved when blank lines
        -- and whole line comments are added back in.
        }

-- | Puts blank lines and comments from the original lines into the update.
redress :: [RawYamlLine] -> RawYaml -> Text
redress
    (pegLines -> YamlLines{blanks, wholeLineComments, partLineComments, reindices})
    (RawYaml t) = let xs = zip [1 ..] (T.lines t) in
        T.concat
            [
                T.unlines . fromMaybe [x] $ do
                    let reindex = flip L.lookup (coerce reindices :: [(Int, Int)])
                    i' <- reindex i
                    j' <- reindex j
                    let lineNumbers = filter (\b -> i' <= b && b < j') $ coerce blanks
                    let ls = (\line -> YamlLineComment (line, "")) <$> lineNumbers
                    let filterLineNumber = filter ((\c -> i' <= c && c < j') . commentLineNumber) 
                    let cs = filterLineNumber wholeLineComments
                    let ps = filterLineNumber partLineComments
                    let blankLinesAsComments = L.sortOn commentLineNumber (ls ++ cs)
                    let x' = maybe
                                x
                                (\(YamlLineComment (_, c)) -> x <> " " <> comment c)
                                (L.find ((== i') . commentLineNumber) ps)

                    return (x' : ((\(YamlLineComment (_, c)) -> c) <$> blankLinesAsComments))

            | (i, x) <- xs
            | (j, _) <- drop 1 xs ++ [(0, "")]
            ]

-- | Uses the order of the keys in the original to preserve the order in the
-- update except that inserting a key orders it last.
encodeInOrder
    :: [RawYamlLine]
    -> [YamlKey]
    -> YamlKey
    -> Yaml.Object
    -> Either UnicodeException RawYaml
encodeInOrder rawLines keysFound upsertKey@(YamlKey k) yObject =
    let keyLine = findKeyLine rawLines
        ixMap = Map.fromList $ (\yk@(YamlKey x) -> (x, keyLine yk)) <$> keysFound
        preservingCompare x y =
            -- If updating then preserve order but if inserting then put last.
            if | upsertKey `L.elem` keysFound -> Map.lookup x ixMap `compare` Map.lookup y ixMap
               | k == x, k == y -> EQ
               | k == x -> GT
               | k == y -> LT
               | otherwise -> Map.lookup x ixMap `compare` Map.lookup y ixMap

        keyCmp = Yaml.setConfCompare preservingCompare Yaml.defConfig
    
    in RawYaml <$> decodeUtf8' (Yaml.encodePretty keyCmp yObject)

findKeyLine :: [RawYamlLine] -> YamlKey -> Maybe Int
findKeyLine rawLines (YamlKey x) = join . listToMaybe . take 1 . dropWhile isNothing $
    [ if x `T.isPrefixOf` y then Just i else Nothing
    | RawYamlLine y <- rawLines
    | i <- [1 ..]
    ]

commentLineNumber :: YamlLineComment -> Int
commentLineNumber (YamlLineComment (c, _)) = c

instance Display YamlLineComment where
    textDisplay (YamlLineComment (i, s)) = textDisplay . T.pack $ show (i, T.unpack s)

comment :: Text -> Text
comment = T.dropWhile (/= '#') 

-- | Gather enough information about lines to peg line numbers so that blank
-- lines and comments can be reinserted later.
pegLines :: [RawYamlLine] -> YamlLines
pegLines rawLines =
    let (ls, rs) = partitionEithers
                    [
                        if | y == "" -> Left . Left $ YamlLineBlank i

                           | "#" `T.isPrefixOf` T.dropWhile (== ' ') y ->
                                Left . Right $ YamlLineComment (i, y)

                           | otherwise ->
                                if "#" `T.isPrefixOf` comment y
                                    then Right . Left $ YamlLineComment (i, y)
                                    else Right $ Right i

                    | RawYamlLine y <- rawLines
                    | i <- [1 ..]
                    ]

        (blanks, wholeLineComments) = partitionEithers ls
        (partLineComments, contentLines) = partitionEithers rs
        indexLines = L.sort $ contentLines ++ (commentLineNumber <$> partLineComments)
        reindex = zipWith (curry YamlLineReindex) [1 ..] indexLines

    in YamlLines blanks wholeLineComments partLineComments reindex