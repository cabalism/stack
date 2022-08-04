{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Update YAML preserving top-level key order, blank lines and line comments.
module Stack.YamlUpdate
    ( encodeInOrder
    , keepBlanks
    , RawConfig(..)
    , RawConfigLine(..)
    , YamlKey(..)
    ) where

import           Stack.Prelude
import           Data.Coerce (coerce)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml
import qualified RIO.Text as RioT
import qualified RIO.Map as Map

newtype RawConfig = RawConfig Text deriving newtype Display
newtype RawConfigLine = RawConfigLine Text
newtype YamlKey = YamlKey Text deriving newtype Display

keepBlanks :: HasLogFunc env => [RawConfigLine] -> YamlKey -> RIO env (Text -> Text)
keepBlanks rawConfigLines (YamlKey addedKey) = do
    let (blanks, (wholeLineComments, partLineComments), reindices) = findBlanks rawConfigLines
    logInfo "BLANK LINE NUMBERS"
    mapM_ (logInfo . display) blanks
    logInfo "WHOLE LINE COMMENTS"
    mapM_ (logInfo . display) wholeLineComments
    logInfo "PART LINE COMMENTS"
    mapM_ (logInfo . display) partLineComments
    return $ \ t ->
        let (ks, others) = L.partition (addedKey `RioT.isPrefixOf`) (RioT.lines t)
            xs = zip [1 ..] others

            ys =
                [
                    RioT.unlines . fromMaybe [x] $ do
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

        -- Append the added key line, assumed to be one line.
        in RioT.concat $ ys ++ take 1 ks
        
encodeInOrder :: HasLogFunc env => [RawConfigLine] -> [YamlKey] -> Yaml.Object -> RIO env (Either UnicodeException Text)
encodeInOrder rawConfigLines keysFound config' = do
    mapM_ (logInfo . display) keysFound
    let findLine = findIdx rawConfigLines
    let ixs = (\yk@(YamlKey x) -> (x, findLine yk)) <$> keysFound
    let mapIxs :: Map Text (Maybe Int)
        mapIxs = Map.fromList ixs
    let firstLineCompare :: Text -> Text -> Ordering
        firstLineCompare x y = Map.lookup x mapIxs `compare` Map.lookup y mapIxs
    let keyCmp = Yaml.setConfCompare firstLineCompare Yaml.defConfig
    return . decodeUtf8' $ Yaml.encodePretty keyCmp config'

findIdx :: [RawConfigLine] -> YamlKey -> Maybe Int
findIdx rawConfigLines (YamlKey x) = join . listToMaybe . take 1 . dropWhile isNothing $
    [ if x `RioT.isPrefixOf` y then Just i else Nothing
    | RawConfigLine y <- rawConfigLines
    | i <- [1 ..]
    ]

newtype YamlLineBlank = YamlLineBlank Int deriving newtype Display
newtype YamlLineComment = YamlLineComment (Int, Text)
newtype YamlLineReindex = YamlLineReindex (Int, Int)

commentLineNumber :: YamlLineComment -> Int
commentLineNumber (YamlLineComment (c, _)) = c

instance Display YamlLineComment where
    textDisplay (YamlLineComment (i, s)) = textDisplay . T.pack $ show (i, RioT.unpack s)

comment :: Text -> Text
comment = RioT.dropWhile (/= '#') 

findBlanks :: [RawConfigLine] -> ([YamlLineBlank], ([YamlLineComment], [YamlLineComment]), [YamlLineReindex])
findBlanks rawConfigLines =
    let (ls, rs) = partitionEithers
                    [
                        if | y == "" -> Left . Left $ YamlLineBlank i
                           | "#" `RioT.isPrefixOf` RioT.dropWhile (== ' ') y -> Left . Right $ YamlLineComment (i, y)
                           | otherwise ->
                                if "#" `RioT.isPrefixOf` comment y
                                    then Right . Left $ YamlLineComment (i, y)
                                    else Right $ Right i
                    | RawConfigLine y <- rawConfigLines
                    | i <- [1 ..]
                    ]
        (blanks, wholeLineComments) = partitionEithers ls
        (partLineComments, contentLines) = partitionEithers rs
        indexLines = L.sort $ contentLines ++ (commentLineNumber <$> partLineComments)
    in (blanks, (wholeLineComments, partLineComments), zipWith (curry YamlLineReindex) [1 ..] indexLines)
