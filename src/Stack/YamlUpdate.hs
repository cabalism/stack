{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
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
newtype YamlKey = YamlKey Text deriving newtype (Eq, Display)

keepBlanks :: HasLogFunc env => [RawConfigLine] -> [YamlKey] -> YamlKey -> RIO env (Text -> Text)
keepBlanks rawConfigLines keys key@(YamlKey addedKey) = do
    let YamlLines{blanks, wholeLineComments, partLineComments, reindices} = pegLines rawConfigLines
    logInfo "BLANK LINE NUMBERS"
    mapM_ (logInfo . display) blanks
    logInfo "WHOLE LINE COMMENTS"
    mapM_ (logInfo . display) wholeLineComments
    logInfo "PART LINE COMMENTS"
    mapM_ (logInfo . display) partLineComments
    return $ \ t ->
        let (ks, others) =
                -- If the key isn't there already partition it for later append.
                L.partition
                    (if key `L.elem` keys
                        then const False
                        else (addedKey `RioT.isPrefixOf`))
                    (RioT.lines t)

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
    logInfo "TOP-LEVEL KEYS"
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

-- | Gather enough information about lines to peg line numbers so that blank
-- lines and comments can be reinserted later.
pegLines :: [RawConfigLine] -> YamlLines
pegLines rawConfigLines =
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
        reindex = zipWith (curry YamlLineReindex) [1 ..] indexLines

    in YamlLines blanks wholeLineComments partLineComments reindex
