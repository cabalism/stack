{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

-- | Type representing Haddock interface options.
module Stack.Types.InterfaceOpt
  ( InterfaceOpt (..)
  ) where

import           Data.Time ( UTCTime )
import           Stack.Prelude

-- | Type representing Haddock interface options.
data InterfaceOpt = InterfaceOpt
  { readInterfaceArgs :: ![String]
  , srcInterfaceFileModTime :: !UTCTime
  , srcInterfaceFile :: !(Path Abs File)
  , destInterfaceFile :: !(Path Abs File)
  }
  deriving (Eq, Ord)
