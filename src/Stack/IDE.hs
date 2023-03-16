{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Functions for IDEs.
module Stack.IDE
  ( OutputStream (..)
  , ListPackagesCmd (..)
  , listPackages
  , listTargets
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Prelude
import           Stack.Types.Config
                   ( BuildConfig (..), HasBuildConfig (..), ppComponents )
import           Stack.Types.NamedComponent
                   ( NamedComponent, renderPkgComponent )
import           Stack.Types.SourceMap ( ProjectPackage (..), SMWanted (..) )
import           System.IO ( putStrLn )

data OutputStream
  = OutputLogInfo
  | OutputStdout

data ListPackagesCmd
  = ListPackageNames
  | ListPackageCabalFiles

outputFunc :: HasTerm env => OutputStream -> String -> RIO env ()
outputFunc OutputLogInfo = prettyInfo . fromString
outputFunc OutputStdout  = liftIO . putStrLn

-- | List the packages inside the current project.
listPackages ::
     HasBuildConfig env
  => OutputStream
  -> ListPackagesCmd
  -> RIO env ()
listPackages stream flag = do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  let strs = case flag of
        ListPackageNames ->
          map packageNameString (Map.keys packages)
        ListPackageCabalFiles ->
          map (toFilePath . ppCabalFP) (Map.elems packages)
  mapM_ (outputFunc stream) strs

-- | List the targets in the current project.
listTargets :: forall env. HasBuildConfig env => OutputStream -> RIO env ()
listTargets stream = do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  pairs <- concat <$> Map.traverseWithKey toNameAndComponent packages
  outputFunc stream $ T.unpack $ T.intercalate "\n" $
    map renderPkgComponent pairs
 where
  toNameAndComponent ::
       PackageName
    -> ProjectPackage
    -> RIO env [(PackageName, NamedComponent)]
  toNameAndComponent pkgName' =
    fmap (map (pkgName', ) . Set.toList) . ppComponents
