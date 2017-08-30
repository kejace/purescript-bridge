{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Bridge (
    bridgeSumType
  , defaultBridge
  , module Bridge
  , writePSTypes
  , withConfigWritePSTypes
 ) where


import           Control.Applicative
import qualified Data.Map                              as M
import qualified Data.Set                              as Set
import qualified Data.Text.IO                          as T
import Control.Monad.Reader (lift, runReaderT)


import           Language.PureScript.Bridge.Builder    as Bridge
import           Language.PureScript.Bridge.Primitives as Bridge
import           Language.PureScript.Bridge.Printer    as Bridge
import           Language.PureScript.Bridge.SumType    as Bridge
import           Language.PureScript.Bridge.Tuple      as Bridge
import           Language.PureScript.Bridge.TypeInfo   as Bridge


-- | Your entry point to this library and quite likely all you will need.
--   Make sure all your types derive Generic and Typeable.
--   Typeable is not needed from ghc-7.10 on.
--
--   Then list all your types you want to use in PureScript and call 'writePSTypes':
--
--   >  let myTypes = [
--   >      mkSumType (Proxy :: Proxy MyType1)
--   >    , mkSumType (Proxy :: Proxy MyType2)
--   >   ]
--   >
--   >  writePSTypes "path/to/your/purescript/project" (buildBridge defaultBridge) myTypes
--
--   You can define your own type bridges based on 'defaultBridge':
--
--
--  >  myBridge = defaultBridge <|> mySpecialTypeBridge
--
--  and use it with 'writePSTypes':
--
--  >  writePSTypes "path/to/your/purescript/project" (buildBridge myBridge) myTypes
--
--   Find examples for implementing your own bridges in: "Language.PureScript.Bridge.Primitives".
--
--  == Result:
--   'writePSTypes' will write out PureScript modules to the given path, mirroring the hierarchy of the Haskell modules
--   the types came from. In addition a list of needed PS packages is printed to the console.
--
--   The list of needed packages is retrieved from the bridged 'TypeInfo' data, so make sure you set '_typePackage' correctly
--   in your own bridges, in order for this feature to be useful.
--
--  == Real world usage example (at time of this writing outdated, at time of reading hopefully fixed):
--   A real world use case of this library can be found <https://github.com/gonimo/gonimo-back/blob/master/app/PSGenerator.hs here>.
--
--   With custom bridges defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/CodeGen/TypeBridges.hs here> and
--   custom PS types defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/CodeGen/PSTypes.hs here>.
--
--   Parts of the generated output can be found <https://github.com/gonimo/gonimo-front/blob/master/src/Gonimo/Types.purs here>.
--
--   Note how 'Secret' and 'Key'
--   get translated according to our custom rules, with correct imports and everything.
--   Also the formatting is quite nice, would you have guessed that this code was generated?
--
--  == /WARNING/:
--   This function overwrites files - make backups or use version control!
writePSTypes :: FilePath -> FullBridge -> [SumType 'Haskell] -> IO ()
writePSTypes = withConfigWritePSTypes defaultConfig

-- Variant of writePSTypes that allows you to supply your own custom configuration
withConfigWritePSTypes :: Config -> FilePath -> FullBridge -> [SumType 'Haskell] -> IO ()
withConfigWritePSTypes config root br sts = flip runReaderT config $ do
    let bridged = map (bridgeSumType br) sts
    let modules = M.elems $ sumTypesToModules M.empty bridged
    mapM_ (printModule root) modules
    lift $ T.putStrLn "The following purescript packages are needed by the generated code:\n"
    let packages = Set.insert "purescript-profunctor-lenses" $ sumTypesToNeededPackages bridged
    mapM_ (lift . T.putStrLn . mappend "  - ") packages
    lift $ T.putStrLn "\nSuccessfully created your PureScript modules!"

-- | Translate all 'TypeInfo' values in a 'SumType' to PureScript types.
--
--   Example usage, with defaultBridge:
--
-- > data Foo = Foo | Bar Int | FooBar Int Text deriving (Generic, Typeable, Show)
--
-- > bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy Foo))
bridgeSumType :: FullBridge -> SumType 'Haskell -> SumType 'PureScript
bridgeSumType br (SumType t cs) = SumType (br t) $ map (bridgeConstructor br) cs

-- | Default bridge for mapping primitive/common types:
--   You can append your own bridges like this:
--
-- >  defaultBridge <|> myBridge1 <|> myBridge2
--
--   Find examples for bridge definitions in "Language.PureScript.Bridge.Primitives" and
--   "Language.PureScript.Bridge.Tuple".
defaultBridge :: BridgePart
defaultBridge = textBridge
    <|> stringBridge
    <|> listBridge
    <|> maybeBridge
    <|> eitherBridge
    <|> boolBridge
    <|> intBridge
    <|> doubleBridge
    <|> tupleBridge
    <|> unitBridge

-- | Translate types in a constructor.
bridgeConstructor :: FullBridge -> DataConstructor 'Haskell -> DataConstructor 'PureScript
bridgeConstructor br (DataConstructor name (Left infos)) =
    DataConstructor name . Left $ map br infos
bridgeConstructor br (DataConstructor name (Right record)) =
    DataConstructor name . Right $ map (bridgeRecordEntry br) record

-- | Translate types in a record entry.
bridgeRecordEntry :: FullBridge -> RecordEntry 'Haskell -> RecordEntry 'PureScript
bridgeRecordEntry br (RecordEntry label value) = RecordEntry label $ br value
