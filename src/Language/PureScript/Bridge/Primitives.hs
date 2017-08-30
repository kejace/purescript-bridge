{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}


module Language.PureScript.Bridge.Primitives where


import           Data.Proxy
import           Language.PureScript.Bridge.Builder
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeInfo
import           Control.Monad.Reader.Class
import           Control.Lens

boolBridge :: BridgePart
boolBridge = typeName ^== "Bool" >> return psBool

eitherBridge :: BridgePart
eitherBridge = typeName ^== "Either" >> psEither

-- | Dummy bridge, translates every type with 'clearPackageFixUp'
dummyBridge :: MonadReader BridgeData m => m PSType
dummyBridge = clearPackageFixUp

intBridge :: BridgePart
intBridge = do
  haskType ^== mkTypeInfo (Proxy :: Proxy Int)
  pv <- psVersion
  return  $ psInt

doubleBridge :: BridgePart
doubleBridge = do
  typeName ^== "Double"
  pv <- view psVersion
  return $ psNumber pv

listBridge :: BridgePart
listBridge = typeName ^== "[]" >> psArray

maybeBridge :: BridgePart
maybeBridge = typeName ^== "Maybe" >> psMaybe

stringBridge :: BridgePart
stringBridge = do
  haskType ^== mkTypeInfo (Proxy :: Proxy String )
  pv <- view psVersion
  return $ psString pv

textBridge :: BridgePart
textBridge = do
  typeName   ^== "Text"
  typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
  pv <- view psVersion
  return $ psString pv

unitBridge :: BridgePart
unitBridge = typeName ^== "()" >> return psUnit
