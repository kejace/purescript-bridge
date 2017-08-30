{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | PureScript types to be used for bridges, e.g. in "Language.PureScript.Bridge.Primitives".
module Language.PureScript.Bridge.PSTypes where

import           Control.Lens                        (views)
import           Data.Monoid
import qualified Data.Text                           as T
import           Control.Monad.Reader.Class


import           Language.PureScript.Bridge.Builder
import           Language.PureScript.Bridge.TypeInfo

-- | Uses  type parameters from 'haskType' (bridged).
psArray :: MonadReader BridgeData m => m PSType
psArray = TypeInfo "purescript-prim" "Prim" "Array" <$> psTypeParameters

psBool :: PSType
psBool = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Boolean"
  , _typeParameters = []
  }

-- | Uses  type parameters from 'haskType' (bridged).
psEither :: MonadReader BridgeData m => m PSType
psEither = TypeInfo "purescript-either" "Data.Either" "Either" <$> psTypeParameters

psInt :: PSVersion -> PSType
psInt PSv0 = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Int"
  , _typeParameters = []
  }
psInt _ = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Int"
  , _typeParameters = []
  }

psNumber :: PSVersion -> PSType
psNumber PSv0 = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "Number"
  , _typeParameters = []
  }
psNumber _ = TypeInfo {
    _typePackage = ""
  , _typeModule = ""
  , _typeName = "Number"
  , _typeParameters = []
  }

-- | Uses  type parameters from 'haskType' (bridged).
psMaybe :: MonadReader BridgeData m => m PSType
psMaybe = TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" <$> psTypeParameters

psString :: PSVersion -> PSType
psString PSv0 = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "String"
  , _typeParameters = []
  }
psString _ = TypeInfo {
    _typePackage = "purescript-prim"
  , _typeModule = "Prim"
  , _typeName = "String"
  , _typeParameters = []
  }

-- | Uses  type parameters from 'haskType' (bridged).
psTuple :: MonadReader BridgeData m => m PSType
psTuple = do
  size <- views (haskType . typeParameters) length
  let
    tupleModule = if size == 2 then "Data.Tuple" else "Data.Tuple.Nested"
    tupleName = "Tuple" <> if size == 2 then "" else T.pack (show size)
  TypeInfo "purescript-tuples" tupleModule tupleName <$> psTypeParameters

psUnit :: PSType
psUnit = TypeInfo  {
    _typePackage = "purescript-prelude"
  , _typeModule = "Prelude"
  , _typeName = "Unit"
  , _typeParameters = []
  }
