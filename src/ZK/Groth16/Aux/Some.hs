
-- | Existential wrapper

{-# LANGUAGE ScopedTypeVariables, DataKinds, KindSignatures, TypeApplications, 
             ExistentialQuantification, Rank2Types, 
             StandaloneDeriving, FlexibleInstances 
  #-}
module ZK.Groth16.Aux.Some 
  ( -- * Curves
    SomeCurve(..)
  , quotedCurve
  , someCurveProxy
    -- * Curve hints
  , CurveHint
  , mergeHint
  , guessCurve
    -- * Existential wrapper
  , Some(..)
  , withSome
    -- * Annotate with proxy
  , WithProxy(..)
  , SomeWithProxy
  , extractCurve
  ) 
  where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Kind

import ZK.Algebra.Class.Pairing 

import ZK.Algebra.Curves.BN128     ()      -- importing just the...
import ZK.Algebra.Curves.BLS12_381 ()      -- ...PairingCurve instances

import ZK.Groth16.Types

--------------------------------------------------------------------------------

someCurveProxy :: SomeCurve -> Some Proxy
someCurveProxy curve = case curve of
  BN128      -> MkSome (Proxy @'BN128    )
  BLS12_381  -> MkSome (Proxy @'BLS12_381)

quotedCurve :: SomeCurve -> String
quotedCurve c = "`" ++ show c ++ "`"

--------------------------------------------------------------------------------
-- * Curve hints

type CurveHint = Maybe SomeCurve

mergeHint :: CurveHint -> CurveHint -> CurveHint
mergeHint Nothing  Nothing  = Nothing
mergeHint (Just x) Nothing  = Just x
mergeHint Nothing  (Just y) = Just y
mergeHint (Just x) (Just y) = if x == y 
  then Just x 
  else error $ "incompatible elliptic curves: " ++ quotedCurve x ++ " vs. " ++ quotedCurve y

guessCurve :: CurveHint -> CurveHint -> Either String SomeCurve
guessCurve Nothing  Nothing  = Left "unknown elliptic curve"
guessCurve (Just x) Nothing  = Right x
guessCurve Nothing  (Just y) = Right y
guessCurve (Just x) (Just y) = if x == y 
  then Right x 
  else Left $ "incompatible elliptic curves: " ++ quotedCurve x ++ " vs. " ++ quotedCurve y

--------------------------------------------------------------------------------
-- * Existential wrapper

data Some f 
  = forall c. PairingCurve c => MkSome !(f c)

withSome :: Some f -> (forall c. PairingCurve c => f c -> a) -> a
withSome some action = case some of
  MkSome x -> action x

--------------------------------------------------------------------------------
-- * Annotate with proxy

data WithProxy (f :: SomeCurve -> Type) (c :: SomeCurve) 
  = WithProxy !(Proxy c) !(f c)  

type SomeWithProxy f = Some (WithProxy f)

extractCurve :: Some (WithProxy f) -> SomeCurve
extractCurve some = case some of
  MkSome (WithProxy pxy _) -> lowerSomeCurve pxy

--------------------------------------------------------------------------------

-- -- maybe this should be somewhere else...
-- deriving instance Show (Some ProverInput)

--------------------------------------------------------------------------------
