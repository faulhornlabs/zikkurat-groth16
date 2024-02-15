
-- | Existential wrapper

{-# LANGUAGE ScopedTypeVariables, DataKinds, ExistentialQuantification, 
             Rank2Types, StandaloneDeriving, FlexibleInstances 
  #-}
module ZK.Groth16.Aux.Some where

--------------------------------------------------------------------------------

import Data.Proxy

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import ZK.Groth16.Types

--------------------------------------------------------------------------------

data Some f 
  = forall c. PairingCurve c => MkSome (f c)

withSome :: Some f -> (forall c. PairingCurve c => f c -> a) -> a
withSome some action = case some of
  MkSome x -> action x
  
--------------------------------------------------------------------------------

deriving instance Show (Some ProverInput)

--------------------------------------------------------------------------------
