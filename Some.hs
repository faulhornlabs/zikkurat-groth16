
{-# LANGUAGE ScopedTypeVariables, DataKinds, ExistentialQuantification, StandaloneDeriving #-}
module Some where

--------------------------------------------------------------------------------

import Data.Proxy

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import Types

--------------------------------------------------------------------------------

data Some f 
  = forall c. PairingCurve c => MkSome (f c)

withSome :: Some f -> (forall c. PairingCurve c => f c -> a) -> a
withSome some action = case some of
  MkSome x -> action x
  
--------------------------------------------------------------------------------

deriving instance Show (Some ProverInput)

--------------------------------------------------------------------------------
