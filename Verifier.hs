
{-# LANGUAGE ScopedTypeVariables, TypeApplications, DeriveFunctor #-}
module Verifier where

--------------------------------------------------------------------------------

import Data.Kind
import Data.Proxy

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import Types

--------------------------------------------------------------------------------

verify :: PairingCurve c => Proxy c -> VKey c -> Proof c -> PublicIO c -> Bool
verify pxy (VKey spec vpoints alphaBeta) (Proof piA piB piC) (PublicIO pubIO) = ok where

  ok  = lhs == rhs

  lhs = pairing pxy piA piB
  rhs = pairing pxy piC (_delta2 spec)
      * pairing pxy ioP (_gamma2 spec)
      * alphaBeta 

  ioP = msm coeffs1 (_pointsIC vpoints)

  coeffs1 = vecCons 1 pubIO

--------------------------------------------------------------------------------
  