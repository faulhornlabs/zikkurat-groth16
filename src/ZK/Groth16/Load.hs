
-- | Loading files (witness, prover key, circuit...)

{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeApplications, 
             ExistentialQuantification, StandaloneDeriving 
  #-}
module ZK.Groth16.Load where

--------------------------------------------------------------------------------

import Data.Proxy

import qualified ZK.Formats.Binary.ZKey     as Fmt
import qualified ZK.Formats.Binary.Witness  as Fmt
import qualified ZK.Formats.Primes          as Fmt

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import ZK.Algebra.Curves.BN128     ()      -- importing just the...
import ZK.Algebra.Curves.BLS12_381 ()      -- ...PairingCurve instances

import ZK.Groth16.Types
import ZK.Groth16.Aux.Glue 
import ZK.Groth16.Aux.Some

--------------------------------------------------------------------------------

data FilePaths = FilePaths
  { _zkeyFile :: FilePath
  , _wtnsFile :: FilePath
--  , _r1csFile :: Maybe FilePath
  }
  deriving Show

--------------------------------------------------------------------------------

someCurveProxy :: Fmt.Curve -> Maybe (Some Proxy)
someCurveProxy curve = case curve of
    Fmt.Bn128       -> Just (MkSome (Proxy @'BN128    ))
    Fmt.Bls12_381   -> Just (MkSome (Proxy @'BLS12_381))
    _               -> Nothing

loadStuff :: FilePaths -> IO (Some ProverInput)
loadStuff fpaths = do
  zkey <- Fmt.parseZKeyFile_ (_zkeyFile fpaths)
  wtns <- Fmt.parseWtnsFile_ (_wtnsFile fpaths)
  let curve = Fmt.zkeyCurve zkey
  return $ case someCurveProxy curve of
    Nothing      -> error $ "unsupported elliptic curve `" ++ show curve ++ "`"
    Just somePxy -> case somePxy of 
      MkSome pxy   -> MkSome $ ProverInput
        { _theProxy    = pxy
        , _theZKey     = convertZKey    pxy zkey 
        , _theWitness  = convertWitness pxy wtns
        }
 
--------------------------------------------------------------------------------
