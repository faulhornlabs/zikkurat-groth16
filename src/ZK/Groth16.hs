
-- | Convenient re-export of the important modules

module ZK.Groth16 
  ( -- * Re-exported modules
    module ZK.Groth16.Prover
  , module ZK.Groth16.Verifier
  , module ZK.Groth16.Load
  , module ZK.Groth16.JSON 
  , module ZK.Groth16.Types  
  , module ZK.Groth16.Aux.Some
    -- * Library version
  , version
  , versionString
  )
  where

--------------------------------------------------------------------------------

import ZK.Groth16.Prover
import ZK.Groth16.Verifier
import ZK.Groth16.Load
import ZK.Groth16.JSON 
import ZK.Groth16.Types
import ZK.Groth16.Aux.Some

import Paths_zikkurat_groth16
import Data.Version

--------------------------------------------------------------------------------

versionString :: String
versionString = "zikkurat-groth16 v" ++ showVersion version

--------------------------------------------------------------------------------
