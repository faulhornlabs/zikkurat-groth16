
{-# LANGUAGE BangPatterns #-}
module Main where -- Test where

--------------------------------------------------------------------------------

import System.FilePath
import System.Directory
import System.IO.Unsafe

import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.Pairing

import Types
import Prover
import Verifier
import Load
import Some

--------------------------------------------------------------------------------

homeDir :: FilePath
homeDir = unsafePerformIO getHomeDirectory

{-
fpaths :: FilePaths
fpaths = FilePaths
  { _zkeyFile = homeDir </> "zk/examples/circom/poseidon2/build/hash.zkey"
  , _wtnsFile = homeDir </> "zk/examples/circom/poseidon2/build/hash.wtns"
  }
-}

fpaths :: FilePaths
fpaths = FilePaths
  { _zkeyFile = homeDir </> "zk/examples/circom/product/build/product.zkey"
  , _wtnsFile = homeDir </> "zk/examples/circom/product/build/product.wtns"
  }

--------------------------------------------------------------------------------

runProver :: forall c. PairingCurve c => ProverInput c -> IO (Proof c)
runProver (ProverInput pxy zkey wtns) = prove pxy zkey wtns

runProverWithZeroMask :: forall c. PairingCurve c => ProverInput c -> IO (Proof c)
runProverWithZeroMask (ProverInput pxy zkey wtns) = return $ proveWithMask pxy zkey wtns (Mask 0 0)

main :: IO ()
main = do
  someInput <- loadStuff fpaths
  withSome someInput $ \proverInput@(ProverInput !pxy !zkey !wtns) -> do
    putStrLn "parsing ok"
    !proof <- runProver proverInput
    putStrLn "proof generated"

    let npub  = _npub (_zkeyHeader zkey) 
    let vkey  = extractVKey pxy zkey
    let pubIO = PublicIO (takeFlatArray npub $ dropFlatArray 1 $ fromWitness wtns)

    putStrLn $ "verification succeeded: " ++ show (verify pxy vkey proof pubIO)

