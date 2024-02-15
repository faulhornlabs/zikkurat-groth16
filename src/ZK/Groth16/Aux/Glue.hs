
-- | Glue between the binary formats (which does not depend on the algebra backend)
-- and the proper types

{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, StandaloneDeriving #-}
module ZK.Groth16.Aux.Glue where

--------------------------------------------------------------------------------

import Data.Kind
import Data.Proxy

import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

----------------------------------------

import qualified ZK.Formats.Types.ZKey      as Fmt
import qualified ZK.Formats.Types.Witness   as Fmt
import qualified ZK.Formats.Types.Etc       as Fmt
import qualified ZK.Formats.Primes          as Fmt
import           ZK.Formats.ForeignArray           

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import ZK.Groth16.Types

--------------------------------------------------------------------------------

convertFlavour :: Fmt.Flavour -> Flavour
convertFlavour what = case what of
  Fmt.JensGroth -> JensGroth  
  Fmt.Snarkjs   -> Snarkjs    

--------------------------------------------------------------------------------

convertCurve :: Fmt.Curve -> SomeCurve
convertCurve what = case what of
  Fmt.Bn128     -> BN128
  Fmt.Bls12_381 -> BLS12_381
  _             -> error $ "convertCurve: unhandled elliptic curve `" ++ show what ++ "`"

--------------------------------------------------------------------------------

convertWitness :: forall c. PairingCurve c => Proxy c -> Fmt.Witness -> Witness c
convertWitness pxy old 
  |                  Fmt._fieldPrime           cfg  /= fieldSizePxy (Proxy @(Fr c))  = error "convertWitness: different prime fields"
  | fromElementSize (Fmt._bytesPerFieldElement cfg) /= sizeInBytes  (Proxy @(Fr c))  = error "convertWitness: different number of bytes per field element"
  | otherwise  = Witness (fromStdFrArray pxy (Fmt._witnessData old))
  where
    cfg = Fmt._witnessFieldConfig old

fromStdFrArray :: forall c. PairingCurve c => Proxy c -> Fmt.StdFrArray -> FlatArray (Fr c)
fromStdFrArray pxy (Fmt.StdFrArray farr) = batchFromStandardRep (foreignArrayToFlatArray farr)

--------------------------------------------------------------------------------

convertGroth16Header :: Fmt.Groth16Header -> Groth16Header
convertGroth16Header old = MkGroth16Header
  { _curve         = convertCurve   (Fmt._curve   old)
  , _flavour       = convertFlavour (Fmt._flavour old)
  , _nvars         = Fmt._nvars         old
  , _npub          = Fmt._npub          old
  , _domainSize    = Fmt._domainSize    old
  , _logDomainSize = Log2 (Fmt._logDomainSize old)
  }

--------------------------------------------------------------------------------
 
convertZKey :: PairingCurve c => Proxy c -> Fmt.ZKey -> ZKey c
convertZKey pxy old = ZKey
  { _zkeyHeader  =      convertGroth16Header       (Fmt._zkeyHeader  old)
  , _zkeySpec    =      convertSpecPoints     pxy  (Fmt._zkeySpec    old)
  , _zkeyCoeffs  = map (convertZKeyCoeff      pxy) (Fmt._zkeyCoeffs  old)
  , _zkeyVPoints =      convertVerifierPoints pxy  (Fmt._zkeyVPoints old)
  , _zkeyPPoints =      convertProverPoints   pxy  (Fmt._zkeyPPoints old)
  }

--------------------------------------------------------------------------------

convertZKeyCoeff :: PairingCurve c => Proxy c -> Fmt.ZKeyCoeff -> ZKeyCoeff c
convertZKeyCoeff pxy old = ZKeyCoeff
  { _matrixSel = convertMatrixSel (Fmt._matrixSel old)
  , _row       = Fmt._row    old
  , _column    = Fmt._column old
  , _value     = fromInteger (Fmt.fromStdFr (Fmt._value old))
  }

convertMatrixSel :: Fmt.MatrixSel -> MatrixSel
convertMatrixSel old = case old of
  Fmt.MatrixA -> MatrixA  
  Fmt.MatrixB -> MatrixB  
  Fmt.MatrixC -> MatrixC 

--------------------------------------------------------------------------------

convertSpecPoints :: PairingCurve c => Proxy c -> Fmt.SpecPoints -> SpecPoints c
convertSpecPoints pxy old = SpecPoints 
  { _alpha1  = fromSingletonG1 pxy (Fmt._alpha1  old)
  , _beta1   = fromSingletonG1 pxy (Fmt._beta1   old)
  , _beta2   = fromSingletonG2 pxy (Fmt._beta2   old)
  , _gamma2  = fromSingletonG2 pxy (Fmt._gamma2  old)
  , _delta1  = fromSingletonG1 pxy (Fmt._delta1  old)
  , _delta2  = fromSingletonG2 pxy (Fmt._delta2  old)
  }

{-# NOINLINE fromSingletonG1 #-}
fromSingletonG1 :: PairingCurve c => Proxy c -> Fmt.SingletonG1 -> G1 c
fromSingletonG1 _pxy (Fmt.SingletonG1 (Fmt.G1Array farr)) = unsafePerformIO $ do
  x <- singletonForeignArrayToFlatIO farr
  convertInfinityIO x
  return x

{-# NOINLINE fromSingletonG2 #-}
fromSingletonG2 :: PairingCurve c => Proxy c -> Fmt.SingletonG2 -> G2 c
fromSingletonG2 _pxy (Fmt.SingletonG2 (Fmt.G2Array farr)) = unsafePerformIO $ do
  x <- singletonForeignArrayToFlatIO farr
  convertInfinityIO x
  return x
  
--------------------------------------------------------------------------------

convertProverPoints :: PairingCurve c => Proxy c -> Fmt.ProverPoints -> ProverPoints c
convertProverPoints pxy old = ProverPoints
  { _pointsA   = fromG1Array pxy (Fmt._pointsA  old)
  , _pointsB1  = fromG1Array pxy (Fmt._pointsB1 old)
  , _pointsB2  = fromG2Array pxy (Fmt._pointsB2 old)
  , _pointsC   = fromG1Array pxy (Fmt._pointsC  old)
  , _pointsH   = fromG1Array pxy (Fmt._pointsH  old)
  }

convertVerifierPoints :: PairingCurve c => Proxy c -> Fmt.VerifierPoints -> VerifierPoints c
convertVerifierPoints pxy old = VerifierPoints
  { _pointsIC  = fromG1Array pxy (Fmt._pointsIC old)
  }

{-# NOINLINE fromG1Array #-}
fromG1Array :: PairingCurve c => Proxy c -> Fmt.G1Array -> FlatArray (G1 c)
fromG1Array _pxy (Fmt.G1Array farr) = unsafePerformIO $ do
  let flatarr = foreignArrayToFlatArray farr
  batchConvertInfinityIO flatarr
  return flatarr

{-# NOINLINE fromG2Array #-}
fromG2Array :: PairingCurve c => Proxy c -> Fmt.G2Array -> FlatArray (G2 c)
fromG2Array _pxy (Fmt.G2Array farr) = unsafePerformIO $ do
  let flatarr = foreignArrayToFlatArray farr
  batchConvertInfinityIO flatarr
  return flatarr

--------------------------------------------------------------------------------

foreignArrayToFlatArray :: forall a. Flat a => ForeignArray -> FlatArray a
foreignArrayToFlatArray (ForeignArray len (ElementSize sz) fptr) 
  | sz == sizeInBytes (Proxy @a)  = MkFlatArray len (castForeignPtr fptr)
  | otherwise = error "foreignArrayToFlatArray: incompatible element sizes"

{-# NOINLINE singletonForeignArrayToFlatIO #-}
singletonForeignArrayToFlatIO :: forall a. Flat a => ForeignArray -> IO a
singletonForeignArrayToFlatIO (ForeignArray len (ElementSize sz) fptr) 
  | len /= 1                      = error "singletonForeignArrayToFlat: expecting an array of length 1"
  | sz  /= sizeInBytes (Proxy @a) = error "singletonForeignArrayToFlat: incompatible element sizes"
  | otherwise                     = withForeignPtr fptr $ \ptr -> makeFlat (castPtr ptr)

{-# NOINLINE singletonForeignArrayToFlat #-}
singletonForeignArrayToFlat :: forall a. Flat a => ForeignArray -> a
singletonForeignArrayToFlat farr = unsafePerformIO $ singletonForeignArrayToFlatIO farr


--------------------------------------------------------------------------------

