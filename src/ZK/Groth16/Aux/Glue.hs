
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
import qualified ZK.Formats.Types.R1CS      as Fmt
import qualified ZK.Formats.Types.Etc       as Fmt
import qualified ZK.Formats.Primes          as Fmt
import           ZK.Formats.ForeignArray           

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import ZK.Groth16.Types
import ZK.Groth16.Aux.Some

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
-- * Witness glue

convertWitness :: CurveHint -> Fmt.Witness -> SomeWithProxy Witness
convertWitness hint old = case hint of 
  Just curve -> newWitness curve
  Nothing    -> case recognizeScalarField (Fmt._fieldPrime (Fmt._witnessFieldConfig old)) of
    Just curve -> newWitness curve
    Nothing    -> error "convertWitness: unknown elliptic curve"
  where
    newWitness curve = case someCurveProxy curve of 
      MkSome pxy -> MkSome (WithProxy pxy (convertWitness' pxy old))

convertWitness' :: forall c. PairingCurve c => Proxy c -> Fmt.Witness -> Witness c
convertWitness' pxy old 
  |                  Fmt._fieldPrime           cfg  /= fieldSize    (Proxy @(Fr c))  = error "convertWitness: different prime fields"
  | fromElementSize (Fmt._bytesPerFieldElement cfg) /= sizeInBytes  (Proxy @(Fr c))  = error "convertWitness: different number of bytes per field element"
  | otherwise  = Witness (fromStdFrArray pxy (Fmt._witnessData old))
  where
    cfg = Fmt._witnessFieldConfig old

fromStdFrArray :: forall c. PairingCurve c => Proxy c -> Fmt.StdFrArray -> FlatArray (Fr c)
fromStdFrArray pxy (Fmt.StdFrArray farr) = batchFromStandardRep (foreignArrayToFlatArray farr)

--------------------------------------------------------------------------------
-- * R1CS glue

convertR1CS :: CurveHint -> Fmt.R1CS -> SomeWithProxy R1CS
convertR1CS = error "convertR1CS: not yet implemented"

--------------------------------------------------------------------------------
-- * VKey glue

-- | PLACEHOLDER!!
convertVKey :: CurveHint -> RawVKey -> SomeWithProxy VKey
convertVKey = error "convertVKey: not yet implemented"

--------------------------------------------------------------------------------
-- * ZKey glue

convertGroth16Header :: Fmt.Groth16Header -> Groth16Header
convertGroth16Header !old = MkGroth16Header
  { _curve         = convertCurve   (Fmt._curve   old)
  , _flavour       = convertFlavour (Fmt._flavour old)
  , _nvars         = Fmt._nvars         old
  , _npub          = Fmt._npub          old
  , _domainSize    = Fmt._domainSize    old
  , _logDomainSize = Log2 (Fmt._logDomainSize old)
  }

--------------------------------------------------------------------------------

convertZKey :: CurveHint -> Fmt.ZKey2 -> SomeWithProxy ZKey
convertZKey !hint !old = 
  case hint of 
    Just curve -> newZKey $ curve
    Nothing    -> newZKey $ convertCurve (Fmt._curve (Fmt._zkeyHeader old)) 
  where  
    newZKey curve = case someCurveProxy curve of 
      MkSome pxy -> MkSome (WithProxy pxy (convertZKey' pxy old))

convertZKey' :: forall c. PairingCurve c => Proxy c -> Fmt.ZKey2 -> ZKey c
convertZKey' pxy !old 
  | pxyCurve /= hdrCurve = error $ "convertZKey': incompatible elliptic curves: " ++ quotedCurve hdrCurve ++ " vs. " ++ quotedCurve pxyCurve
  | otherwise            = ZKey
    { _zkeyHeader  = newHeader 
    , _zkeySpec    = newSpec   
    , _zkeyMatrixA = matA
    , _zkeyMatrixB = matB
    , _zkeyVPoints = newVPoints
    , _zkeyPPoints = newPPoints
    }
  where
    pxyCurve = lowerSomeCurve pxy
    hdrCurve = _curve newHeader

    -- n = _domainSize newHeader
    -- m = _nvars      newHeader

    !newHeader    = convertGroth16Header      (Fmt._zkeyHeader  old)
    !newSpec      = convertSpecPoints     pxy (Fmt._zkeySpec    old)
    !newVPoints   = convertVerifierPoints pxy (Fmt._zkeyVPoints old)
    !newPPoints   = convertProverPoints   pxy (Fmt._zkeyPPoints old)
    (!matA,!matB) = convertZKeyMatrices   pxy (Fmt._zkeyCoeffs  old)

convertZKeyMatrices :: forall c. PairingCurve c => Proxy c -> (Fmt.SparseMatrix, Fmt.SparseMatrix) -> (SparseMatrix (Fr c), SparseMatrix (Fr c))
convertZKeyMatrices pxy (oldMatA, oldMatB) =
  ( convertZKeyMatrix pxy oldMatA
  , convertZKeyMatrix pxy oldMatB
  ) 
  
convertZKeyMatrix :: forall c. PairingCurve c => Proxy c -> Fmt.SparseMatrix -> SparseMatrix (Fr c)
convertZKeyMatrix pxy (Fmt.SparseMatrix dims rowIdxs colIdxs values) = 
  MkSparseMatrix
    {  _sparseDims   = dims
    , _sparseRowIdxs = fromIntArray              rowIdxs
    , _sparseColIdxs = fromIntArray              colIdxs
    , _sparseCoeffs  = fromDoubleMontFrArray pxy values
    }

--------------------------------------------------------------------------------

{-
-- converting from list fof coeffs; 
-- this is slow because the zikkurat-formats-binary lib has very slow conversion

data Entry a = Entry 
  { _entryRow :: !Int 
  , _entryCol :: !Int
  , _entryVal :: !a
  }
  deriving Show

convertZKeyCoeffs :: forall c. PairingCurve c => Proxy c -> (Int,Int) -> [Fmt.ZKeyCoeff] -> (SparseMatrix (Fr c), SparseMatrix (Fr c))
convertZKeyCoeffs pxy nm coeffs = (matA, matB) where
  (listA, listB) = splitCoeffs coeffs
  !matA = packMatrix listA
  !matB = packMatrix listB

  packMatrix :: [Entry (Fr c)] -> SparseMatrix (Fr c)
  packMatrix entries = MkSparseMatrix nm rowIdxs colIdxs coeffVec where
    k = length entries
    rowIdxs  = packFlatArrayFromList' k (map _entryRow entries)
    colIdxs  = packFlatArrayFromList' k (map _entryCol entries)
    coeffVec = packFlatArrayFromList' k (map _entryVal entries)

  splitCoeffs :: [Fmt.ZKeyCoeff] -> ( [Entry (Fr c)], [Entry (Fr c)] )
  splitCoeffs = go [] [] where
    go as bs []          = (as,bs)
    go as bs (this:rest) = 
      case Fmt._matrixSel this of
        Fmt.MatrixA -> go (new:as) bs rest
        Fmt.MatrixB -> go as (new:bs) rest
        Fmt.MatrixC -> error "convertZKeyCoeffs: matrix C selector; this should not happen"
      where
        new = Entry 
          { _entryRow = Fmt._row    this
          , _entryCol = Fmt._column this
          , _entryVal = fromInteger (Fmt.fromStdFr (Fmt._value this))
          }
-}

--------------------------------------------------------------------------------
 
{-
-- legacy version 
-- this was _extremely slow_ 

convertZKey' :: forall c. PairingCurve c => Proxy c -> Fmt.ZKey -> ZKey c
convertZKey' pxy !old 
  | pxyCurve /= hdrCurve = error $ "convertZKey': incompatible elliptic curves: " ++ quotedCurve hdrCurve ++ " vs. " ++ quotedCurve pxyCurve
  | otherwise            = ZKey
    { _zkeyHeader  = newHeader 
    , _zkeySpec    = newSpec   
    , _zkeyCoeffs  = newCoeffs 
    , _zkeyVPoints = newVPoints
    , _zkeyPPoints = newPPoints
    }
  where
    pxyCurve = lowerSomeCurve pxy
    hdrCurve = _curve newHeader

    !newHeader  =            convertGroth16Header       (Fmt._zkeyHeader  old)
    !newSpec    =            convertSpecPoints     pxy  (Fmt._zkeySpec    old)
    !newCoeffs  = strictMap (convertZKeyCoeff      pxy) (Fmt._zkeyCoeffs  old)
    !newVPoints =            convertVerifierPoints pxy  (Fmt._zkeyVPoints old)
    !newPPoints =            convertProverPoints   pxy  (Fmt._zkeyPPoints old)
   
strictMap :: (a -> b) -> [a] -> [b]
strictMap f = go where 
  go (x:xs) = let !y = f x in y `seq` (y : go xs)
  go []     = []

convertZKeyCoeff :: PairingCurve c => Proxy c -> Fmt.ZKeyCoeff -> ZKeyCoeff c
convertZKeyCoeff pxy !old = ZKeyCoeff
  { _matrixSel = convertMatrixSel (Fmt._matrixSel old)
  , _row       = Fmt._row    old
  , _column    = Fmt._column old
  , _value     = fromInteger (Fmt.fromStdFr (Fmt._value old))
  }

convertMatrixSel :: Fmt.MatrixSel -> MatrixSel
convertMatrixSel !old = case old of
  Fmt.MatrixA -> MatrixA  
  Fmt.MatrixB -> MatrixB  
  Fmt.MatrixC -> MatrixC 
-}

--------------------------------------------------------------------------------

convertSpecPoints :: PairingCurve c => Proxy c -> Fmt.SpecPoints -> SpecPoints c
convertSpecPoints pxy !old = SpecPoints 
  { _alpha1  = fromSingletonG1 pxy (Fmt._alpha1  old)
  , _beta1   = fromSingletonG1 pxy (Fmt._beta1   old)
  , _beta2   = fromSingletonG2 pxy (Fmt._beta2   old)
  , _gamma2  = fromSingletonG2 pxy (Fmt._gamma2  old)
  , _delta1  = fromSingletonG1 pxy (Fmt._delta1  old)
  , _delta2  = fromSingletonG2 pxy (Fmt._delta2  old)
  }
  
convertProverPoints :: PairingCurve c => Proxy c -> Fmt.ProverPoints -> ProverPoints c
convertProverPoints pxy !old = ProverPoints
  { _pointsA   = fromG1Array pxy (Fmt._pointsA  old)
  , _pointsB1  = fromG1Array pxy (Fmt._pointsB1 old)
  , _pointsB2  = fromG2Array pxy (Fmt._pointsB2 old)
  , _pointsC   = fromG1Array pxy (Fmt._pointsC  old)
  , _pointsH   = fromG1Array pxy (Fmt._pointsH  old)
  }

convertVerifierPoints :: PairingCurve c => Proxy c -> Fmt.VerifierPoints -> VerifierPoints c
convertVerifierPoints pxy !old = VerifierPoints
  { _pointsIC  = fromG1Array pxy (Fmt._pointsIC old)
  }

--------------------------------------------------------------------------------
-- * Helpers

fromG1Array :: PairingCurve c => Proxy c -> Fmt.G1Array -> FlatArray (G1 c)
fromG1Array _pxy (Fmt.G1Array farr) = foreignArrayToFlatArray farr

fromG2Array :: PairingCurve c => Proxy c -> Fmt.G2Array -> FlatArray (G2 c)
fromG2Array _pxy (Fmt.G2Array farr) = foreignArrayToFlatArray farr

fromSingletonG1 :: PairingCurve c => Proxy c -> Fmt.SingletonG1 -> G1 c
fromSingletonG1 _pxy (Fmt.SingletonG1 (Fmt.G1Array farr)) = peekFlatArray (foreignArrayToFlatArray farr) 0

fromSingletonG2 :: PairingCurve c => Proxy c -> Fmt.SingletonG2 -> G2 c
fromSingletonG2 _pxy (Fmt.SingletonG2 (Fmt.G2Array farr)) = peekFlatArray (foreignArrayToFlatArray farr) 0

toG1Array :: PairingCurve c => Proxy c -> FlatArray (G1 c) -> Fmt.G1Array
toG1Array pxy = Fmt.G1Array . flatArrayToForeignArray 

toG2Array :: PairingCurve c => Proxy c -> FlatArray (G2 c) -> Fmt.G2Array
toG2Array pxy = Fmt.G2Array . flatArrayToForeignArray 

toSingletonG1 :: PairingCurve c => Proxy c -> G1 c -> Fmt.SingletonG1
toSingletonG1 pxy = Fmt.SingletonG1 . Fmt.G1Array . singletonFlatToForeignArray

toSingletonG2 :: PairingCurve c => Proxy c -> G2 c -> Fmt.SingletonG2
toSingletonG2 pxy = Fmt.SingletonG2 . Fmt.G2Array . singletonFlatToForeignArray

{-
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
-}

--------------------------------------------------------------------------------

fromIntArray :: Fmt.IntArray -> FlatArray Int
fromIntArray (Fmt.IntArray arr) = (foreignArrayToFlatArray arr)

-- hackety hack hack 
fromDoubleMontFrArray :: forall c. PairingCurve c => Proxy c -> Fmt.DoubleMontFrArray -> FlatArray (Fr c)
fromDoubleMontFrArray pxy (Fmt.DoubleMontFrArray arr) = unsafePerformIO $ do
  let flatarr  = foreignArrayToFlatArray arr  :: FlatArray (Fr    c)
  let flatarr' = batchToStandardRep  flatarr  :: FlatArray (StdFr c)
  return (unsafeCastFlatArray flatarr')

--------------------------------------------------------------------------------

foreignArrayToFlatArray :: forall a. Flat a => ForeignArray -> FlatArray a
foreignArrayToFlatArray (ForeignArray len (ElementSize sz) fptr) 
  | sz == sizeInBytes (Proxy @a)  = MkFlatArray len (castForeignPtr fptr)
  | otherwise = error "foreignArrayToFlatArray: incompatible element sizes"

flatArrayToForeignArray :: forall a. Flat a => FlatArray a -> ForeignArray 
flatArrayToForeignArray (MkFlatArray len fptr) = ForeignArray len (ElementSize sz) (castForeignPtr fptr) where
  sz = sizeInBytes (Proxy @a)

singletonFlatToForeignArray :: forall a. Flat a => a -> ForeignArray 
singletonFlatToForeignArray x = flatArrayToForeignArray (singletonArray x)

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

