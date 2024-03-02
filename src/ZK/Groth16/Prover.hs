
-- | The Groth16 prover

{-# LANGUAGE ScopedTypeVariables, TypeApplications, DeriveFunctor #-}
module ZK.Groth16.Prover where

--------------------------------------------------------------------------------

import Data.Kind
import Data.Proxy

import Data.List
import Data.Functor
import Data.Functor.Identity

import qualified Data.IntMap.Strict as IntMap; import Data.IntMap.Strict (IntMap)

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import ZK.Groth16.Types
import ZK.Groth16.Aux.Time

--------------------------------------------------------------------------------

data ABC a = ABC
  { _Az :: !a
  , _Bz :: !a
  , _Cz :: !a
  }
  deriving (Show,Functor)

data Mask a = Mask
  { _maskR :: !a
  , _maskS :: !a
  }

rndMask :: Rnd a => IO (Mask a)
rndMask = do
  r <- rndIO
  s <- rndIO
  return (Mask r s)

--------------------------------------------------------------------------------
-- * Build the vectors Az, Bz, Cz

type Column c = FlatArray (Fr c)

buildABC :: forall c. PairingCurve c => Proxy c -> ZKey c -> Witness c -> ABC (Column c)
buildABC pxy zkey (Witness zvec) = abc where

  header = _zkeyHeader  zkey
  m = _nvars      header
  n = _domainSize header 

  abc = ABC vecA vecB vecC

  !vecA = sparseMatrixMul (_zkeyMatrixA zkey) zvec
  !vecB = sparseMatrixMul (_zkeyMatrixB zkey) zvec
  !vecC = pwMul vecA vecB

----------------------------------------

{-
-- legacy version
buildABC :: forall c. PairingCurve c => Proxy c -> ZKey c -> Witness c -> ABC (Column c)
buildABC pxy zkey (Witness zvec) = abc where
  
  header = _zkeyHeader  zkey
  coeffs = _zkeyCoeffs  zkey  
  m = _nvars      header
  n = _domainSize header

  vecA = fromSparseVec n tableA 
  vecB = fromSparseVec n tableB
  vecC = pwMul vecA vecB

  abc = ABC vecA vecB vecC

  (tableA,tableB) = foldl' f (IntMap.empty, IntMap.empty) coeffs

  f :: (IntMap (Fr c), IntMap (Fr c)) -> ZKeyCoeff c -> (IntMap (Fr c), IntMap (Fr c))
  f (!oldA,!oldB) (ZKeyCoeff !which !row !col !value) = case which of
    MatrixA -> let newA = IntMap.insertWith (+) row (value * wtns col) oldA in (newA,oldB) 
    MatrixB -> let newB = IntMap.insertWith (+) row (value * wtns col) oldB in (oldA,newB) 

  wtns :: Int -> Fr c
  wtns !i = peekFlatArray zvec i

fromSparseVec :: (Flat a, Num a) => Int -> IntMap a -> FlatArray a 
fromSparseVec n table = packFlatArrayFromList' n [ f i | i<-[0..n-1] ] where
  f !i = IntMap.findWithDefault (fromInteger 0) i table
-}

--------------------------------------------------------------------------------
-- * Shift evaluation domains

-- | Computes @[1,eta,eta^2,...eta^{N-1}]@
powersOfEta :: forall c. PairingCurve c => Proxy c -> Int -> Fr c -> FlatArray (Fr c)
powersOfEta pxy n eta = powers 1 eta n

{-
shiftEvalDomain :: forall c. PairingCurve c => Proxy c -> FFTDomain (Fr c) -> Fr c -> FlatArray (Fr c) -> FlatArray (Fr c)
shiftEvalDomain pxy domain eta arr = runIdentity $ shiftEvalDomains pxy domain eta (Identity arr) 

shiftEvalDomains :: forall f c. (Functor f, PairingCurve c) => Proxy c -> FFTDomain (Fr c) -> Fr c -> f (FlatArray (Fr c)) -> f (FlatArray (Fr c))
shiftEvalDomains pxy domain eta arrs = fmap f arrs where
  n    = fftDomainSize domain
  pows = powersOfEta pxy n eta
  f arr = arr' where
    poly    = intt @(Poly c) domain arr
    coeffs' = pwMul (coeffsFlatArr poly) pows 
    poly'   = mkPolyFlat coeffs'
    arr'    = ntt @(Poly c) domain poly'
-}

shiftEvalDomain :: forall c. PairingCurve c => Proxy c -> FFTDomain (Fr c) -> Fr c -> FlatArray (Fr c) -> FlatArray (Fr c)
shiftEvalDomain pxy domain eta arr 
  =         ntt @(Poly c) domain  
  $ shiftedINTT @(Poly c) domain eta 
  $ arr

shiftEvalDomains :: forall f c. (Functor f, PairingCurve c) => Proxy c -> FFTDomain (Fr c) -> Fr c -> f (FlatArray (Fr c)) -> f (FlatArray (Fr c))
shiftEvalDomains pxy domain eta = fmap (shiftEvalDomain pxy domain eta) 

--------------------------------------------------------------------------------

{-
powersOfEtaNaive :: forall a. Field a => Int -> a -> FlatArray a
powersOfEtaNaive n eta = packFlatArrayFromList' n (go n 1) where
  go :: Int -> a -> [a]
  go 0 _ = []
  go k x = x : go (k-1) (eta*x)

pointwiseProductNaive :: Field a => FlatArray a -> FlatArray a -> FlatArray a 
pointwiseProductNaive xs ys 
  = packFlatArrayFromList 
  $ zipWith (*) (unpackFlatArrayToList xs) (unpackFlatArrayToList ys)

shiftEvalDomainsNaive :: forall f p a. (Functor f, Field a, UnivariateFFT p, Coeff p ~ a) => Proxy p -> FFTDomain a -> a -> f (FlatArray a) -> f (FlatArray a)
shiftEvalDomainsNaive pxy domain eta arrs = fmap f arrs where
  n    = fftDomainSize domain
  pows = powersOfEtaNaive n eta
  f arr = arr' where
    poly    = intt @p domain arr
    coeffs' = pointwiseProductNaive (coeffsFlatArr poly) pows 
    poly'   = mkPolyFlat coeffs'
    arr'    = ntt @p domain poly'
-}

--------------------------------------------------------------------------------
-- * Compute the QS coefficients

-- | Computes the quotient polynomial Q = (A*B - C) / Z
-- by computing the values on a shifted domain, and interpolating the result
-- remark: Q has degree `n-2`, so it's enough to use a domain of size n
computeQuotientPointwise :: forall c. PairingCurve c => Proxy c -> ABC (Column c) -> Poly c
computeQuotientPointwise pxy abc@(ABC az bz cz) 
  | n /= n2 || n /= n3 || n2 /= n3  = error "computeQuotientPointwise: fatal error, shouldn't happen"
  | n /= exp2_ m                    = error "computeQuotientPointwise: non-power-of-two column vectors"               
  | otherwise                       = wrapArray cs
  where
    n  = flatArrayLength az
    n2 = flatArrayLength bz
    n3 = flatArrayLength cz

    ni = fromIntegral n :: Integer
    m  = integerLog2 ni

    dom2  = getFFTDomain (m + 1)
    dom   = halveSubgroup  dom2
    eta   = fftSubgroupGen dom2
    etaN  = power eta ni
    invZ1 = inverse (etaN - 1)       -- note:  1 / [ (eta*omega^j)^n - 1 ]  =  1/(eta^n - 1)

    ABC az1 bz1 cz1 = shiftEvalDomains pxy dom eta abc
    ys = vecScale invZ1 (pwMulSub az1 bz1 cz1) :: Column c
    q1 = intt @(Poly c) dom ys

    cs = mulByPowers 1 (inverse eta) (unwrapArray q1 :: Column c)
    -- pows = powersOfEta pxy n (inverse eta) 
    -- cs   = pwMul pows (unwrapArray q1 :: Column c)

----------------------------------------

-- | Snarkjs does something different, not actually computing the quotient poly.
-- They can get away with this, because during the trusted setup, they
-- replace the points encoding the values `delta^-1 * tau^i * Z(tau)` by 
-- (shifted) Lagrange bases.
-- see eg. <https://geometry.xyz/notebook/the-hidden-little-secret-in-snarkjs>
--
computeSnarkjsScalarCoeffs :: forall c. PairingCurve c => Proxy c -> ABC (Column c) -> FlatArray (Fr c)
computeSnarkjsScalarCoeffs pxy abc@(ABC az bz cz) 
  | n /= n2 || n /= n3 || n2 /= n3  = error "computeSnarkjsScalarCoeffs: fatal error, shouldn't happen"
  | n /= exp2_ m                    = error "computeSnarkjsScalarCoeffs: non-power-of-two column vectors"               
  | otherwise                       = wrapArray ys
  where
    n  = flatArrayLength az
    n2 = flatArrayLength bz
    n3 = flatArrayLength cz
    m  = integerLog2 (fromIntegral n :: Integer)

    dom2  = getFFTDomain (m + 1)
    dom   = halveSubgroup  dom2
    eta   = fftSubgroupGen dom2

    ABC az1 bz1 cz1 = shiftEvalDomains pxy dom eta abc
    ys = pwMulSub az1 bz1 cz1

--------------------------------------------------------------------------------
-- * Main prover

computeScalarCoeffs :: PairingCurve c => Proxy c -> ABC (Column c) -> Flavour -> FlatArray (Fr c)
computeScalarCoeffs pxy abc flavour = case flavour of
  JensGroth -> unwrapArray (computeQuotientPointwise   pxy abc)
  Snarkjs   ->              computeSnarkjsScalarCoeffs pxy abc

prove :: forall c. PairingCurve c => Proxy c -> ZKey c -> Witness c -> IO (Proof c)
prove pxy zkey witness = do
  mask <- rndMask :: IO (Mask (Fr c))
  return $ proveWithMask pxy zkey witness mask

proveWithMask :: forall c. PairingCurve c => Proxy c -> ZKey c -> Witness c -> Mask (Fr c) -> Proof c
proveWithMask pxy zkey witness@(Witness zs) (Mask r s) = Proof piA piB piC where

  header = _zkeyHeader  zkey
  spec   = _zkeySpec    zkey
  vpts   = _zkeyVPoints zkey
  ppts   = _zkeyPPoints zkey

  m = _nvars      header
  n = _domainSize header

  priv = msm (dropFlatArray (_npub header + 1) zs) (_pointsC ppts)

  abc = buildABC pxy zkey witness
  qs  = computeScalarCoeffs pxy abc (_flavour header)
  piA = _alpha1 spec <+> msm zs (_pointsA  ppts) <+> (r <**> _delta1 spec)
  rho = _beta1  spec <+> msm zs (_pointsB1 ppts) <+> (s <**> _delta1 spec)
  piB = _beta2  spec <+> msm zs (_pointsB2 ppts) <+> (s <**> _delta2 spec)
  piC = priv <+> msm qs (_pointsH ppts) <+> (s <**> piA) <+> (r <**> rho) <-> ((r*s) <**> _delta1 spec)

--------------------------------------------------------------------------------
-- * IO version

data Masking 
  = TrueZK 
  | ZeroMask 
  deriving (Eq,Show)

proveIO :: forall c. PairingCurve c => Proxy c -> Bool -> Masking -> ZKey c -> Witness c -> IO (Proof c)
proveIO pxy detailsFlag masking zkey wtns = do
  mask <- case masking of
    ZeroMask -> return (Mask 0 0)
    TrueZK   -> rndMask 
  proveWithMaskIO pxy detailsFlag zkey wtns mask

proveWithMaskIO :: forall c. PairingCurve c => Proxy c -> Bool -> ZKey c -> Witness c -> Mask (Fr c) -> IO (Proof c)
proveWithMaskIO pxy detailsFlag zkey witness@(Witness zs) (Mask r s) = do

  let header = _zkeyHeader  zkey
  let spec   = _zkeySpec    zkey
  let vpts   = _zkeyVPoints zkey
  let ppts   = _zkeyPPoints zkey

  let m = _nvars      header
  let n = _domainSize header

  let priv = msm (dropFlatArray (_npub header + 1) zs) (_pointsC ppts)

  abc <- printMeasureTime detailsFlag "building ABC       " $ return $ buildABC pxy zkey witness
  qs  <- printMeasureTime detailsFlag "computing quotient " $ return $ computeScalarCoeffs pxy abc (_flavour header)
  piA <- printMeasureTime detailsFlag "calculating piA    " $ return $ _alpha1 spec <+> msm zs (_pointsA  ppts) <+> (r <**> _delta1 spec)
  rho <- printMeasureTime detailsFlag "calculating rho    " $ return $ _beta1  spec <+> msm zs (_pointsB1 ppts) <+> (s <**> _delta1 spec)
  piB <- printMeasureTime detailsFlag "calculating piB    " $ return $ _beta2  spec <+> msm zs (_pointsB2 ppts) <+> (s <**> _delta2 spec)
  piC <- printMeasureTime detailsFlag "calculating piC    " $ return $ priv <+> msm qs (_pointsH ppts) <+> (s <**> piA) <+> (r <**> rho) <-> ((r*s) <**> _delta1 spec)

{-
  let abc = buildABC pxy zkey witness
  let qs  = computeScalarCoeffs pxy abc (_flavour header)
  let piA = _alpha1 spec <+> msm zs (_pointsA  ppts) <+> (r <**> _delta1 spec)
  let rho = _beta1  spec <+> msm zs (_pointsB1 ppts) <+> (s <**> _delta1 spec)
  let piB = _beta2  spec <+> msm zs (_pointsB2 ppts) <+> (s <**> _delta2 spec)
  let piC = priv <+> msm qs (_pointsH ppts) <+> (s <**> piA) <+> (r <**> rho) <-> ((r*s) <**> _delta1 spec)
-}

  return (Proof piA piB piC)

--------------------------------------------------------------------------------

extractPublicIO :: forall c. PairingCurve c => Proxy c -> ZKey c -> Witness c -> PublicIO c
extractPublicIO pxy zkey (Witness zs) 
  = PublicIO 
  $ dropFlatArray 1 
  $ takeFlatArray (_npub header + 1) zs
  where
    header = _zkeyHeader  zkey

--------------------------------------------------------------------------------

