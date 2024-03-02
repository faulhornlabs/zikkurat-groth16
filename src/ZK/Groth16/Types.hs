
-- | Type definitions

{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, StandaloneDeriving #-}
module ZK.Groth16.Types where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import Data.Proxy

import ZK.Algebra.Class.Field  
import ZK.Algebra.Class.Curve  
import ZK.Algebra.Class.Pairing 
import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.Vector
import ZK.Algebra.Class.Misc

--------------------------------------------------------------------------------
-- * Config

-- | Which version of the Groth16 setup we use
data Flavour 
  = JensGroth    -- ^ the version described in the original Groth16 paper 
  | Snarkjs      -- ^ the version implemented by Snarkjs
  deriving (Eq,Show)

data Verbosity 
  = Silent
  | Normal
  | Verbose
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- * Proof

data Proof (c :: SomeCurve) = Proof 
  { _piA :: !(G1 c)
  , _piB :: !(G2 c)
  , _piC :: !(G1 c)
  }

-- | The public IO (excluding the special variable 1)
newtype PublicIO (c :: SomeCurve) 
  = PublicIO { fromPublicIO :: FlatArray (Fr c) }

deriving instance PairingCurve c => Show (Proof    c)
deriving instance PairingCurve c => Show (PublicIO c)

--------------------------------------------------------------------------------

{-
-- * Prover input

data ProverInput (c :: SomeCurve) = ProverInput
  { _theProxy   :: Proxy c
  , _theZKey    :: ZKey c
  , _theWitness :: Witness c
  }

deriving instance PairingCurve c => Show (ProverInput c)
-}

--------------------------------------------------------------------------------
-- * Witness

newtype Witness (c :: SomeCurve) 
  = Witness { fromWitness :: FlatArray (Fr c) }

deriving instance PairingCurve c => Show (Witness c)

--------------------------------------------------------------------------------
-- * R1CS

data WitnessConfig = MkWitnessConfig
  { _wtnsCurve  :: !SomeCurve     -- ^ which elliptic curve we are using
  , _wtnsNVars  :: !Int           -- ^ total number of witness variables (or wires), including the special \"variable\" constant 1.
  , _wtnsNPubIO :: !Int           -- ^ number of public inputs and outputs (excluding the constant 1 variable)
  }
  deriving Show

data R1CS (c :: SomeCurve) = MkR1CS
  { _witnessCfg   :: !WitnessConfig         -- ^ witness configuration (public and private inputs)
  , _constraints  :: [Constraint c]         -- ^ the list of R1CS constraints
  }

-- | An R1CS constraints has the form @A * B = C@ where A,B,C are (affine) linear terms
data Constraint (c :: SomeCurve) = MkConstraint
  { _constraintA :: !(LinComb c)
  , _constraintB :: !(LinComb c)
  , _constraintC :: !(LinComb c)
  } 

newtype LinComb (c :: SomeCurve)
  = MkLinComb [(Int,Fr c)]           -- ^ list of terms as @(index, coeff)@ pairs. Index 0 is the special variable \1\".

deriving instance PairingCurve c => Show (R1CS       c)
deriving instance PairingCurve c => Show (Constraint c)
deriving instance PairingCurve c => Show (LinComb    c)

--------------------------------------------------------------------------------
-- * VKey (verifier key)

-- | TMP PLACEHOLDER!
data RawVKey = RawVKey

-- | Verification key
data VKey (c :: SomeCurve) = VKey
  { _vkeySpec      :: !(SpecPoints c)
  , _vkeyVPoints   :: !(VerifierPoints c)
  , _vkeyAlphaBeta :: !(Fp12 c)
  }

--------------------------------------------------------------------------------
-- * ZKey (prover key)

-- | Prover key
data ZKey (c :: SomeCurve) = ZKey
  { _zkeyHeader  :: !Groth16Header
  , _zkeySpec    :: !(SpecPoints       c )
  , _zkeyMatrixA :: !(SparseMatrix (Fr c))
  , _zkeyMatrixB :: !(SparseMatrix (Fr c))
  , _zkeyVPoints :: !(VerifierPoints   c ) 
  , _zkeyPPoints :: !(ProverPoints     c )
  }
 
extractVKey :: PairingCurve c => Proxy c -> ZKey c -> VKey c
extractVKey pxy zkey = VKey
  { _vkeySpec      = spec
  , _vkeyVPoints   = _zkeyVPoints zkey
  , _vkeyAlphaBeta = pairing pxy (_alpha1 spec) (_beta2 spec)
  }
  where 
    spec = _zkeySpec zkey

deriving instance PairingCurve c => Show (VKey c)
deriving instance PairingCurve c => Show (ZKey c)

--------------------------------------------------------------------------------

data Groth16Header = MkGroth16Header
  { _curve         :: !SomeCurve      -- ^ which elliptic curve we use
  , _flavour       :: !Flavour        -- ^ which version of the Groth16 setup we use
  , _nvars         :: !Int            -- ^ number of witness variables (including the constant 1 "variable")
  , _npub          :: !Int            -- ^ number of public input/output variables (excluding the constant 1)
  , _domainSize    :: !Int            -- ^ size of the FFT domain (a power of two)
  , _logDomainSize :: !Log2 
  }
  deriving Show

--------------------------------------------------------------------------------

-- | Special points
data SpecPoints (c :: SomeCurve) = SpecPoints 
  { _alpha1  :: !(G1 c)           -- ^ @[alpha]_1@
  , _beta1   :: !(G1 c)           -- ^ @[beta]_1@
  , _beta2   :: !(G2 c)           -- ^ @[beta]_2@
  , _gamma2  :: !(G2 c)           -- ^ @[gamma]_2@
  , _delta1  :: !(G1 c)           -- ^ @[delta]_1@
  , _delta2  :: !(G2 c)           -- ^ @[delta]_2@
  }

-- | Prover points (most of the prover key, excluding 'SpecPoints')
data ProverPoints (c :: SomeCurve) = ProverPoints
  { _pointsA  :: !(FlatArray (G1 c))      -- ^ the curve points @[A_j(tau)]_1 in G1@
  , _pointsB1 :: !(FlatArray (G1 c))      -- ^ the curve points @[B_j(tau)]_1 in G1@
  , _pointsB2 :: !(FlatArray (G2 c))      -- ^ the curve points @[B_j(tau)]_2 in G2@
  , _pointsC  :: !(FlatArray (G1 c))      -- ^ the curve points @[delta^-1 * ( beta*A_j(tau) + alpha*B_j(tau) + C_j(tau) )]_1@ in G1
  , _pointsH  :: !(FlatArray (G1 c))      -- ^ the curve points @[delta^-1 * L_{2i+1}(tau)]_1@ in G1
  }

-- | Verifier points (corresponds to the public IO section of the circuit)
data VerifierPoints (c :: SomeCurve) = VerifierPoints
  { _pointsIC   :: FlatArray (G1 c)      -- ^ the curve points corresponding to public IO: @[gamma^-1 * ( beta*A_j(tau) + alpha*B_j(tau) + C_j(tau) )]_1@ 
  }

deriving instance PairingCurve c => Show (SpecPoints     c)
deriving instance PairingCurve c => Show (ProverPoints   c)
deriving instance PairingCurve c => Show (VerifierPoints c)

--------------------------------------------------------------------------------

-- | Which matrix (from the R1CS equation (Az)*(Bz) = Cz)
data MatrixSel
  = MatrixA  
  | MatrixB  
  | MatrixC 
  deriving (Eq,Show)

data ZKeyCoeff (c :: SomeCurve) = ZKeyCoeff  
  { _matrixSel :: !MatrixSel 
  , _row       :: !Int 
  , _column    :: !Int 
  , _value     :: !(Fr c)
  }

deriving instance PairingCurve c => Show (ZKeyCoeff c)

--------------------------------------------------------------------------------
