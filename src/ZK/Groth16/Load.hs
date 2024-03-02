
-- | Loading files (witness, prover key, circuit...)

{-# LANGUAGE ScopedTypeVariables, TypeApplications,
             DataKinds, KindSignatures,  
             ExistentialQuantification, 
             StandaloneDeriving, DeriveFunctor,
             RecordWildCards
  #-}
module ZK.Groth16.Load where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Maybe
import Data.List

import Unsafe.Coerce

import qualified ZK.Formats.Binary.ZKey     as Fmt
import qualified ZK.Formats.Binary.Witness  as Fmt
import qualified ZK.Formats.Binary.R1CS     as Fmt
import qualified ZK.Formats.Primes          as Fmt

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing 

import ZK.Groth16.Types
import ZK.Groth16.Aux.Glue 
import ZK.Groth16.Aux.Some
import ZK.Groth16.Aux.Time
import ZK.Groth16.JSON        as Json

--------------------------------------------------------------------------------
-- * Input \/ Output

data Direction a
  = Input  !a
  | Output !a
  | None
  deriving (Eq,Show,Functor)

isInput, isOutput :: Direction a -> Bool
isInput  d = isJust (fromInput  d)
isOutput d = isJust (fromOutput d)

fromInput :: Direction a -> Maybe a
fromInput (Input x) = Just x
fromInput _         = Nothing

fromOutput :: Direction a -> Maybe a
fromOutput (Output y) = Just y
fromOutput _          = Nothing


{-
replaceDirection :: Direction a -> b -> Direction b
replaceDirection old y = fmap (const y) old
-}

--------------------------------------------------------------------------------

data InOutFiles = InOutFiles
  { _fileZkey      :: !(Direction FilePath)     
  , _fileVkey      :: !(Direction FilePath)     
  , _fileWtns      :: !(Direction FilePath)     
  , _fileR1CS      :: !(Direction FilePath)     
  , _fileProofJson :: !(Direction FilePath)
  , _filePubJson   :: !(Direction FilePath)  
  }
  deriving Show

inputFiles :: InOutFiles -> [FilePath]
inputFiles (InOutFiles{..}) = catMaybes
  [ fromInput _fileZkey     
  , fromInput _fileVkey     
  , fromInput _fileWtns     
  , fromInput _fileR1CS     
  , fromInput _fileProofJson
  , fromInput _filePubJson  
  ]

data RawInOutData = RawInOutData 
  { _rawZkey  :: !(Direction  Fmt.ZKey2   )
  , _rawVkey  :: !(Direction  RawVKey     )
  , _rawWtns  :: !(Direction  Fmt.Witness )
  , _rawR1CS  :: !(Direction  Fmt.R1CS    )
  , _rawProof :: !(Direction Json.RawProof)
  , _rawPubIO :: !(Direction Json.RawPubIO)
  }

data SomeInOutData = SomeInOutData
  { _someZkey   :: !(Direction (SomeWithProxy ZKey    ))
  , _someVkey   :: !(Direction (SomeWithProxy VKey    ))
  , _someWtns   :: !(Direction (SomeWithProxy Witness ))
  , _someR1CS   :: !(Direction (SomeWithProxy R1CS    ))
  , _someProof  :: !(Direction (SomeWithProxy Proof   ))
  , _somePubIO  :: !(Direction (SomeWithProxy PublicIO))
  }

data InOutData (c :: SomeCurve) = InOutData 
  { _dataZkey   :: !(Direction (ZKey     c))
  , _dataVkey   :: !(Direction (VKey     c))
  , _dataWtns   :: !(Direction (Witness  c))
  , _dataR1CS   :: !(Direction (R1CS     c))
  , _dataProof  :: !(Direction (Proof    c))
  , _dataPubIO  :: !(Direction (PublicIO c))
  }

--------------------------------------------------------------------------------

fmtCurveProxy :: Fmt.Curve -> Maybe (Some Proxy)
fmtCurveProxy curve = case curve of
  Fmt.Bn128       -> Just $ someCurveProxy BN128
  Fmt.Bls12_381   -> Just $ someCurveProxy BLS12_381
  _               -> Nothing

--------------------------------------------------------------------------------
-- * Existential type hackery

transposeSome :: CurveHint -> SomeInOutData -> SomeWithProxy InOutData
transposeSome hint what@(SomeInOutData{..}) =
  case nub cs of
    []  -> error "no way to choose a concrete elliptic curve"
    [c] -> final c
    _   -> error $ "transposeSome: incompatible types behind the existential wrappers:\n  " ++ show cs
  where 
    c1 = mbCurve _someZkey 
    c2 = mbCurve _someVkey 
    c3 = mbCurve _someWtns 
    c4 = mbCurve _someR1CS 
    c5 = mbCurve _someProof
    c6 = mbCurve _somePubIO
    cs = catMaybes [hint,c1,c2,c3,c4,c5,c6]

    final c = case someCurveProxy c of
      MkSome pxy -> MkSome (WithProxy pxy (unsafeCastAll pxy what))

    mbCurve :: Direction (SomeWithProxy f) -> Maybe SomeCurve
    mbCurve (Input x) = Just (extractCurve x)
    mbCurve _         = Nothing

unsafeCastAll :: forall c. Proxy c -> SomeInOutData -> InOutData c
unsafeCastAll pxy (SomeInOutData{..}) = InOutData 
  { _dataZkey   = fmap unsafeCast1 _someZkey  
  , _dataVkey   = fmap unsafeCast1 _someVkey  
  , _dataWtns   = fmap unsafeCast1 _someWtns  
  , _dataR1CS   = fmap unsafeCast1 _someR1CS  
  , _dataProof  = fmap unsafeCast1 _someProof 
  , _dataPubIO  = fmap unsafeCast1 _somePubIO 
  }
  where
    unsafeCast1 :: forall f. SomeWithProxy f -> f c
    unsafeCast1 some = case some of
      MkSome (WithProxy _ what) -> (unsafeCoerce :: f d -> f c) what

--------------------------------------------------------------------------------

-- | TEMPORARY PLACEHOLDER
loadRawVKeyFile_ :: FilePath -> IO RawVKey
loadRawVKeyFile_ _ = return RawVKey

--------------------------------------------------------------------------------
-- * Load all input files

loadEverything :: Bool -> CurveHint -> InOutFiles -> IO (SomeWithProxy InOutData)
loadEverything printFlag hint fpaths = convertEverything printFlag hint =<< loadEverythingRaw printFlag fpaths

loadEverythingRaw :: Bool -> InOutFiles -> IO RawInOutData
loadEverythingRaw printFlag fpaths = do
  !zkey  <- loadWithTime printFlag  Fmt.parseZKeyFile2_  (_fileZkey      fpaths)
  !vkey  <- loadWithTime printFlag    loadRawVKeyFile_   (_fileVkey      fpaths)
  !wtns  <- loadWithTime printFlag  Fmt.parseWtnsFile_   (_fileWtns      fpaths)
  !r1cs  <- loadWithTime printFlag  Fmt.parseR1CSFile_   (_fileR1CS      fpaths)
  !proof <- loadWithTime printFlag Json.loadRawProof_    (_fileProofJson fpaths)
  !pub   <- loadWithTime printFlag Json.loadRawPubIO_    (_filePubJson   fpaths)
  return $ RawInOutData
    { _rawZkey   = zkey
    , _rawVkey   = vkey
    , _rawWtns   = wtns
    , _rawR1CS   = r1cs
    , _rawProof  = proof
    , _rawPubIO  = pub 
    }

convertEverything :: Bool -> CurveHint -> RawInOutData -> IO (SomeWithProxy InOutData)
convertEverything printFlag hint raw = transposeSome hint <$> (convertEverything1 printFlag hint raw)

convertEverything1 :: Bool -> CurveHint -> RawInOutData -> IO SomeInOutData
convertEverything1 printFlag  hint0 (RawInOutData{..}) = do
  !zkey  <- convertWithTime printFlag "zkey"  (convertZKey           hint0) _rawZkey  ; let !hint1 = mergeHint hint0 (mbExtractCurve zkey )
  !vkey  <- convertWithTime printFlag "vkey"  (convertVKey           hint1) _rawVkey  ; let !hint2 = mergeHint hint0 (mbExtractCurve vkey )
  !wtns  <- convertWithTime printFlag "wtns"  (convertWitness        hint2) _rawWtns  ; let !hint3 = mergeHint hint1 (mbExtractCurve wtns )
  !r1cs  <- convertWithTime printFlag "r1cs"  (convertR1CS           hint3) _rawR1CS  ; let !hint4 = mergeHint hint2 (mbExtractCurve r1cs )
  !proof <- convertWithTime printFlag "proof" (convertToSomeProof    hint4) _rawProof ; let !hint5 = mergeHint hint3 (mbExtractCurve proof) 
  !pubIO <- convertWithTime printFlag "pubIO" (convertToSomePublicIO hint5) _rawPubIO ; let !hint6 = mergeHint hint4 (mbExtractCurve pubIO)
  return $! SomeInOutData
    { _someZkey  = zkey  
    , _someVkey  = vkey  
    , _someWtns  = wtns  
    , _someR1CS  = r1cs  
    , _someProof = proof 
    , _somePubIO = pubIO 
    }

mbExtractCurve :: Direction (SomeWithProxy f) -> Maybe SomeCurve
mbExtractCurve dir = fmap extractCurve (fromInput dir)
  

{-
loadEverything :: CurveHint -> InOutFiles -> IO (Some (WithProxy InOutData))
loadEverything hint0 fpaths = do
  zkey  <- loadWith  Fmt.parseZKeyFile_     (_fileZkey      fpaths)
  wtns  <- loadWith  Fmt.parseWtnsFile_     (_fileWtns      fpaths)
  r1cs  <- loadWith  Fmt.parseR1CSFile_     (_fileR1CS      fpaths)
  proof <- loadWith Json.loadProofFile_     (_fileProofJson fpaths)
  pub   <- loadWith Json.loadPublicIOFile_  (_filePubJson   fpaths)
  let curve = Fmt.zkeyCurve zkey
  return $ case someCurveProxy curve of
    Nothing      -> error $ "unsupported elliptic curve `" ++ show curve ++ "`"
    Just somePxy -> case somePxy of 
      MkSome pxy   -> MkSome $ WithProxy pxy $ InOutData
        { _dataZkey      = zkey
        , _dataWtns      = wtns
        , _dataR1CS      = r1cs
        , _dataProofJson = proof
        , _dataPubJson   = pub 
        }
  where
-}

----------------------------------------
-- * old TMP version

{-
data FilePaths = FilePaths
  { _zkeyFile :: FilePath
  , _wtnsFile :: FilePath
  }
  deriving Show

loadStuff :: FilePaths -> IO (Some ProverInput)
loadStuff fpaths = do
  zkey <- Fmt.parseZKeyFile_ (_zkeyFile fpaths)
  wtns <- Fmt.parseWtnsFile_ (_wtnsFile fpaths)
  let curve = Fmt.zkeyCurve zkey
  return $ case fmtCurveProxy curve of
    Nothing      -> error $ "unsupported elliptic curve `" ++ show curve ++ "`"
    Just somePxy -> case somePxy of 
      MkSome pxy   -> MkSome $ ProverInput
        { _theProxy    = pxy
        , _theZKey     = convertZKey'    pxy zkey 
        , _theWitness  = convertWitness' pxy wtns
        }
-}

--------------------------------------------------------------------------------
-- * Helpers

loadWith :: (a -> IO b) -> Direction a -> IO (Direction b)
loadWith action (Input  x)  = Input <$> action x
loadWith action (Output _)  = return None
loadWith action None        = return None

loadWithTime :: Bool -> (FilePath -> IO b) -> Direction FilePath -> IO (Direction b)
loadWithTime flag action (Input fn) = printMeasureTime flag ("loading `" ++ fn ++ "`") $ loadWith action (Input fn)
loadWithTime _    _      _          = return None

convertWithTime :: Functor f => Bool -> String -> (a -> b) -> f a -> IO (f b)
convertWithTime True text fun !x = printMeasureTime True ("converting " ++ text) $ do { let !y = fmap fun x in y `seq` return y }
convertWithTime _    text fun !x = return $! (fmap fun x)

saveWith :: (a -> b -> IO ()) -> Direction a -> Direction b -> IO ()
saveWith action (Output x) (Output y)  = action x y 
saveWith _      _          _           = return ()

saveWith_ :: (a -> b -> IO ()) -> Direction a -> b -> IO ()
saveWith_ action (Output x) y  = action x y 
saveWith_ _      _          _  = return ()


--------------------------------------------------------------------------------
