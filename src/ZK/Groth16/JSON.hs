
-- | JSON input\/output of proof and public IO

{-# LANGUAGE ScopedTypeVariables #-}
module ZK.Groth16.JSON where

--------------------------------------------------------------------------------

import Data.Proxy
import Control.Exception

import ZK.Algebra.API
import ZK.Algebra.Class.Pairing

import ZK.Groth16.Types
import ZK.Groth16.Aux.Some

--------------------------------------------------------------------------------
-- * \"Raw\" types (untyped proof and public IO)

type RawFr  = Integer
type RawFp  = Integer
type RawFp2 = (RawFp ,RawFp )
type RawG1  = (RawFp ,RawFp )
type RawG2  = (RawFp2,RawFp2)

data RawProof = RawProof
  { _rawPrfCurve :: Maybe SomeCurve 
  , _rawPiA      :: RawG1
  , _rawPiB      :: RawG2 
  , _rawPiC      :: RawG1
  }
  deriving (Eq,Show)

data RawPubIO = RawPubIO
  { _rawPubCurve  :: Maybe SomeCurve 
  , _rawPubValues :: [RawFr]
  }
  deriving (Eq,Show)

showRawG1 :: RawG1 -> String
showRawG1 (x,y) = unlines
  [ "    [ " ++ show (show x)
  , "    , " ++ show (show y)
  , "    , \"1\""
  , "    ]"
  ]

showRawG2 :: RawG2 -> String
showRawG2 ((xa,xb),(ya,yb)) = unlines
  [ "    [ [ " ++ show (show xa)
  , "      , " ++ show (show xb)
  , "      ]"
  , "    , [ " ++ show (show ya)
  , "      , " ++ show (show yb)
  , "      ]"
  , "    , [ \"1\""
  , "      , \"0\""
  , "      ]"
  , "    ]"
  ]

--------------------------------------------------------------------------------
-- * Pretty-printing

toRawFr :: forall c. PairingCurve c => Proxy c -> Fr c -> RawFr
toRawFr pxy x = asInteger x

toRawFp :: forall c. PairingCurve c => Proxy c -> Fp c -> RawFp
toRawFp pxy x = asInteger x

toRawFp2 :: forall c. PairingCurve c => Proxy c -> Fp2 c -> RawFp2
toRawFp2 pxy x = let (u,v) = quadraticUnpack x in (toRawFp pxy u, toRawFp pxy v)

toRawG1 :: forall c. PairingCurve c => Proxy c -> G1 c -> RawG1
toRawG1 pxy g1 = let (x,y) = coords2 g1 in (toRawFp pxy x, toRawFp pxy y)

toRawG2 :: forall c. PairingCurve c => Proxy c -> G2 c -> RawG2
toRawG2 pxy g2 = let (x,y) = coords2 g2 in (toRawFp2 pxy x, toRawFp2 pxy y)

--------------------------------------------------------------------------------

proofToRawProof :: forall c. PairingCurve c => Proxy c -> Proof c -> RawProof
proofToRawProof pxy (Proof piA piB piC) = RawProof
  { _rawPrfCurve = Just (lowerSomeCurve pxy)
  , _rawPiA      = toRawG1 pxy piA
  , _rawPiB      = toRawG2 pxy piB
  , _rawPiC      = toRawG1 pxy piC
  }

publicIOToRawPubIO :: forall c. PairingCurve c => Proxy c -> PublicIO c -> RawPubIO
publicIOToRawPubIO pxy (PublicIO flatarr) = RawPubIO
  { _rawPubCurve  = Just (lowerSomeCurve pxy)
  , _rawPubValues = map asInteger (unpackFlatArrayToList flatarr)
  }

--------------------------------------------------------------------------------

rawCurveToString :: SomeCurve -> String
rawCurveToString c = case c of 
  BN128     -> "bn128"
  BLS12_381 -> "bls12-381"

rawProofToJSON :: RawProof -> String
rawProofToJSON (RawProof mbcurve piA piB piC) = unlines $
  [ "{ \"protocol\": " ++ "\"groth16\"" 
  ] ++ 
  (case mbcurve of
    Just curve -> [ ", \"curve\":    " ++ rawCurveToString curve ]
    Nothing    -> []
  ) ++
  [ ", \"pi_a\":" 
  , showRawG1 piA 
  , ", \"pi_b\":"
  , showRawG2 piB
  , ", \"pi_c\":"
  , showRawG1 piC
  , "}"
  ]

exportRawProof :: FilePath -> RawProof -> IO ()
exportRawProof fpath proof = writeFile fpath (rawProofToJSON proof)

exportProof :: PairingCurve c => Proxy c -> FilePath -> Proof c -> IO () 
exportProof pxy fpath proof = exportRawProof fpath (proofToRawProof pxy proof)

--------------------------------------------------------------------------------

rawPubIOToJSON :: RawPubIO -> String
rawPubIOToJSON (RawPubIO mbcurve values) = unlines ls where
  ls  = ls1 ++ ["]"] 
  ls1 = zipWith f cs values 
  f c v = [c] ++ " " ++ show v
  cs = '[' : repeat ','

exportRawPubIO :: FilePath -> RawPubIO -> IO ()
exportRawPubIO fpath pubio = writeFile fpath (rawPubIOToJSON pubio)

exportPublicIO :: PairingCurve c => Proxy c -> FilePath -> PublicIO c -> IO () 
exportPublicIO pxy fpath pubio = exportRawPubIO fpath (publicIOToRawPubIO pxy pubio)

--------------------------------------------------------------------------------
-- * Conversion between typed and untyped versions

fromRawFr :: PairingCurve c => Proxy c -> RawFr -> Fr c
fromRawFr pxy = fromInteger

fromRawFp :: PairingCurve c => Proxy c -> RawFp -> Fp c
fromRawFp pxy = fromInteger

fromRawFp2 :: PairingCurve c => Proxy c -> RawFp2 -> Fp2 c
fromRawFp2 pxy (a,b) = quadraticPack (fromRawFp pxy a, fromRawFp pxy b)

fromRawG1 :: PairingCurve c => Proxy c -> RawG1 -> G1 c
fromRawG1 pxy (x,y) = mkPoint2 (fromRawFp pxy x, fromRawFp pxy y)

fromRawG2 :: PairingCurve c => Proxy c -> RawG2 -> G2 c
fromRawG2 pxy (x,y) = mkPoint2 (fromRawFp2 pxy x, fromRawFp2 pxy y)

--------------------------------------------------------------------------------

convertToSomeProof :: CurveHint -> RawProof -> Some (WithProxy Proof)
convertToSomeProof curveHint (RawProof mbCurve piA piB piC) = 
  case guessCurve curveHint mbCurve of
    Left  msg   -> error msg
    Right curve -> case someCurveProxy curve of
      MkSome pxy  -> MkSome $ WithProxy pxy $ Proof 
        (fromRawG1 pxy piA)
        (fromRawG2 pxy piB)
        (fromRawG1 pxy piC)

convertToSomePublicIO :: CurveHint -> RawPubIO -> Some (WithProxy PublicIO)
convertToSomePublicIO curveHint (RawPubIO mbCurve values) = 
  case guessCurve curveHint mbCurve of
    Left  msg   -> error msg
    Right curve -> case someCurveProxy curve of
      MkSome pxy  -> MkSome $ WithProxy pxy $ PublicIO $ packFlatArrayFromList $ map (fromRawFr pxy) values

--------------------------------------------------------------------------------
-- * Parsing

parseRawProof :: String -> Either String RawProof
parseRawProof str = Left "parseRawProof: not implemented"

parseRawPubIO :: String -> Either String RawPubIO
parseRawPubIO str = Left "parseRawPubIO: not implemented"

{-
parseProof :: PairingCurve c => Proxy c -> String -> Either String (Proof c)
parseProof pxy str = Left "parseProof: not implemented"

parseProof_ :: PairingCurve c => Proxy c -> String -> Proof c
parseProof_ pxy str = case parseProof pxy str of
  Right prf -> prf
  Left  msg -> error msg -- "parseProof_: parsing failed"

parsePublicIO :: PairingCurve c => Proxy c -> String -> Either String (PublicIO c)
parsePublicIO pxy str = Left "parsePublicIO: not implemented"

parsePublicIO_ :: PairingCurve c => Proxy c -> String -> PublicIO c
parsePublicIO_ pxy str = case parsePublicIO pxy str of
  Right pub -> pub
  Left  msg -> error msg -- error "parsePublicIO_: parsing failed"
-}

--------------------------------------------------------------------------------
-- * Loading files

loadRawProof :: String -> IO (Either String RawProof)
loadRawProof fpath = do
  ei <- trySome (readFile fpath)
  return $ case ei of
    Left  msg  -> Left (show msg)
    Right text -> parseRawProof text

loadRawProof_ :: String -> IO RawProof
loadRawProof_ fpath = do
  ei <- loadRawProof fpath
  case ei of
    Left  msg -> fail   msg
    Right prf -> return prf

loadRawPubIO :: String -> IO (Either String RawPubIO)
loadRawPubIO fpath = do
  ei <- trySome (readFile fpath)
  return $ case ei of
    Left  msg  -> Left (show msg)
    Right text -> parseRawPubIO text

loadRawPubIO_ :: String -> IO RawPubIO
loadRawPubIO_ fpath = do
  ei <- loadRawPubIO fpath
  case ei of
    Left  msg -> fail   msg
    Right pub -> return pub

{-
loadProofFile :: PairingCurve c => Proxy c -> String -> IO (Either String (Proof c))
loadProofFile pxy fpath = do
  ei <- trySome (readFile fpath)
  return $ case ei of
    Left  msg  -> Left (show msg)
    Right text -> parseProof pxy text

loadProofFile_ :: PairingCurve c => Proxy c -> String -> IO (Proof c)
loadProofFile_ pxy fpath = do
  ei <- loadProofFile pxy fpath
  case ei of
    Left  msg -> fail   msg
    Right prf -> return prf

loadPublicIOFile :: PairingCurve c => Proxy c -> String -> IO (Either String (PublicIO c))
loadPublicIOFile pxy fpath = do
  ei <- trySome (readFile fpath)
  return $ case ei of
    Left  msg  -> Left (show msg)
    Right text -> parsePublicIO pxy text

loadPublicIOFile_ :: PairingCurve c => Proxy c -> String -> IO (PublicIO c)
loadPublicIOFile_ pxy fpath = do
  ei <- loadPublicIOFile pxy fpath
  case ei of
    Left  msg -> fail   msg
    Right pub -> return pub
-}

trySome :: IO a -> IO (Either SomeException a)
trySome = try

--------------------------------------------------------------------------------
