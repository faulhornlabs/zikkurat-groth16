
-- | Command line interface to the prover

{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeApplications, DataKinds, KindSignatures #-}
module Main where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Char
import Data.List
import Data.Semigroup ( (<>) )
import Data.Proxy

import System.Exit

import Options.Applicative
 
import ZK.Groth16
import ZK.Groth16.Aux.Some 
import ZK.Groth16.Aux.Time

import ZK.Algebra.API hiding ( (<**>) )
import ZK.Algebra.Class.Pairing

--------------------------------------------------------------------------------

stopWithMessage :: String -> IO a
stopWithMessage msg = do
  putStrLn msg
  exitWith (ExitFailure 22)      -- 22 = invalid argument

main :: IO ()
main = do
  opts <- execParser options
  when (_optVersion   opts) $ putStrLn versionString  -- "zikkurat-groth16 v0.1"
  when (_optDebugFlag opts) $ printOpts opts

  let files = _optFiles opts
  when (null $ inputFiles files) $ do
    stopWithMessage "no input files given, nothing we can do"

  let printFlag =  (_optVerbosity opts >= Verbose) || (_optMeasureTime opts) 
  someInputData <- loadEverything printFlag (_optCurve opts) files
  case someInputData of
    MkSome (WithProxy pxy inputData) -> execute pxy opts inputData

--------------------------------------------------------------------------------

execute :: forall c. PairingCurve c => Proxy c -> Opts -> InOutData c -> IO ()
execute pxy opts inputData = do

  when (_optVerbosity opts >= Verbose || _optDebugFlag opts) $ do
    putStrLn $ "elliptic curve = " ++ quotedCurve (lowerSomeCurve pxy)

  mbZkey  <- doSetup  pxy opts inputData
  mbProof <- doProve  pxy opts inputData
  _       <- doVerify pxy opts inputData

  putStrLn "OK."

doSetup :: forall c. PairingCurve c => Proxy c -> Opts -> InOutData c -> IO (Maybe (ZKey c))
doSetup pxy opts inputData = do
  let files = _optFiles opts
  maybeWhen (_optDoSetup opts) $ do
    unless (isInput (_fileR1CS files)) $ do
      stopWithMessage "fake setup requires an .r1cs file"
    stopWithMessage "fake setup not implemented yet"

doProve :: forall c. PairingCurve c => Proxy c -> Opts -> InOutData c -> IO (Maybe (Proof c))
doProve pxy opts inputData = do
  let timeFlag    = (_optVerbosity opts >= Verbose || _optMeasureTime opts)
  let detailsFlag = (_optVerbosity opts >= Verbose && _optMeasureTime opts)
  let files       = _optFiles opts
  maybeWhen (_optDoProve opts) $ do
    case (_dataZkey inputData, _dataWtns inputData) of
      (Input zkey, Input wtns) -> do
        let masking = if _optEnableMask opts then TrueZK else ZeroMask
        proof <- printMeasureTime timeFlag "proving" (proveIO pxy detailsFlag masking zkey wtns)
        let pubio = extractPublicIO pxy zkey wtns
        saveWith_ (exportProof    pxy) (_fileProofJson files) proof 
        saveWith_ (exportPublicIO pxy) (_filePubJson   files) pubio

        -- TEMPORARY!
        let vkey = extractVKey pxy zkey
        ok <- printMeasureTime timeFlag "verifying" (verifyIO pxy vkey proof pubio)
        putStrLn ("verification " ++ if ok then "succeeded." else "FAILED!")

        return proof

      _ -> stopWithMessage "proving requires a zkey and a witness"

doVerify :: forall c. PairingCurve c => Proxy c -> Opts -> InOutData c -> IO ()
doVerify pxy opts inputData = do
  let timeFlag = (_optVerbosity opts >= Verbose || _optMeasureTime opts)
  when (_optDoVerify opts) $ do

    vkey <- case (_dataVkey inputData, _dataZkey inputData) of
      (Input vkey, _) -> return vkey
      (_, Input zkey) -> return (extractVKey pxy zkey)
      _               -> stopWithMessage "verifying requires a verifier key"

    case (_dataProof inputData, _dataPubIO inputData) of
      (Input proof, Input pubio) -> do 
        ok <- printMeasureTime timeFlag "verifying" (verifyIO pxy vkey proof pubio)
        unless (_optVerbosity opts <= Silent) $ do
          putStrLn ("verification " ++ if ok then "succeeded." else "FAILED!")
      _ -> stopWithMessage "verifying requires proof and a public input/output"

--------------------------------------------------------------------------------
-- * Options

data Opts = Opts
  { _optFiles         :: InOutFiles
  , _optFlavour       :: Flavour
  , _optCurve         :: Maybe SomeCurve
  , _optDoProve       :: Bool
  , _optDoVerify      :: Bool
  , _optDoSetup       :: Bool
  , _optVerbosity     :: Verbosity
  , _optMeasureTime   :: Bool
  , _optVersion       :: Bool
  , _optEnableMask    :: Bool
  , _optDebugFlag     :: Bool
  }
  deriving Show

printFiles :: InOutFiles -> IO ()
printFiles files = do
  putStrLn $ "ZkeyFile      = " ++ show (_fileZkey      files)
  putStrLn $ "WtnsFile      = " ++ show (_fileWtns      files)
  putStrLn $ "R1CSFile      = " ++ show (_fileR1CS      files)
  putStrLn $ "ProofJsonFile = " ++ show (_fileProofJson files)
  putStrLn $ "PubJsonFile   = " ++ show (_filePubJson   files)

printOpts :: Opts -> IO ()
printOpts opts = do
  putStrLn ""
  putStrLn "cli options set to:"
  putStrLn "-------------------"
  printFiles (_optFiles opts)
  putStrLn $ "Flavour       = " ++ show (_optFlavour       opts)
  putStrLn $ "Curve         = " ++ show (_optCurve         opts)
  putStrLn $ "DoProve       = " ++ show (_optDoProve       opts)
  putStrLn $ "DoVerify      = " ++ show (_optDoVerify      opts)
  putStrLn $ "DoSetup       = " ++ show (_optDoSetup       opts)
  putStrLn $ "Verbosity     = " ++ show (_optVerbosity     opts)
  putStrLn $ "MeasureTime   = " ++ show (_optMeasureTime   opts)
  putStrLn $ "Version       = " ++ show (_optVersion       opts)
  putStrLn $ "EnableMask    = " ++ show (_optEnableMask    opts)
  putStrLn $ "DebugFlag     = " ++ show (_optDebugFlag     opts)
  putStrLn ""

defaultOpts :: Opts
defaultOpts = Opts
  { _optFiles         = defaultFiles
  , _optFlavour       = Snarkjs
  , _optCurve         = Nothing
  , _optDoProve       = False  
  , _optDoVerify      = False  
  , _optDoSetup       = False  
  , _optVerbosity     = Normal
  , _optMeasureTime   = False
  , _optVersion       = False
  , _optEnableMask    = True
  , _optDebugFlag     = False
  }

defaultFiles :: InOutFiles
defaultFiles = InOutFiles
  { _fileZkey       = None 
  , _fileVkey       = None 
  , _fileWtns       = None 
  , _fileR1CS       = None 
  , _fileProofJson  = None 
  , _filePubJson    = None 
  }

--------------------------------------------------------------------------------
-- * command line options

options :: ParserInfo Opts
options = info (optsParser <**> helper)
  (  fullDesc
--  <> progDesc "zikkurat-groth16"
  <> header "zikkurat-groth16 command line interface" 
  )  

optsParser :: Parser Opts
optsParser =  Opts 
          <$> filesParser
          <*> flavourP  
          <*> curveP
          <*> doProveP  
          <*> doVerifyP 
          <*> doSetupP  
          <*> verbosityP  
          <*> measureTimeP
          <*> versionP
          <*> enableMaskP
          <*> debugFlagP

filesParser :: Parser InOutFiles
filesParser =  InOutFiles
           <$> (fmap maybeToDir zkeyFileP     ) 
           <*> (fmap maybeToDir vkeyFileP     ) 
           <*> (fmap maybeToDir wtnsFileP     ) 
           <*> (fmap maybeToDir r1csFileP     ) 
           <*> (fmap maybeToDir proofJsonFileP) 
           <*> (fmap maybeToDir pubIoJsonFileP) 
           
maybeToDir :: Maybe a -> Direction a
maybeToDir (Just x) = Input x
maybeToDir Nothing  = None

zkeyFileP :: Parser (Maybe FilePath)
zkeyFileP 
  = (Just <$> strOption
      (  long  "zkey"
      <> short 'z'
      <> metavar "ZKEY_FILE"
      <> help "zkey (prover key) file" ))
  <|> pure Nothing

vkeyFileP :: Parser (Maybe FilePath)
vkeyFileP 
  = (Just <$> strOption
      (  long  "vkey"
      <> short 'e'
      <> metavar "VKEY_FILE"
      <> help "vkey (verifier key) file" ))
  <|> pure Nothing

wtnsFileP :: Parser (Maybe FilePath)
wtnsFileP 
  = (Just <$> strOption
      (  long  "wtns"
      <> short 'w'
      <> metavar "WTNS_FILE"
      <> help "witness file" ))
  <|> pure Nothing

r1csFileP :: Parser (Maybe FilePath)
r1csFileP 
  = (Just <$> strOption
      (  long  "r1cs"
      <> short 'r'
      <> metavar "R1CS_FILE"
      <> help "r1cs (circuit) file" ))
  <|> pure Nothing

proofJsonFileP :: Parser (Maybe FilePath)
proofJsonFileP 
  = (Just <$> strOption
      (  long  "proof"
      <> short 'o'
      <> metavar "PROOF_FILE"
      <> help "proof .json file" ))
  <|> pure Nothing

pubIoJsonFileP :: Parser (Maybe FilePath)
pubIoJsonFileP 
  = (Just <$> strOption
      (  long  "public"
      <> short 'i'
      <> metavar "PUBIO_FILE"
      <> help "public I/O .json file" ))
  <|> pure Nothing

doProveP :: Parser Bool
doProveP = switch
  (  long  "prove"
  <> short 'p'
  <> help  "run the prover"
  )

doVerifyP :: Parser Bool
doVerifyP = switch
  (  long  "verify"
  <> short 'y'
  <> help  "run the verifier"
  )

doSetupP :: Parser Bool
doSetupP = switch
  (  long  "setup"
  <> short 'u'
  <> help  "run the fake circuit-specific setup"
  )

measureTimeP :: Parser Bool
measureTimeP = switch
  (  long  "time"
  <> short 't'
  <> help  "measure running times"
  )

versionP :: Parser Bool
versionP = switch
  (  long  "version"
  <> help  "print version info"
  )

debugFlagP :: Parser Bool
debugFlagP = switch
  (  long  "debug"
  <> short 'd'
  <> help  "turn on debug output"
  )

flavourP :: Parser Flavour
flavourP = jensgroth <|> snarkjs where
  jensgroth = flag' JensGroth 
    (  long  "jensgroth"
    <> short 'J'
    <> help  "original paper version of the setup"
    )
  snarkjs = flag Snarkjs Snarkjs      -- default 
    (  long  "snarkjs"
    <> short 'K'
    <> help  "snarkjs version of the setup (default)"
    )

verbosityP :: Parser Verbosity
verbosityP = (verbose <|> silent <|> pure Normal) where
  verbose = flag' Verbose
    (  long  "verbose"
    <> short 'v'
    <> help  "verbose output"
    )
  silent = flag' Silent
    (  long  "silent"
    <> short 's'
    <> help  "silent output"
    )

enableMaskP :: Parser Bool
enableMaskP = flag True False      -- default is enabled, this turns it off
  (  long  "nomask"
  <> short 'n'
  <> help  "disable full ZK masking"
  )

--------------------------------------------------------------------------------

curveP :: Parser (Maybe SomeCurve)
curveP = (Just <$> someCurveP) <|> (pure Nothing) where
  someCurveP = recogCurveName_ <$> strOption
    (  long  "curve"
    <> short 'c'
    <> metavar "CURVE_NAME"
    <> help "name of the elliptic curve (eg. BN254, BLS12-381)" )

recogCurveName_ :: String -> SomeCurve
recogCurveName_ s = case recogCurveName s of
  Just curve -> curve
  Nothing    -> error $ "unrecognized elliptic curve `" ++ s ++ "`"

recogCurveName :: String -> Maybe SomeCurve
recogCurveName s = case filter isAlphaNum (map toLower s) of
  "bn128"     -> Just BN128
  "altbn128"  -> Just BN128
  "bn254"     -> Just BN128
  "bls12381"  -> Just BLS12_381
  _           -> Nothing

--------------------------------------------------------------------------------

