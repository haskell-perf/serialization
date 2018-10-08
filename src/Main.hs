{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- import           Data.Monoid                         ((<>))
import Data.Typeable

-- import           Data.Word
-- import           Criterion.IO
import Criterion.Types

-- import           Data.Bifunctor
import qualified Data.Binary as B

-- import           Data.Binary.Serialise.CBOR as CBOR
import Codec.Serialise as CBOR

-- import           Data.List
-- import           Data.Ord
import GHC.Generics

-- import           Statistics.Resampling.Bootstrap
import System.Mem (performMajorGC)

-- import           System.Random
-- import           Text.Printf
-- import           Data.Binary.Serialise.CBOR.Decoding as CBOR
-- import           Data.Binary.Serialise.CBOR.Encoding as CBOR
import qualified Data.Flat as F
import qualified Data.Serialize as C
import qualified Data.Store as S
import Dataset
import qualified GHC.Packing as P
import Report

-- Testing and random data generation
import Test.QuickCheck

-- Benchmarks
import Criterion.Main

data BinTree a
  = Tree (BinTree a)
         (BinTree a)
  | Leaf a
  deriving (Show, Eq, Typeable, Generic)

-- General instances
instance {-# OVERLAPPABLE #-} F.Flat a => F.Flat (BinTree a)

instance {-# OVERLAPPABLE #-} S.Store a => S.Store (BinTree a)

instance {-# OVERLAPPABLE #-} B.Binary a => B.Binary (BinTree a)

instance {-# OVERLAPPABLE #-} C.Serialize a => C.Serialize (BinTree a)

instance {-# OVERLAPPABLE #-} CBOR.Serialise a =>
                              CBOR.Serialise (BinTree a)

-- -- Specialised instances, sometime increase performance
-- instance {-# OVERLAPPING #-} F.Flat [Direction]
-- instance {-# OVERLAPPING #-} F.Flat (BinTree Direction)
-- instance {-# OVERLAPPING #-} F.Flat (BinTree Int)
-- instance {-# OVERLAPPING #-} S.Store [Direction]
-- instance {-# OVERLAPPING #-} S.Store (BinTree Direction)
-- instance {-# OVERLAPPING #-} S.Store (BinTree Int)
-- instance {-# OVERLAPPING #-} B.Binary [Direction]
-- instance {-# OVERLAPPING #-} B.Binary (BinTree Direction)
-- instance {-# OVERLAPPING #-} B.Binary (BinTree Int)
-- instance {-# OVERLAPPING #-} C.Serialize [Direction]
-- instance {-# OVERLAPPING #-} C.Serialize (BinTree Direction)
-- instance {-# OVERLAPPING #-} C.Serialize (BinTree Int)
-- instance {-# OVERLAPPING #-} CBOR.Serialise [Direction]
-- instance {-# OVERLAPPING #-} CBOR.Serialise (BinTree Direction)
-- instance {-# OVERLAPPING #-} CBOR.Serialise (BinTree Int)
instance NFData a => NFData (BinTree a) where
  rnf (Leaf a) = rnf a `seq` ()
  rnf (Tree left right) = rnf left `seq` rnf right `seq` ()

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = oneof [Leaf <$> arbitrary, Tree <$> arbitrary <*> arbitrary]
  shrink Leaf {} = []
  shrink (Tree left right) = [left, right] ++ shrink left ++ shrink right

-- A simple enumeration
data Direction
  = North
  | South
  | Center
  | East
  | West
  deriving ( Eq
           , Ord
           , Read
           , Show
           , Typeable
           , Generic
           , NFData
           , B.Binary
           , C.Serialize
           , CBOR.Serialise
           , S.Store
           , F.Flat
           )

instance Arbitrary Direction where
  arbitrary = elements [North, South, Center, East, West]

-- Custom instances, unused as all packages offer automatic derivation of instances
-- instance B.Binary a => B.Binary (BinTree a) where
--   put (Leaf a) = do
--     B.put (0 :: Word8)
--     B.put a
--   put (Tree left right) = do
--     B.put (1 :: Word8)
--     B.put left
--     B.put right
--   get = do
--     t <- B.get :: B.Get Word8
--     case t ofer
--       0 -> Leaf <$> B.get
--       1 -> Tree <$> B.get <*> B.get
--       _ -> error "Binary.get for BinTree"
-- instance C.Serialize a => C.Serialize (BinTree a) where
--   put (Leaf a) = do
--     C.put (0 :: Word8)
--     C.put a
--   put (Tree left right) = do
--     C.put (1 :: Word8)
--     C.put left
--     C.put right
--   get = do
--     t <- C.get :: C.Get Word8
--     case t of
--       0 -> Leaf <$> C.get
--       1 -> Tree <$> C.get <*> C.get
--       _ -> error "Serialize.get for BinTree"
-- instance CBOR.Serialise a => CBOR.Serialise (BinTree a) where
--   encode (Leaf a) =
--     Encoding (TkTag 0) <> encode a
--   encode (Tree left right) =
--     Encoding (TkTag 1) <> encode left <> encode right
--   decode =
--     decodeTag >>= \case
--       0 -> Leaf <$> decode
--       1 -> Tree <$> decode <*> decode
--       _ -> fail "CBOR.Serialise.decode for BinTree"
data PkgBinary =
  PkgBinary

data PkgCereal =
  PkgCereal

data PkgPackman =
  PkgPackman

data PkgCBOR =
  PkgCBOR

data PkgFlat =
  PkgFlat

data PkgStore =
  PkgStore

class Serialize lib a where
  serialize :: lib -> a -> IO BS.ByteString
  deserialize :: lib -> BS.ByteString -> IO a

instance (B.Binary a, NFData a) => Serialize PkgBinary a where
  {-# NOINLINE serialize #-}
  serialize _ = return . force . LBS.toStrict . B.encode
  {-# NOINLINE deserialize #-}
  deserialize _ = return . force . B.decode . LBS.fromStrict

instance (C.Serialize a, NFData a) => Serialize PkgCereal a where
  {-# NOINLINE serialize #-}
  serialize _ = return . force . C.encode
  {-# NOINLINE deserialize #-}
  deserialize _ = either error (return . force) . C.decode

instance (NFData a, Typeable a) => Serialize PkgPackman a where
  {-# NOINLINE serialize #-}
  serialize _ =
    fmap (force . LBS.toStrict . B.encode) .
    flip P.trySerializeWith (1000 * 2 ^ (20 :: Int))
  {-# NOINLINE deserialize #-}
  deserialize _ = fmap force . P.deserialize . B.decode . LBS.fromStrict

instance (CBOR.Serialise a, NFData a) => Serialize PkgCBOR a where
  {-# NOINLINE serialize #-}
  serialize _ = return . force . LBS.toStrict . CBOR.serialise
  {-# NOINLINE deserialize #-}
  deserialize _ = return . force . CBOR.deserialise . LBS.fromStrict

instance (S.Store a, NFData a) => Serialize PkgStore a where
  {-# NOINLINE serialize #-}
  serialize _ = return . force . S.encode
  {-# NOINLINE deserialize #-}
  deserialize _ = return . force . fromRight . S.decode

instance (F.Flat a, NFData a) => Serialize PkgFlat a where
  {-# NOINLINE serialize #-}
  serialize _ = return . force . F.flat
  {-# NOINLINE deserialize #-}
  deserialize _ = return . force . fromRight . F.unflat

pkgs ::
     ( NFData a
     , C.Serialize a
     , Typeable a
     , Serialise a
     , S.Store a
     , F.Flat a
     , B.Binary a
     )
  => [(String, a -> IO BS.ByteString, BS.ByteString -> IO a)]
pkgs =
  [ ("flat", serialize PkgFlat, deserialize PkgFlat)
  , ("store", serialize PkgStore, deserialize PkgStore)
  , ("binary", serialize PkgBinary, deserialize PkgBinary)
  , ("cereal", serialize PkgCereal, deserialize PkgCereal)
  , ("packman", serialize PkgPackman, deserialize PkgPackman)
  , ("serialise", serialize PkgCBOR, deserialize PkgCBOR)
  ]

prop :: Serialize lib (BinTree Int) => lib -> Property
prop lib = forAll arbitrary (ioProperty . test)
  where
    test :: BinTree Int -> IO Bool
    test t = do
      s <- serialize lib t
      d <- deserialize lib s
      return $ d == t

runQC :: Serialize lib (BinTree Int) => lib -> IO ()
runQC = quickCheckWith stdArgs {maxSuccess = 1000} . prop

generateBalancedTree :: (Arbitrary a1) => Int -> IO (BinTree a1)
generateBalancedTree = generateBalancedTree_ (generate $ arbitrary)
  where
    generateBalancedTree_ r 0 = Leaf <$> r
    generateBalancedTree_ r n =
      Tree <$> generateBalancedTree_ r (n - 1) <*>
      generateBalancedTree_ r (n - 1)

workDir :: [Char]
workDir = ""

runBench :: IO ()
runBench
  -- Data structures to (de)serialise
 = do
  !intTree <-
    force . ("BinTree Int", ) <$> (generateBalancedTree 21 :: IO (BinTree Int))
  !directionTree <-
    force . ("BinTree Direction", ) <$>
    (generateBalancedTree 21 :: IO (BinTree Direction))
  !directionList <-
    force . ("[Direction]", ) <$>
    mapM (\_ -> generate $ arbitrary :: IO Direction) [1 .. 100000 :: Int]
  !carsDataset <- force . ("Cars dataset", ) <$> carsData
    -- !abaloneDataset <- force . ("Abalone dataset",) <$> abaloneData
  let !irisDataset = force ("Iris dataset", irisData)
  performMajorGC
  let jsonReport = reportsFile workDir
  let htmlReport = "report.html"
  let tests =
        benchs directionList ++
        benchs intTree ++
        benchs directionTree ++ benchs carsDataset ++ benchs irisDataset
  --let tests = []
  defaultMainWith
    (defaultConfig {jsonFile = Just jsonReport, reportFile = Just htmlReport}) $
    tests
  deleteMeasures workDir
  updateMeasures workDir
  sizes directionList
  sizes directionTree
  sizes intTree
  sizes carsDataset
  sizes irisDataset
  addTransfers workDir
  putStrLn "Summary:\n"
    -- printMeasuresDiff ms
  -- printMeasures workDir
  printSummary workDir
  reportMeasures workDir

sizes ::
     ( Typeable t
     , NFData t
     , B.Binary t
     , F.Flat t
     , Serialise t
     , C.Serialize t
     , S.Store t
     )
  => (String, t)
  -> IO ()
sizes (name, obj) = do
  ss <-
    mapM
      (\(n, s, _) -> (\ss -> (n, fromIntegral . BS.length $ ss)) <$> s obj)
      pkgs
  -- print ("sizes for " ++ name) >> print ss
  addMeasures workDir ("size (bytes)/" ++ name) ss

benchs ::
     ( Eq a
     , Typeable a
     , NFData a
     , B.Binary a
     , F.Flat a
     , Serialise a
     , C.Serialize a
     , S.Store a
     )
  => (String, a)
  -> [Benchmark]
benchs (name, obj) =
  let nm pkg = concat [name, "-", pkg]
                                                                          -- env (return obj) $ \sobj -> bgroup ("serialization (mSecs)") $ map (\(pkg,s,_) -> bench (nm pkg) (nfIO (s sobj))) pkgs
   in [ bgroup "serialization (time)" $
        map (\(pkg, s, _) -> bench (nm pkg) (nfIO (s obj))) pkgs
                                                                          -- NOTE: the benchmark time includes the comparison of the deserialised obj with the original
      , bgroup "deserialization (time)" $
        map
          (\(pkg, s, d) ->
             env (s obj) $ (\bs -> bench (nm pkg) $ whnfIO ((obj ==) <$> d bs)))
          pkgs
      ]

main :: IO ()
main
    -- runQC Binary
    -- runQC Cereal
    -- runQC Packman
    -- runQC CBOR
 = do
  runBench
    -- print $ S.encode [North, South] -- "\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH" 
    --                                 --  "\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"

fromRight :: Either a b -> b
fromRight (Right v) = v
fromRight (Left _) = error "Unexpected Left"
