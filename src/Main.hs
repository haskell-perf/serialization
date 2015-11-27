{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             LambdaCase, MultiParamTypeClasses #-}

module Main where

import           Control.DeepSeq
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import           Data.Monoid                         ((<>))
import           Data.Typeable
import           Data.Word
import           System.Mem                          (performMajorGC)
import           System.Random

-- Serialization libs
import qualified Data.Binary                         as B
import           Data.Binary.Serialise.CBOR          as CBOR
import           Data.Binary.Serialise.CBOR.Decoding as CBOR
import           Data.Binary.Serialise.CBOR.Encoding as CBOR
import qualified Data.Serialize                      as C
import qualified GHC.Packing                         as P

-- Testing and random data generation
import           Test.QuickCheck

-- Benchmarks
import           Criterion.Main

data BinTree a = Tree (BinTree a) (BinTree a) | Leaf a
  deriving (Show, Eq, Typeable)

instance NFData a => NFData (BinTree a) where
    rnf (Leaf a) = rnf a `seq` ()
    rnf (Tree left right) = rnf left `seq` rnf right `seq` ()

instance Arbitrary a => Arbitrary (BinTree a) where
    arbitrary = oneof [Leaf <$> arbitrary, Tree <$> arbitrary <*> arbitrary]

    shrink Leaf{} = []
    shrink (Tree left right) = [left, right] ++ shrink left ++ shrink right

instance B.Binary a => B.Binary (BinTree a) where
  put (Leaf a) = do
    B.put (0 :: Word8)
    B.put a

  put (Tree left right) = do
    B.put (1 :: Word8)
    B.put left
    B.put right

  get = do
    t <- B.get :: B.Get Word8
    case t of
      0 -> Leaf <$> B.get
      1 -> Tree <$> B.get <*> B.get
      _ -> error "Binary.get for BinTree"

instance C.Serialize a => C.Serialize (BinTree a) where
  put (Leaf a) = do
    C.put (0 :: Word8)
    C.put a

  put (Tree left right) = do
    C.put (1 :: Word8)
    C.put left
    C.put right

  get = do
    t <- C.get :: C.Get Word8
    case t of
      0 -> Leaf <$> C.get
      1 -> Tree <$> C.get <*> C.get
      _ -> error "Serialize.get for BinTree"

instance CBOR.Serialise a => CBOR.Serialise (BinTree a) where
  encode (Leaf a) =
    Encoding (TkTag 0) <> encode a

  encode (Tree left right) =
    Encoding (TkTag 1) <> encode left <> encode right

  decode =
    decodeTag >>= \case
      0 -> Leaf <$> decode
      1 -> Tree <$> decode <*> decode
      _ -> fail "CBOR.Serialise.decode for BinTree"

data Binary = Binary
data Cereal = Cereal
data Packman = Packman
data CBOR = CBOR

class Serialize lib a where
    serialize :: lib -> a -> IO BS.ByteString
    deserialize :: lib -> BS.ByteString -> IO a

instance (B.Binary a, NFData a) => Serialize Binary a where
    serialize   _ = return . force . LBS.toStrict . B.encode
    deserialize _ = return . force . B.decode . LBS.fromStrict

instance (C.Serialize a, NFData a) => Serialize Cereal a where
    serialize   _ = return . force . C.encode
    deserialize _ = either error (return . force) . C.decode

instance (NFData a, Typeable a) => Serialize Packman a where
    serialize   _ =
      fmap (force . LBS.toStrict . B.encode) . flip P.trySerializeWith (1000 * 2^(20 :: Int))
    deserialize _ = fmap force . P.deserialize . B.decode . LBS.fromStrict

instance (CBOR.Serialise a, NFData a) => Serialize CBOR a where
    serialize _   = return . force . LBS.toStrict . CBOR.serialise
    deserialize _ = return . force . CBOR.deserialise . LBS.fromStrict

prop :: Serialize lib (BinTree Int) => lib -> Property
prop lib = forAll arbitrary (ioProperty . test)
  where
    test :: BinTree Int -> IO Bool
    test t = do
      s <- serialize lib t
      d <- deserialize lib s
      return $ d == t

runQC :: Serialize lib (BinTree Int) => lib -> IO ()
runQC = quickCheckWith stdArgs{maxSuccess=1000} . prop

generateBalancedTree :: Word32 -> IO (BinTree Int)
generateBalancedTree 0 = Leaf <$> randomIO
generateBalancedTree n = Tree <$> generateBalancedTree (n-1) <*> generateBalancedTree (n-1)

runBench :: IO ()
runBench = do
  tree <- force <$> generateBalancedTree 22

  bs_binary <- serialize Binary tree
  bs_cereal <- serialize Cereal tree
  bs_packman <- serialize Packman tree
  bs_binary_cbor <- serialize CBOR tree

  putStrLn $ "Length of ByteString generated by binary:                " ++ show (BS.length bs_binary)
  putStrLn $ "Length of ByteString generated by cereal:                " ++ show (BS.length bs_cereal)
  putStrLn $ "Length of ByteString generated by packman:               " ++ show (BS.length bs_packman)
  putStrLn $ "Length of ByteString generated by binary-serialise-cbor: " ++ show (BS.length bs_binary_cbor)

  performMajorGC

  defaultMain
    -- Previously we had `serialize >>= deserialize` benchmarks here, but I
    -- removed them because with '-O2' binary had a weird performance burst that
    -- showed that `serialize` takes same amount of time with `serialize >>=
    -- deserialize`. I don't understand how can that happen, but in practice we
    -- never serialize only to immediately deserialize. So hopefully this new
    -- version is more useful and accurate.
    [ env (generateBalancedTree 22) $ \tree ->
        bgroup "serialization"
          [ bench "binary"  $ nfIO $ serialize Binary tree
          , bench "cereal"  $ nfIO $ serialize Cereal tree
          , bench "packman" $ nfIO $ serialize Packman tree
          , bench "binary-CBOR" $ nfIO $ serialize CBOR tree
          ]
    , bgroup "deserialization"
      [ env (generateBalancedTree 22 >>= serialize Binary) $ \bs ->
          bench "binary" $ whnfIO (deserialize Binary bs :: IO (BinTree Int))
      , env (generateBalancedTree 22 >>= serialize Cereal) $ \bs ->
          bench "cereal" $ whnfIO (deserialize Cereal bs :: IO (BinTree Int))
      , env (generateBalancedTree 22 >>= serialize Packman) $ \bs ->
          bench "packman" $ whnfIO (deserialize Packman bs :: IO (BinTree Int))
      , env (generateBalancedTree 22 >>= serialize CBOR) $ \bs ->
          bench "binary-CBOR" $ whnfIO (deserialize CBOR bs :: IO (BinTree Int))
      ]
    ]

main :: IO ()
main = do
    -- runQC Binary
    -- runQC Cereal
    -- runQC Packman
    -- runQC CBOR

    runBench
