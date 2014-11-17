{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

module Main where

import           Control.Applicative
import           Control.DeepSeq
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Typeable
import           Data.Word
import           System.IO
import           System.Random

-- Serialization libs
import qualified Data.Binary               as B
import qualified Data.Serialize            as C
import qualified GHC.Packing               as P

-- Testing and random data generation
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

-- Benchmarks
import           Criterion.Main
import           Criterion.Types

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

data Binary = Binary
data Cereal = Cereal
data Packman = Packman

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
    serialize   _ = fmap (force . LBS.toStrict . B.encode) . flip P.trySerializeWith (1000 * 2^20)
    deserialize _ = fmap force . P.deserialize . B.decode . LBS.fromStrict

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
  putStr "Generating tree... "
  hFlush stdout
  tree <- force <$> generateBalancedTree 22
  putStrLn "Done."
  defaultMainWith defaultConfig{forceGC=True, verbosity=Verbose}
    [ bgroup "binary"
      [ bench "serialize" $ nfIO $ serialize Binary tree
      , bench "serialize + deserialize" $ nfIO
          (serialize Binary tree >>= deserialize Binary :: IO (BinTree Int))
      ]
    , bgroup "cereal"
      [ bench "serialize" $ nfIO $ serialize Cereal tree
      , bench "serialize + deserialize" $ nfIO
          (serialize Cereal tree >>= deserialize Cereal :: IO (BinTree Int))
      ]
    , bgroup "packman"
      [ bench "serialize" $ nfIO $ serialize Packman tree
      , bench "serialize + deserialize" $ nfIO
          (serialize Packman tree >>= deserialize Packman :: IO (BinTree Int))
      ]
    ]

main :: IO ()
main = do
    -- runQC Binary
    -- runQC Cereal
    -- runQC Packman

    runBench
