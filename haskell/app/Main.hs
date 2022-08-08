module Main where

import Prelude hiding (Char)
import Control.Monad (replicateM)
import Data.List (foldl')

-- import qualified System.Random as Random
import qualified System.Random.Stateful as Random

data Bit = Bit0 | Bit1
  deriving (Show)
type Gene = (Bit, Bit, Bit, Bit)
type Chromosome = [Gene]
data Char
  = Char0
  | Char1
  | Char2
  | Char3
  | Char4
  | Char5
  | Char6
  | Char7
  | Char8
  | Char9
  | CharAdd
  | CharSub
  | CharMul
  | CharDiv
  | CharInvalid

instance Show Char where
  show Char0 = "0"
  show Char1 = "1"
  show Char2 = "2"
  show Char3 = "3"
  show Char4 = "4"
  show Char5 = "5"
  show Char6 = "6"
  show Char7 = "7"
  show Char8 = "8"
  show Char9 = "9"
  show CharAdd = "+"
  show CharSub = "-"
  show CharMul = "*"
  show CharDiv = "/"
  show CharInvalid = "n/a"

geneLength :: Int
geneLength = 4

highNumber :: Float
highNumber = 10^(12 :: Integer)

encodeGene :: Char -> Gene
encodeGene Char0 = (Bit0, Bit0, Bit0, Bit0)
encodeGene Char1 = (Bit0, Bit0, Bit0, Bit1)
encodeGene Char2 = (Bit0, Bit0, Bit1, Bit0)
encodeGene Char3 = (Bit0, Bit0, Bit1, Bit1)
encodeGene Char4 = (Bit0, Bit1, Bit0, Bit0)
encodeGene Char5 = (Bit0, Bit1, Bit0, Bit1)
encodeGene Char6 = (Bit0, Bit1, Bit1, Bit0)
encodeGene Char7 = (Bit0, Bit1, Bit1, Bit1)
encodeGene Char8 = (Bit1, Bit0, Bit0, Bit0)
encodeGene Char9 = (Bit1, Bit0, Bit0, Bit1)
encodeGene CharAdd = (Bit1, Bit0, Bit1, Bit0)
encodeGene CharSub = (Bit1, Bit0, Bit1, Bit1)
encodeGene CharMul = (Bit1, Bit1, Bit0, Bit0)
encodeGene CharDiv = (Bit1, Bit1, Bit0, Bit1)
encodeGene CharInvalid = (Bit1, Bit1, Bit1, Bit1)

decodeGene :: Gene -> Char
decodeGene (Bit0, Bit0, Bit0, Bit0) = Char0
decodeGene (Bit0, Bit0, Bit0, Bit1) = Char1
decodeGene (Bit0, Bit0, Bit1, Bit0) = Char2
decodeGene (Bit0, Bit0, Bit1, Bit1) = Char3
decodeGene (Bit0, Bit1, Bit0, Bit0) = Char4
decodeGene (Bit0, Bit1, Bit0, Bit1) = Char5
decodeGene (Bit0, Bit1, Bit1, Bit0) = Char6
decodeGene (Bit0, Bit1, Bit1, Bit1) = Char7
decodeGene (Bit1, Bit0, Bit0, Bit0) = Char8
decodeGene (Bit1, Bit0, Bit0, Bit1) = Char9
decodeGene (Bit1, Bit0, Bit1, Bit0) = CharAdd
decodeGene (Bit1, Bit0, Bit1, Bit1) = CharSub
decodeGene (Bit1, Bit1, Bit0, Bit0) = CharMul
decodeGene (Bit1, Bit1, Bit0, Bit1) = CharDiv
decodeGene _ = CharInvalid

randomBit :: Random.RandomGenM g r m => g -> m Bit
randomBit g = do
  x <- Random.randomRM (0 :: Int, 1 :: Int) g
  case x of
    0 -> pure Bit0
    _ -> pure Bit1

randomGene
  :: (MonadFail m, Random.RandomGenM g r m)
  => g
  -> m Gene
randomGene g = do
  [b1, b2, b3, b4] <- replicateM geneLength . randomBit $ g
  pure (b1, b2, b3, b4)

randomChromosome
  :: (MonadFail m, Random.RandomGenM g r m)
  => g
  -> Integer -- ^ min length, inclusive
  -> Integer -- ^ max length, inclusive
  -> m Chromosome
randomChromosome g x y = do
  len <- Random.uniformRM (x, y) g
  replicateM (fromIntegral len) (randomGene g)

chromosomeBits :: Chromosome -> [Bit]
chromosomeBits = concatMap (\(g1, g2, g3, g4) -> [g1, g2, g3, g4])

isOperator :: Char -> Bool
isOperator CharAdd = True
isOperator CharSub = True
isOperator CharMul = True
isOperator CharDiv = True
isOperator _ = False

isDigit :: Char -> Bool
isDigit Char0 = True
isDigit Char1 = True
isDigit Char2 = True
isDigit Char3 = True
isDigit Char4 = True
isDigit Char5 = True
isDigit Char6 = True
isDigit Char7 = True
isDigit Char8 = True
isDigit Char9 = True
isDigit _ = False

data Token = TokOperator | TokNumber
  deriving (Eq)
data Operator = OpAdd | OpSub | OpMul | OpDiv

evaluate :: [Char] -> Float
evaluate =
  (\(acc', _, _) -> acc')
  . foldl' f (0 :: Float, OpAdd, TokNumber)
  where
    f (acc, op, tok) x
      | tok == TokNumber && isDigit x =
        let
          nextOp = TokOperator
          n = (read . show $ x) :: Float
        in
          case op of
            OpAdd -> (acc + n, op, nextOp)
            OpSub -> (acc - n, op, nextOp)
            OpMul -> (acc * n, op, nextOp)
            -- For the purpose of the algorithm, make division by 0 result in a high number (not Infinity or NaN)
            OpDiv -> if n == 0 then (highNumber, op, nextOp) else (acc / n, op, nextOp)
      | tok == TokOperator && isOperator x =
        let nextOp = TokNumber
        in
          case x of
            CharAdd -> (acc, OpAdd, nextOp)
            CharSub -> (acc, OpAdd, nextOp)
            CharMul -> (acc, OpAdd, nextOp)
            CharDiv -> (acc, OpAdd, nextOp)
            _ -> (acc, op, tok)
      | otherwise = (acc, op, tok)

decodeChromosome :: Chromosome -> Float
decodeChromosome = evaluate . map decodeGene

fitness :: Chromosome -> Float -> Float
fitness chrom target
  | n == target = highNumber
  | otherwise = 1 / abs (target - n)
  where n = decodeChromosome chrom

main :: IO ()
main = do
  putStrLn "chrom1"
  print chrom1
  print $ decodeChromosome chrom1
  -- print $ fitness chrom1 42
  
  -- print $ evaluate chrom1

-- 6 + 5 * 4 / 2 + 1
-- = 23
chrom1 :: Chromosome
chrom1 =
  [ (Bit0,Bit1,Bit1,Bit0)
  , (Bit1,Bit0,Bit1,Bit0)
  , (Bit0,Bit1,Bit0,Bit1)
  , (Bit1,Bit1,Bit0,Bit0)
  , (Bit0,Bit1,Bit0,Bit0)
  , (Bit1,Bit1,Bit0,Bit1)
  , (Bit0,Bit0,Bit1,Bit0)
  , (Bit1,Bit0,Bit1,Bit0)
  , (Bit0,Bit0,Bit0,Bit1)
  ]
