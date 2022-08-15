module Main where

import Prelude hiding (Char)
import Control.Monad (replicateM)
import Data.List (foldl', maximumBy, intercalate)
import Data.Function (on)
import qualified Control.Monad.IO.Class as IO
import qualified Data.List.Split as Split
import qualified System.Random.Stateful as Random
import qualified Control.Monad.Loops as CML

data Bit = Bit0 | Bit1
  deriving (Show)
type Gene = (Bit, Bit, Bit, Bit)
data Chromosome = Chromosome
  { genes :: [Gene]
  , fitness :: Float
  , target :: Float
  }
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
  -> Integer -- ^ Minimum length, inclusive
  -> Integer -- ^ Maximum length, inclusive
  -> Float -- ^ Target number
  -> m Chromosome
randomChromosome g x y trg = do
  len <- Random.uniformRM (x, y) g
  gs <- replicateM (fromIntegral len) (randomGene g)
  return Chromosome
    { genes = gs
    , fitness = calcFitness trg gs
    , target = trg
    }

chromosomeBits :: Chromosome -> [Bit]
chromosomeBits = concatMap (\(g1, g2, g3, g4) -> [g1, g2, g3, g4]) . genes

bitsToChromosome
  :: Float -- ^ Target
  -> [Bit] -- ^ Bits
  -> Chromosome
bitsToChromosome trg bs = Chromosome
  { genes = gs
  , fitness = calcFitness trg gs
  , target = trg
  }
  where
    f [a, b, c, d] = (a, b, c, d)
    f _ = error "Invalid gene length"
    gs = map f . Split.chunksOf geneLength $ bs

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

data Token = TokOperator | TokDigit
  deriving (Eq)
data Operator = OpAdd | OpSub | OpMul | OpDiv

evaluate :: [Char] -> Float
evaluate =
  (\(acc', _, _) -> acc')
  . foldl' f (0 :: Float, OpAdd, TokDigit)
  where
    f (acc, op, tok) x
      | tok == TokDigit && isDigit x =
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
        let nextOp = TokDigit
        in
          case x of
            CharAdd -> (acc, OpAdd, nextOp)
            CharSub -> (acc, OpSub, nextOp)
            CharMul -> (acc, OpMul, nextOp)
            CharDiv -> (acc, OpDiv, nextOp)
            _ -> (acc, op, tok)
      | otherwise = (acc, op, tok)

decodeGenes :: [Gene] -> Float
decodeGenes = evaluate . map decodeGene

calcFitness
  :: Float -- ^ Target
  -> [Gene] -- ^ Genes
  -> Float -- ^ Fitness
calcFitness trg gs
  | n == trg = highNumber
  | otherwise = 1 / abs (trg - n)
  where n = decodeGenes gs

rouletteSelect
  :: (MonadFail m, Random.RandomGenM g r m)
  => g
  -> [Chromosome]
  -> m (Chromosome, [Chromosome])
rouletteSelect g population = do
  r <- Random.uniformRM (0, totalFitness) g
  let (xs, ys) = break (\(_, cf) -> cf >= r) $ zip population cumulFitnesses
  let newPopulation = map fst xs <> map fst (tail ys)
  pure (fst . head $ ys, newPopulation)
  where
    fitnesses = map fitness population
    totalFitness = sum fitnesses
    cumulFitnesses = scanl1 (+) fitnesses

crossover
  :: (MonadFail m, Random.RandomGenM g r m)
  => g
  -> Float -- ^ Crossover rate
  -> Chromosome -- ^ First chromosome
  -> Chromosome -- ^ Second chromosome
  -> m (Chromosome, Chromosome)
crossover g crossoverRate x y = do
  r <- Random.uniformRM (0 :: Float, 1 :: Float) g
  if r <= crossoverRate
    then
    do
      n <- Random.uniformRM (1, min (length . genes $ x) (length . genes $ y) - 1) g
      let (xStart, xEnd) = splitAt n (genes x)
      let (yStart, yEnd) = splitAt n (genes y)
      let xNew = xStart <> yEnd
      let yNew = yStart <> xEnd
      pure
        ( Chromosome xNew (calcFitness (target x) xNew) (target x)
        , Chromosome yNew (calcFitness (target y) yNew) (target y)
        )
    else pure (x, y)

mutate
  :: (MonadFail m, Random.RandomGenM g r m)
  => g
  -> Float -- ^ Mutation rate
  -> Chromosome -- ^ Chromosome to mutate
  -> m Chromosome
mutate g mutationRate x = do
  ys <- mapM f (chromosomeBits x)
  pure $ bitsToChromosome (target x) ys
  where
    f b = do
      r <- Random.uniformRM (0 :: Float, 1 :: Float) g
      if r <= mutationRate
        then pure $ flipBit b
        else pure $ b
    flipBit Bit0 = Bit1
    flipBit Bit1 = Bit0

{-
At the beginning of a run of a genetic algorithm a large population of random chromosomes is created. Each one, when decoded will represent a different solution to the problem at hand. Let's say there are N chromosomes in the initial population. Then, the following steps are repeated until a solution is found
  1. Test each chromosome to see how good it is at solving the problem at hand and assign a fitness score accordingly. The fitness score is a measure of how good that chromosome is at solving the problem to hand.
  2. Select two members from the current population. The chance of being selected is proportional to the chromosomes fitness. Roulette wheel selection is a commonly used method.
  3. Dependent on the crossover rate crossover the bits from each chosen chromosome at a randomly chosen point.
  4. Step through the chosen chromosomes bits and flip dependent on the mutation rate.
  5. Repeat step 2, 3, 4 until a new population of N members has been created.
-}
run
  :: (MonadFail m, Random.RandomGenM g r m, IO.MonadIO m)
  => g
  -> Int -- ^ Population size
  -> Int -- ^ Maximum steps
  -> Float -- ^ Target number
  -> Float -- ^ Crossover rate
  -> Float -- ^ Mutation rate
  -> m Chromosome
run g popSize maxSteps trg cr mr = do
  initialPopulation <- replicateM popSize (randomChromosome g 1 40 trg)
  finalPopulation <- fmap head $ CML.unfoldrM step (initialPopulation, 0)
  pure $ maximumBy (compare `on` fitness) finalPopulation
  where
    step (pop, n)
      | n >= maxSteps = pure Nothing
      | otherwise = do
          IO.liftIO . putStrLn $ "step: " <> show n
          newPop <- fmap concat $ CML.unfoldrM addToPop pop
          pure $ Just (newPop, (newPop, n + 1))
    addToPop pop
      | length pop < 2 = pure Nothing
      | otherwise = do
          (c1, pop') <- rouletteSelect g pop
          (c2, pop'') <- rouletteSelect g pop'
          (c1', c2') <- crossover g cr c1 c2
          c1'' <- mutate g mr c1'
          c2'' <- mutate g mr c2'
          pure $ Just ([c1'', c2''], pop'')

showChromosome :: Chromosome -> String
showChromosome = intercalate " " . map (show . decodeGene) . genes

showCleanChromosome :: Chromosome -> String
showCleanChromosome =
  intercalate " "
  . map show
  . (\xs -> if isOperator (last xs) then init xs else xs)
  . fst
  . foldl' f ([], TokDigit)
  . genes
  where
    f :: ([Char], Token) -> Gene -> ([Char], Token)
    f (acc, tok) gene
      | tok == TokDigit && isDigit x =
        (acc <> [x], TokOperator)
      | tok == TokOperator && isOperator x =
        (acc <> [x], TokDigit)
      | otherwise =
        (acc, tok)
      where x = decodeGene gene

main :: IO ()
main = do
  putStrLn "chrom1"
  putStrLn $ showChromosome chrom1
  putStrLn $ "= " <> show (decodeGenes . genes $ chrom1)
  print $ fitness chrom1
  putStrLn ""

  let trg = 42
  putStrLn $ "target: " <> show trg
  best <- run Random.globalStdGen 500 20 trg 0.7 0.001
  putStrLn $ "best:"
  putStrLn $ showChromosome best
  putStrLn $ "= " <> showCleanChromosome best
  putStrLn $ "= " <> show (decodeGenes . genes $ best)
  putStrLn $ "fitness: " <> show (fitness best)

-- 6 + 5 * 4 / 2 + 1
-- = 23
chrom1 :: Chromosome
chrom1 = Chromosome
  { genes =
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
  , fitness = 0
  , target = 42
  }
