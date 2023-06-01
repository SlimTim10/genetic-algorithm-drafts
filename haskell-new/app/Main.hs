module Main where

import Prelude hiding (Char)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified System.IO
-- monadic pseudo-random number generators
import qualified System.Random.Stateful as Random
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Loops as Loops
import Data.Function (on)
import qualified Language.Haskell.Interpreter as Interpreter
import qualified Data.Either as Either

data Bit = Bit0 | Bit1
  deriving (Show, Eq)

type Gene = (Bit, Bit, Bit, Bit)

type Chromosome = [Gene]

data Organism = Organism
  { oChromosome :: Chromosome
  , oFitness :: Float
  }
  deriving (Show)

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

-- | How many bits are in a gene.
geneLength :: Int
geneLength = 4

-- TODO: find a better solution?
-- | Some absurdly high number to represent perfect fitness.
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

-- encodeGene . decodeGene = id
-- decodeGene . encodeGene = id

-- | Generate a random bit.
randomBit :: Random.RandomGenM rg r m => rg -> m Bit
randomBit rg = do
  x <- Random.randomRM (0 :: Int, 1 :: Int) rg
  case x of
    0 -> pure Bit0
    _ -> pure Bit1

-- | Generate a random gene.
randomGene
  :: (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> m Gene
randomGene rg = do
  [b1, b2, b3, b4] <- Monad.replicateM geneLength . randomBit $ rg
  pure (b1, b2, b3, b4)

-- | Generate a random chromosome of a given length (number of genes).
randomChromosome
  :: (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> Integer -- ^ Length, inclusive
  -> m Chromosome
randomChromosome rg len = do
  gs <- Monad.replicateM (fromIntegral len) (randomGene rg)
  pure gs

-- | Calculate the fitness of a given chromosome with respect to a given target number.
calcFitness
  :: Float -- ^ Target
  -> Chromosome
  -> IO Float -- ^ Fitness
calcFitness target chrom = do
  n <- phenotype chrom
  pure $ (1 / (abs (target - n) + 1))

data Token = TokOperator | TokDigit
  deriving (Eq)
data Operator = OpAdd | OpSub | OpMul | OpDiv

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

-- | Get a cleaned version of a chromosome.
-- e.g., 2 5 + - 3 -> 2 + 3
cleanChromosome :: Chromosome -> Chromosome
cleanChromosome = fst . List.foldl' step ([], TokDigit)
  where
    step :: (Chromosome, Token) -> Gene -> (Chromosome, Token)
    step (acc, tok) gene
      | tok == TokDigit && isDigit (decodeGene gene) =
        (acc <> [gene], TokOperator)
      | tok == TokOperator && isOperator (decodeGene gene) =
        (acc <> [gene], TokDigit)
      | otherwise = (acc, tok)

-- | Get the phenotype of a chromosome.
phenotype :: Chromosome -> IO Float
phenotype
  = evaluate
  . concat
  . map (show . decodeGene)
  . cleanChromosome

-- | Evaluate a math string.
evaluate :: String -> IO Float
evaluate expr = do
  result :: Either Interpreter.InterpreterError String <- Interpreter.runInterpreter $ do
    Interpreter.setImports ["Prelude"]
    Interpreter.eval expr
  pure $ read (Either.fromRight "0" result)

-- | Use the roulette selection tactic to pick one organism from a given population.
rouletteSelect
  :: (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> [Organism]
  -> m Organism
rouletteSelect rg population = do
  let fitnesses :: [Float] = map oFitness population
  let totalFitness :: Float = sum fitnesses
  let cumulFitnesses :: [Float] = scanl1 (+) fitnesses
  r :: Float <- Random.uniformRM (0, totalFitness) rg
  let mo :: Maybe Organism = fmap fst . List.find (\(_, cf) -> r <= cf) $ zip population cumulFitnesses
  pure $ case mo of
    Nothing -> last population
    Just o -> o

-- | Given a pair of chromosomes and a crossover rate, return a pair of chromosomes that may be the original pair or crossed over.
crossover
  :: (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> Float -- ^ Crossover rate
  -> (Chromosome, Chromosome)
  -> m (Chromosome, Chromosome)
crossover rg crossoverRate (x, y) = do
  r <- Random.uniformRM (0 :: Float, 1 :: Float) rg
  if r > crossoverRate
    then pure (x, y)
    else
    do
      n <- Random.uniformRM (1, min (length x) (length y) - 1) rg
      let (xStart, xEnd) = splitAt n x
      let (yStart, yEnd) = splitAt n y
      let xNew = xStart <> yEnd
      let yNew = yStart <> xEnd
      pure (xNew, yNew)

-- | Turn a chromosome into a flat list of bits.
bits :: Chromosome -> [Bit]
bits = concatMap (\(a, b, c, d) -> [a, b, c, d])

-- | Group a flat list of bits into genes.
genes :: [Bit] -> [Gene]
genes = map (\[a, b, c, d] -> (a, b, c, d)) . Split.chunksOf geneLength

-- | Given a chromosome and a mutation rate, return the chromosome with its bits potentially mutate.
mutate
  :: forall m rg r. (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> Float -- ^ Mutation rate
  -> Chromosome -- ^ Chromosome to mutate
  -> m Chromosome
mutate rg mutationRate x = do
  let bs = bits x
  bs' <- mapM maybeFlipBit bs
  pure $ genes bs'
  where
    maybeFlipBit :: Bit -> m Bit
    maybeFlipBit bit = do
      r <- Random.uniformRM (0 :: Float, 1 :: Float) rg
      if r <= mutationRate
        then pure $ flipBit bit
        else pure $ bit

    flipBit :: Bit -> Bit
    flipBit Bit0 = Bit1
    flipBit Bit1 = Bit0

chromosomeToOrganism
  :: Float -- ^ Target
  -> Chromosome
  -> IO Organism
chromosomeToOrganism target x = do
  fitness <- calcFitness target x
  pure $ Organism x fitness

run
  :: forall m rg r. (MonadFail m, Random.RandomGenM rg r m, IO.MonadIO m)
  => rg -- ^ Random generator
  -> Int -- ^ Population size
  -> Float -- ^ Crossover rate
  -> Float -- ^ Mutation rate
  -> Int -- ^ Generation limit
  -> Float -- ^ Target number
  -> m Organism
run rg popSize crossoverRate mutationRate generationLimit target = do
  let chromosomeLength = 20
  initialChromosomes :: [Chromosome] <- Monad.replicateM popSize (randomChromosome rg chromosomeLength)
  initialPopulation :: [Organism] <- IO.liftIO $ mapM (chromosomeToOrganism target) initialChromosomes
  finalPopulation :: [Organism] <- head <$> Loops.unfoldrM step (initialPopulation, 0)
  pure $ List.maximumBy (compare `on` oFitness) finalPopulation
  where
    step :: ([Organism], Int) -> m (Maybe ([Organism], ([Organism], Int)))
    step (pop, generation)
      | generation >= generationLimit = pure Nothing
      | otherwise = do
          IO.liftIO . putStrLn $ "Generation: " <> show generation
          -- IO.liftIO . putStr $ "producing offspring" -- DEBUG
          newPop :: [Organism] <- concat <$> Monad.replicateM (popSize `div` 2) (produceOffspring pop)
          -- IO.liftIO . putStrLn $ "" -- DEBUG
          pure $ Just (newPop, (newPop, generation + 1))

    produceOffspring :: [Organism] -> m [Organism]
    produceOffspring pop = do
      -- IO.liftIO . putStr $ "." -- DEBUG
      o1 :: Organism <- rouletteSelect rg pop
      o2 :: Organism <- rouletteSelect rg pop
      (c1, c2) :: (Chromosome, Chromosome) <- crossover rg crossoverRate (oChromosome o1, oChromosome o2)
      c1' :: Chromosome <- mutate rg mutationRate c1
      c2' :: Chromosome <- mutate rg mutationRate c2
      o1' :: Organism <- IO.liftIO $ chromosomeToOrganism target c1'
      o2' :: Organism <- IO.liftIO $ chromosomeToOrganism target c2'
      pure $ [o1', o2']

showChromosome :: Chromosome -> String
showChromosome = List.intercalate " " . map (show . decodeGene)

main :: IO ()
main = do
  System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering

  let populationSize :: Int = 20
  let crossoverRate :: Float = 0.6
  let mutationRate :: Float = 0.05
  let generationLimit :: Int = 20
  let target :: Float = 42
  
  putStrLn $ "Target: " <> show target

  -- Uncomment the following 2 lines to force fixed randomization (useful for testing).
  -- let rand = Random.mkStdGen 0
  -- Random.setStdGen rand

  best <- run
    Random.globalStdGen
    populationSize
    crossoverRate
    mutationRate
    generationLimit
    target

  putStrLn $ "Winner:"
  putStrLn $ showChromosome (oChromosome best)
  putStrLn $ "= " <> (showChromosome . cleanChromosome) (oChromosome best)
  p <- phenotype (oChromosome best)
  putStrLn $ "= " <> show p

-- | Example chromosome.
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

testEvaluateSpeed :: IO ()
testEvaluateSpeed = do
  putStrLn "Start"
  let xs = replicate 100 "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9"
  result <- mapM evaluate xs
  print result
  putStrLn "Done"
