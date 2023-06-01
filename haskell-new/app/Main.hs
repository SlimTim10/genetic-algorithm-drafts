module Main where

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified System.IO
-- monadic pseudo-random number generators
import qualified System.Random.Stateful as Random
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Loops as Loops
import Data.Function (on)

data Bit = Bit0 | Bit1
  deriving (Show, Eq)

type Gene = (Bit, Bit, Bit, Bit)

type Chromosome = [Gene]

data Organism = Organism
  { oChromosome :: Chromosome
  , oFitness :: Float
  }
  deriving (Show)

data CChar
  = CChar0
  | CChar1
  | CChar2
  | CChar3
  | CChar4
  | CChar5
  | CChar6
  | CChar7
  | CChar8
  | CChar9
  | CCharAdd
  | CCharSub
  | CCharMul
  | CCharDiv
  | CCharInvalid

instance Show CChar where
  show CChar0 = "0"
  show CChar1 = "1"
  show CChar2 = "2"
  show CChar3 = "3"
  show CChar4 = "4"
  show CChar5 = "5"
  show CChar6 = "6"
  show CChar7 = "7"
  show CChar8 = "8"
  show CChar9 = "9"
  show CCharAdd = "+"
  show CCharSub = "-"
  show CCharMul = "*"
  show CCharDiv = "/"
  show CCharInvalid = "n/a"

-- | How many bits are in a gene.
geneLength :: Int
geneLength = 4

-- TODO: find a better solution?
-- | Some absurdly high number to represent perfect fitness.
highNumber :: Float
highNumber = 10^(12 :: Integer)

encodeGene :: CChar -> Gene
encodeGene CChar0 = (Bit0, Bit0, Bit0, Bit0)
encodeGene CChar1 = (Bit0, Bit0, Bit0, Bit1)
encodeGene CChar2 = (Bit0, Bit0, Bit1, Bit0)
encodeGene CChar3 = (Bit0, Bit0, Bit1, Bit1)
encodeGene CChar4 = (Bit0, Bit1, Bit0, Bit0)
encodeGene CChar5 = (Bit0, Bit1, Bit0, Bit1)
encodeGene CChar6 = (Bit0, Bit1, Bit1, Bit0)
encodeGene CChar7 = (Bit0, Bit1, Bit1, Bit1)
encodeGene CChar8 = (Bit1, Bit0, Bit0, Bit0)
encodeGene CChar9 = (Bit1, Bit0, Bit0, Bit1)
encodeGene CCharAdd = (Bit1, Bit0, Bit1, Bit0)
encodeGene CCharSub = (Bit1, Bit0, Bit1, Bit1)
encodeGene CCharMul = (Bit1, Bit1, Bit0, Bit0)
encodeGene CCharDiv = (Bit1, Bit1, Bit0, Bit1)
encodeGene CCharInvalid = (Bit1, Bit1, Bit1, Bit1)

decodeGene :: Gene -> CChar
decodeGene (Bit0, Bit0, Bit0, Bit0) = CChar0
decodeGene (Bit0, Bit0, Bit0, Bit1) = CChar1
decodeGene (Bit0, Bit0, Bit1, Bit0) = CChar2
decodeGene (Bit0, Bit0, Bit1, Bit1) = CChar3
decodeGene (Bit0, Bit1, Bit0, Bit0) = CChar4
decodeGene (Bit0, Bit1, Bit0, Bit1) = CChar5
decodeGene (Bit0, Bit1, Bit1, Bit0) = CChar6
decodeGene (Bit0, Bit1, Bit1, Bit1) = CChar7
decodeGene (Bit1, Bit0, Bit0, Bit0) = CChar8
decodeGene (Bit1, Bit0, Bit0, Bit1) = CChar9
decodeGene (Bit1, Bit0, Bit1, Bit0) = CCharAdd
decodeGene (Bit1, Bit0, Bit1, Bit1) = CCharSub
decodeGene (Bit1, Bit1, Bit0, Bit0) = CCharMul
decodeGene (Bit1, Bit1, Bit0, Bit1) = CCharDiv
decodeGene _ = CCharInvalid

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
  -> Float -- ^ Fitness
calcFitness target chrom = 1 / (abs (target - n) + 1)
  where n = phenotype chrom

data Token = TokOperator | TokDigit
  deriving (Eq)
data Operator = OpAdd | OpSub | OpMul | OpDiv

isOperator :: CChar -> Bool
isOperator CCharAdd = True
isOperator CCharSub = True
isOperator CCharMul = True
isOperator CCharDiv = True
isOperator _ = False

isDigit :: CChar -> Bool
isDigit CChar0 = True
isDigit CChar1 = True
isDigit CChar2 = True
isDigit CChar3 = True
isDigit CChar4 = True
isDigit CChar5 = True
isDigit CChar6 = True
isDigit CChar7 = True
isDigit CChar8 = True
isDigit CChar9 = True
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
phenotype :: Chromosome -> Float
phenotype =
  evaluate
  . concat
  . map (show . decodeGene)
  . cleanChromosome

-- | Evaluate a math string, applying the operators as they appear from left-to-right.
evaluate :: String -> Float
evaluate expr = fst . List.foldl' evaluate' (0, (+)) $ expr
  where
    evaluate' :: (Float, (Float -> Float -> Float)) -> Char -> (Float, (Float -> Float -> Float))
    evaluate' (acc, _) '+' = (acc, (+))
    evaluate' (acc, _) '-' = (acc, (-))
    evaluate' (acc, _) '*' = (acc, (*))
    evaluate' (acc, _) '/' = (acc, (/))
    evaluate' (acc, op) x
      | x `elem` "0123456789" = (acc `op` read [x], op)
      | otherwise = (acc, op)

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

-- An organism is a chromosome along with its fitness
withFitness
  :: Float -- ^ Target
  -> Chromosome
  -> Organism
withFitness target x = Organism x (calcFitness target x)

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
  let initialPopulation :: [Organism] = map (withFitness target) initialChromosomes
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
      let o1' :: Organism = withFitness target c1'
      let o2' :: Organism = withFitness target c2'
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
  let p = phenotype (oChromosome best)
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
  let result = map evaluate xs
  print result
  putStrLn "Done"
