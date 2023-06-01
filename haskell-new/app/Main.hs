module Main where

import qualified Data.List as List
import qualified Data.List.Split as List.Split
import qualified System.IO
-- monadic pseudo-random number generators
import qualified System.Random.Stateful as Random
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Loops as Loops
import Data.Function (on)

data Bit = Bit0 | Bit1
  deriving (Show, Eq)

-- | There are two kinds of genes: one for numbers and one for operators.
data Gene
  = NumberGene (Bit, Bit, Bit, Bit)
  | OperatorGene (Bit, Bit)
  deriving (Show)

-- | How many bits are in a number gene.
numberGeneSize :: Int
numberGeneSize = 4

-- | How many bits are in an operator gene.
operatorGeneSize :: Int
operatorGeneSize = 2

-- | The alleles that genes can express.
data Allele
  = Allele0
  | Allele1
  | Allele2
  | Allele3
  | Allele4
  | Allele5
  | Allele6
  | Allele7
  | Allele8
  | Allele9
  | AlleleAdd
  | AlleleSub
  | AlleleMul
  | AlleleDiv
  | AlleleInvalid
  deriving (Eq)

instance Show Allele where
  show Allele0 = "0"
  show Allele1 = "1"
  show Allele2 = "2"
  show Allele3 = "3"
  show Allele4 = "4"
  show Allele5 = "5"
  show Allele6 = "6"
  show Allele7 = "7"
  show Allele8 = "8"
  show Allele9 = "9"
  show AlleleAdd = "+"
  show AlleleSub = "-"
  show AlleleMul = "*"
  show AlleleDiv = "/"
  show AlleleInvalid = "(junk)"

decodeGene :: Gene -> Allele
decodeGene (NumberGene (Bit0, Bit0, Bit0, Bit0)) = Allele0
decodeGene (NumberGene (Bit0, Bit0, Bit0, Bit1)) = Allele1
decodeGene (NumberGene (Bit0, Bit0, Bit1, Bit0)) = Allele2
decodeGene (NumberGene (Bit0, Bit0, Bit1, Bit1)) = Allele3
decodeGene (NumberGene (Bit0, Bit1, Bit0, Bit0)) = Allele4
decodeGene (NumberGene (Bit0, Bit1, Bit0, Bit1)) = Allele5
decodeGene (NumberGene (Bit0, Bit1, Bit1, Bit0)) = Allele6
decodeGene (NumberGene (Bit0, Bit1, Bit1, Bit1)) = Allele7
decodeGene (NumberGene (Bit1, Bit0, Bit0, Bit0)) = Allele8
decodeGene (NumberGene (Bit1, Bit0, Bit0, Bit1)) = Allele9
decodeGene (OperatorGene (Bit0, Bit0)) = AlleleAdd
decodeGene (OperatorGene (Bit0, Bit1)) = AlleleSub
decodeGene (OperatorGene (Bit1, Bit0)) = AlleleMul
decodeGene (OperatorGene (Bit1, Bit1)) = AlleleDiv
decodeGene _ = AlleleInvalid

encodeGene :: Allele -> Gene
encodeGene Allele0 = NumberGene (Bit0, Bit0, Bit0, Bit0)
encodeGene Allele1 = NumberGene (Bit0, Bit0, Bit0, Bit1)
encodeGene Allele2 = NumberGene (Bit0, Bit0, Bit1, Bit0)
encodeGene Allele3 = NumberGene (Bit0, Bit0, Bit1, Bit1)
encodeGene Allele4 = NumberGene (Bit0, Bit1, Bit0, Bit0)
encodeGene Allele5 = NumberGene (Bit0, Bit1, Bit0, Bit1)
encodeGene Allele6 = NumberGene (Bit0, Bit1, Bit1, Bit0)
encodeGene Allele7 = NumberGene (Bit0, Bit1, Bit1, Bit1)
encodeGene Allele8 = NumberGene (Bit1, Bit0, Bit0, Bit0)
encodeGene Allele9 = NumberGene (Bit1, Bit0, Bit0, Bit1)
encodeGene AlleleAdd = OperatorGene (Bit0, Bit0)
encodeGene AlleleSub = OperatorGene (Bit0, Bit1)
encodeGene AlleleMul = OperatorGene (Bit1, Bit0)
encodeGene AlleleDiv = OperatorGene (Bit1, Bit1)
encodeGene AlleleInvalid = NumberGene (Bit1, Bit1, Bit1, Bit1)

-- encodeGene . decodeGene = id
-- decodeGene . encodeGene = id

-- | A chromosome is a collection of genes.
type Chromosome = [Gene]

-- | An organism is a chromosome together with its fitness.
data Organism = Organism
  { oChromosome :: Chromosome
  , oFitness :: Float
  }
  deriving (Show)

-- | Some absurdly high number to circumvent division by 0.
highNumber :: Float
highNumber = 10^(12 :: Integer)

-- | Generate a random bit.
randomBit :: Random.RandomGenM rg r m => rg -> m Bit
randomBit rg = do
  x <- Random.randomRM (0 :: Int, 1 :: Int) rg
  case x of
    0 -> pure Bit0
    _ -> pure Bit1

data NumberOrOperator = Number | Operator

-- | Generate a random gene (number or operator).
randomGene
  :: (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> NumberOrOperator
  -> m Gene
randomGene rg Number = do
  [b1, b2, b3, b4] <- Monad.replicateM numberGeneSize . randomBit $ rg
  pure $ NumberGene (b1, b2, b3, b4)
randomGene rg Operator = do
  [b1, b2] <- Monad.replicateM operatorGeneSize . randomBit $ rg
  pure $ OperatorGene (b1, b2)

-- | Generate a random chromosome of a given length (number of genes). It will alternate number and operator genes, beginning with a number.
randomChromosome
  :: (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> Integer -- ^ Length in genes
  -> m Chromosome
randomChromosome rg len = do
  numberGenes <- Monad.replicateM ((ceiling :: Double -> Int) (fromIntegral len / 2)) (randomGene rg Number)
  operatorGenes <- Monad.replicateM (fromIntegral (len `div` 2)) (randomGene rg Operator)
  pure $
    List.foldl' (\acc (x, y) -> acc <> [x, y]) []
    $ zip numberGenes operatorGenes

-- | Calculate the fitness of a given chromosome with respect to a given target number.
calcFitness
  :: Float -- ^ Target
  -> Chromosome
  -> Float -- ^ Fitness
calcFitness target chrom = 1 / (abs (target - n) + 1)
  where n = phenotype chrom

-- | Get a cleaned version of a chromosome.
-- Remove any junk alleles (along with their preceeding operators).
-- e.g., 6 - (junk) * 3 + (junk) - 7 -> 6 * 3 - 7
cleanChromosome :: Chromosome -> Chromosome
cleanChromosome x =
  (\gs -> if length gs > 0 then tail gs else gs) -- Remove the initial plus
  . map encodeGene
  . List.foldl' (\acc (a, b) -> acc <> [a, b]) []
  . filter (\(_, numberAllele) -> numberAllele /= AlleleInvalid)
  . map (\[a, b] -> (a, b))
  . filter (\chunk -> length chunk == 2)
  $ List.Split.chunksOf 2 (AlleleAdd : alleles) -- Pairs of operators and numbers, with a plus added to the beginning
  where
    alleles :: [Allele]
    alleles = map decodeGene x

-- | Get the phenotype of a chromosome (its evaluated number).
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
      | x `elem` "0123456789" =
        if isNaN acc' || isInfinite acc'
        then (highNumber, op)
        else (acc', op)
      | otherwise = (acc, op)
      where
        acc' = acc `op` read [x]

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

-- | Given a pair of chromosomes and a crossover rate, return a pair of chromosomes that may be crossed over or the original pair.
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

-- | Given a chromosome and a mutation rate, return the chromosome with its bits potentially mutated.
mutate
  :: forall m rg r. (MonadFail m, Random.RandomGenM rg r m)
  => rg -- ^ Random generator
  -> Float -- ^ Mutation rate
  -> Chromosome -- ^ Chromosome to mutate
  -> m Chromosome
mutate rg mutationRate x = mapM mutateGene x
  where
    mutateGene :: Gene -> m Gene
    mutateGene (NumberGene (b1, b2, b3, b4)) = do
      [b1', b2', b3', b4'] <- mapM maybeFlipBit [b1, b2, b3, b4]
      pure $ NumberGene (b1', b2', b3', b4')
    mutateGene (OperatorGene (b1, b2)) = do
      [b1', b2'] <- mapM maybeFlipBit [b1, b2]
      pure $ OperatorGene (b1', b2')

    maybeFlipBit :: Bit -> m Bit
    maybeFlipBit bit = do
      r <- Random.uniformRM (0 :: Float, 1 :: Float) rg
      if r <= mutationRate
        then pure $ flipBit bit
        else pure bit

    flipBit :: Bit -> Bit
    flipBit Bit0 = Bit1
    flipBit Bit1 = Bit0

-- | An organism is a chromosome together with its fitness.
withFitness
  :: Float -- ^ Target
  -> Chromosome
  -> Organism
withFitness target x = Organism x (calcFitness target x)

-- | Run the genetic algorithm.
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
          newPop :: [Organism] <- concat <$> Monad.replicateM (popSize `div` 2) (produceOffspring pop)
          pure $ Just (newPop, (newPop, generation + 1))

    produceOffspring :: [Organism] -> m [Organism]
    produceOffspring pop = do
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

  let populationSize :: Int = 200
  let crossoverRate :: Float = 0.6
  let mutationRate :: Float = 0.05
  let generationLimit :: Int = 50
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
  [ encodeGene Allele6
  , encodeGene AlleleAdd
  , encodeGene Allele5
  , encodeGene AlleleMul
  , encodeGene Allele4
  , encodeGene AlleleDiv
  , encodeGene Allele2
  , encodeGene AlleleAdd
  , encodeGene Allele1
  ]

testEvaluateSpeed :: IO ()
testEvaluateSpeed = do
  putStrLn "Start"
  let xs = replicate 100 "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9"
  let result = map evaluate xs
  print result
  putStrLn "Done"
