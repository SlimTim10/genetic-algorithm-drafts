// http://www.ai-junkie.com/ga/intro/gat3.html

const R = require('ramda')

// TYPES
// Chromosome :: [Bit]
// Gene :: (Bit, Bit, Bit, Bit)
// GeneType :: Number | Operator

const highNumber = Math.pow(10, 12)

const geneMap = {
  '0': [0,0,0,0],
  '1': [0,0,0,1],
  '2': [0,0,1,0],
  '3': [0,0,1,1],
  '4': [0,1,0,0],
  '5': [0,1,0,1],
  '6': [0,1,1,0],
  '7': [0,1,1,1],
  '8': [1,0,0,0],
  '9': [1,0,0,1],
  '+': [1,0,1,0],
  '-': [1,0,1,1],
  '*': [1,1,0,0],
  '/': [1,1,0,1]
}

const GeneType = {
  number: 'number',
  operator: 'operator'
}
const numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
const operators = ['+', '-', '*', '/']

// Char -> Gene
const charToGene = x => geneMap[x]

// Gene -> Char
const geneToChar = x => {
  const y = Object.entries(geneMap)
        .find(([_, v]) => R.equals(x, v))
  return y ? y[0] : 'n/a'
}

// Chromosome -> [Gene]
const genes = x => R.splitEvery(4, x)

// Chromosome -> Chromosome
const clean = x => {
  const buildChrom = (next, head) => geneType => {
    const c = geneToChar(head)
    return (
      geneType === GeneType.number ? (
        numbers.includes(c)
          ? [head, ...next(GeneType.operator)].flat()
          : next(geneType)
      ) : geneType === GeneType.operator ? (
        operators.includes(c)
          ? [head, ...next(GeneType.number)].flat()
          : next(geneType)
      ) : next(geneType)
    )
  }
  
  const cleanedChrom = genes(x)
        .reduceRight(buildChrom, () => [])(GeneType.number)
  const gs = genes(cleanedChrom)
  return (!numbers.includes(geneToChar(R.last(gs))))
    ? R.init(gs).flat()
    : cleanedChrom
}

// Chromosome -> String
const show = x => genes(x).map(geneToChar).join(' ')

// Chromosome -> Integer
const decode = x => {
  const gs = genes(clean(x))
  if (R.isEmpty(gs)) return 0
  return R.compose(R.splitEvery(2), R.tail)(gs)
    .reduce((acc, [g1, g2]) => {
      const op = geneToChar(g1)
      const n = Number(geneToChar(g2))
      return (
        op === '+' ? acc + n
          : op === '-' ? acc - n
          : op === '*' ? acc * n
        // For the purpose of the algorithm, make division by 0 result in a high number (not Infinity or NaN)
          : op === '/' ? (n === 0 ? highNumber : acc / n)
          : acc
      )
    }, Number(geneToChar(R.head(gs))))
}

// (Number, Chromosome) -> Number
const fitness = (target, chrom) => {
  const n = R.compose(decode, clean)(chrom)
  return (
    n === target ? highNumber
      : 1 / Math.abs(target - n)
  )
}

// Return a random integer from x to y (inclusive)
const getRandomInt = (x, y) => (
  Math.floor(Math.random() * (y - x + 1) + x)
)

const makeGene = () => R.times(
  () => getRandomInt(0, 1),
  R.compose(R.length, R.head, Object.values)(geneMap)
)

const makeChromosome = () => {
  const r = getRandomInt(1, 40)
  const gs = R.times(makeGene, r)
  return gs.flat()
}

// 
// [Chromosome] -> (Chromosome, [Chromosome])
const rouletteSelect = (target, population) => {
  const fitnesses = R.map(R.curry(fitness)(target), population)
  const totalFitness = R.sum(fitnesses)
  const cumulFitnesses = R.tail(R.scan(R.add, 0, fitnesses))

  const r = Math.random() * totalFitness
  const [xs, ys] = R.splitWhen(
    ([_, cf]) => cf >= r,
    R.zip(population, cumulFitnesses)
  )
  const newPopulation = [...R.map(R.head, xs), ...R.map(R.head, R.tail(ys))]
  const chosen = R.head(R.head(ys))
  return [chosen, newPopulation]
}

// Float -> Chromosome -> Chromosome -> (Chromosome, Chromosome)
const crossover = (crossoverRate, x, y) => {
  const r = Math.random() * crossoverRate
  if (r <= crossoverRate) {
    const n = getRandomInt(1, Math.min(x.length, y.length) - 1)
    const [xStart, xEnd] = R.splitAt(n, x)
    const [yStart, yEnd] = R.splitAt(n, y)
    const xNew = [...xStart, ...yEnd]
    const yNew = [...yStart, ...xEnd]
    return (xNew, yNew)
  } else {
    return (x, y)
  }
}

// Float -> Chromosome -> Chromosome
const mutate = (mutationRate, x) => {
  // TODO
  return x
}

/*
At the beginning of a run of a genetic algorithm a large population of random chromosomes is created. Each one, when decoded will represent a different solution to the problem at hand. Let's say there are N chromosomes in the initial population. Then, the following steps are repeated until a solution is found
  1. Test each chromosome to see how good it is at solving the problem at hand and assign a fitness score accordingly. The fitness score is a measure of how good that chromosome is at solving the problem to hand.
  2. Select two members from the current population. The chance of being selected is proportional to the chromosomes fitness. Roulette wheel selection is a commonly used method.
  3. Dependent on the crossover rate crossover the bits from each chosen chromosome at a randomly chosen point.
  4. Step through the chosen chromosomes bits and flip dependent on the mutation rate.
  5. Repeat step 2, 3, 4 until a new population of N members has been created.
*/
const run = (populationSize, maxSteps, target, crossoverRate, mutationRate) => {
  const initialPopulation = R.times(makeChromosome, populationSize)

  const step = (population, n) => {
    console.log('step:', n)
    if (n > maxSteps) return population

    const newPopulation = R.flatten(R.unfold(pop => {
      if (R.isEmpty(pop)) return false
      // Choose 2 chromosomes using roulette wheel
      const [chrom1, pop_] = rouletteSelect(target, pop)
      const [chrom2, pop__] = rouletteSelect(target, pop_)
      // Apply crossover
      const [chrom1C, chrom2C] = crossover(crossoverRate, chrom1, chrom2)
      // Apply mutation
      const chrom1M = mutate(mutationRate, chrom1C)
      const chrom2M = mutate(mutationRate, chrom2C)
      return [[chrom1M, chrom2M], pop__]
    }, population))
    
    return step(newPopulation, n + 1)
  }
  // const finalPopulation = R.unfold(step, initialPopulation)
  // const finalPopulation = R.reduceRight(step, _ => [], R.range(0, maxSteps))(initialPopulation)
  const finalPopulation = step(initialPopulation, 0)
  
  const best = R.compose(
    R.head,
    R.reverse,
    R.sortBy(R.curry(fitness)(target))
  )(finalPopulation)
  
  console.log('BEST')
  console.log('chromosome:', best)
  console.log(show(best))
  console.log('=', R.compose(show, clean)(best))
  console.log('=', decode(best))
  console.log('fitness:', fitness(target, best))
}

run(10, 10, 42, 0.7, 0.001)

module.exports = {
  geneMap,
  charToGene,
  geneToChar,
  genes,
  clean,
  show,
  decode,
  fitness
}
