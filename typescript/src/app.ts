const R = require('ramda')

type Bit = 0 | 1
type Gene = [Bit, Bit, Bit, Bit]
type Chromosome = Bit[]
type Char = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '+' | '-' | '*' | '/' | 'n/a'

const geneLength = 4

const encodeGene = (x: Char): Gene => {
	return (
		R.equals(x, '0') ? [0, 0, 0, 0]
			: R.equals(x, '1') ? [0, 0, 0, 1]
			: R.equals(x, '2') ? [0, 0, 1, 0]
			: R.equals(x, '3') ? [0, 0, 1, 1]
			: R.equals(x, '4') ? [0, 1, 0, 0]
			: R.equals(x, '5') ? [0, 1, 0, 1]
			: R.equals(x, '6') ? [0, 1, 1, 0]
			: R.equals(x, '7') ? [0, 1, 1, 1]
			: R.equals(x, '8') ? [1, 0, 0, 0]
			: R.equals(x, '9') ? [1, 0, 0, 1]
			: R.equals(x, '+') ? [1, 0, 1, 0]
			: R.equals(x, '-') ? [1, 0, 1, 1]
			: R.equals(x, '*') ? [1, 1, 0, 0]
			: R.equals(x, '/') ? [1, 1, 0, 1]
			: [1, 1, 1, 1]
	)
}

const decodeGene = (x: Gene): Char => {
	return (
		R.equals(x, [0, 0, 0, 0]) ? '0'
			: R.equals(x, [0, 0, 0, 1]) ? '1'
			: R.equals(x, [0, 0, 1, 0]) ? '2'
			: R.equals(x, [0, 0, 1, 1]) ? '3'
			: R.equals(x, [0, 1, 0, 0]) ? '4'
			: R.equals(x, [0, 1, 0, 1]) ? '5'
			: R.equals(x, [0, 1, 1, 0]) ? '6'
			: R.equals(x, [0, 1, 1, 1]) ? '7'
			: R.equals(x, [1, 0, 0, 0]) ? '8'
			: R.equals(x, [1, 0, 0, 1]) ? '9'
			: R.equals(x, [1, 0, 1, 0]) ? '+'
			: R.equals(x, [1, 0, 1, 1]) ? '-'
			: R.equals(x, [1, 1, 0, 0]) ? '*'
			: R.equals(x, [1, 1, 0, 1]) ? '/'
			: 'n/a'
	)
}

const randomBit = (): Bit =>
	(Math.random() * 2) < 1 ? 0 : 1

const randomGene = (): Gene =>
	[randomBit(), randomBit(), randomBit(), randomBit()]

// min <= result <= max
const getRandomInt = (min: number, max: number): number => {
	const x = Math.ceil(min)
	const y = Math.floor(max)
	return Math.floor(Math.random() * (y - x + 1) + x)
}

// min <= result < max
const getRandomNumber = (min: number, max: number): number => (
	Math.random() * (max - min) + min
)

const randomChromosome = (min: number, max: number): Chromosome => {
	const numGenes = getRandomInt(min, max)
	return R.compose(
		R.flatten,
		R.map(randomGene)
	)(R.range(0, numGenes))
}

export const genes = (x: Chromosome): Gene[] =>
	R.splitEvery(geneLength, x)

export const isNumber = (x: Char): boolean =>
	R.includes(x, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])

export const isOperator = (x: Char): boolean =>
	R.includes(x, ['+', '-', '*', '/'])

enum Token {
	Operator,
	Number,
}

enum Operator {
	Add,
	Subtract,
	Multiply,
	Divide,
}

// Evaluate a series of Chars
// e.g., ['1', '+', '1'] = 2
export const evaluate = (xs: Char[]): number => {	
	const f = ([acc, op, tok]: [number, Operator, Token], x: Char): [number, Operator, Token] => {
		if (R.equals(tok, Token.Number) && isNumber(x)) {
			return (
				op === Operator.Add ? [acc + Number(x), op, Token.Operator]
					: op === Operator.Subtract ? [acc - Number(x), op, Token.Operator]
					: op === Operator.Multiply ? [acc * Number(x), op, Token.Operator]
				// For the purpose of the algorithm, make division by 0 result in a high number (not Infinity or NaN)
					: (Number(x) === 0 ? [1e12, op, Token.Operator] : [acc / Number(x), op, Token.Operator])
			)
		} else if (R.equals(tok, Token.Operator) && isOperator(x)) {
			return (
				x === '+' ? [acc, Operator.Add, Token.Number]
					: x === '-' ? [acc, Operator.Subtract, Token.Number]
					: x === '*' ? [acc, Operator.Multiply, Token.Number]
					: [acc, Operator.Divide, Token.Number]
			)
		} else {
			return [acc, op, tok]
		}
	}
	
	return R.reduce(f, [0, Operator.Add, Token.Number], xs)[0]
}

export const decodeChromosome = (x: Chromosome): number => (
	R.compose(
		evaluate,
		R.map(decodeGene),
		genes
	)(x)
)

export const fitness = (x: Chromosome, target: number): number => {
	const n = decodeChromosome(x)
	return (
		n === target
			? 1e12
			: (1 / Math.abs(target - n))
	)
}

const rouletteSelect = (
	population: Chromosome[],
	target: number
): [Chromosome, Chromosome[]] => {
	const fitnesses: number[] = R.map((x: Chromosome) => fitness(x, target), population)
	const totalFitness: number = R.sum(fitnesses)
	const cumulFitnesses: number[] = R.tail(R.scan(R.add, 0, fitnesses))
	
	const r: number = getRandomNumber(0, totalFitness)
	const pickIdx: number = R.findIndex((f: number) => f >= r, cumulFitnesses)
	const pick: Chromosome = population[pickIdx]
	const newPopulation: Chromosome[] = R.remove(pickIdx, 1, population)

	return [pick, newPopulation]
}

const crossover = (
	crossoverRate: number,
	x: Chromosome,
	y: Chromosome
): [Chromosome, Chromosome] => {
	const r: number = getRandomNumber(0, 1)
	if (r <= crossoverRate) {
		const xg: Gene[] = genes(x)
		const yg: Gene[] = genes(y)
		const idx: number = getRandomInt(1, R.min(R.length(xg), R.length(yg)) - 1)
		const [xStart, xEnd]: [Gene[], Gene[]] = R.splitAt(idx, xg)
		const [yStart, yEnd]: [Gene[], Gene[]] = R.splitAt(idx, yg)
		const xNew: Chromosome = R.flatten([xStart, yEnd])
		const yNew: Chromosome = R.flatten([yStart, xEnd])
		return [xNew, yNew]
	} else {
		return [x, y]
	}
}

const mutate = (
	mutationRate: number,
	x: Chromosome
): Chromosome => {
	const flip = (b: Bit): Bit => b === 0 ? 1 : 0
	return R.map((b: Bit) => {
		const r: number = getRandomNumber(0, 1)
		if (r <= mutationRate) {
			return flip(b)
		} else {
			return b
		}
	}, x)
}

export const showChromosome = (x: Chromosome): string => R.compose(
	R.join(' '),
	R.map(decodeGene),
	genes
)(x)

export const showCleanChromosome = (x: Chromosome): string => {
	const cs: Char[] = R.compose(
		R.map(decodeGene),
		genes
	)(x)
	const f = ([acc, tok]: [Char[], Token], x: Char): [Char[], Token] => {
		if (R.equals(tok, Token.Number) && isNumber(x)) {
			return [[...acc, x], Token.Operator]
		} else if (R.equals(tok, Token.Operator) && isOperator(x)) {
			return [[...acc, x], Token.Number]
		} else {
			return [acc, tok]
		}
	}
	
	const cs_: Char[] = R.reduce(f, [[], Token.Number], cs)[0]
	return R.join(' ', cs_)
}

/*
At the beginning of a run of a genetic algorithm a large population of random chromosomes is created. Each one, when decoded will represent a different solution to the problem at hand. Let's say there are N chromosomes in the initial population. Then, the following steps are repeated until a solution is found
  1. Test each chromosome to see how good it is at solving the problem at hand and assign a fitness score accordingly. The fitness score is a measure of how good that chromosome is at solving the problem to hand.
  2. Select two members from the current population. The chance of being selected is proportional to the chromosomes fitness. Roulette wheel selection is a commonly used method.
  3. Dependent on the crossover rate crossover the bits from each chosen chromosome at a randomly chosen point.
  4. Step through the chosen chromosomes bits and flip dependent on the mutation rate.
  5. Repeat step 2, 3, 4 until a new population of N members has been created.
*/
const run = (
	populationSize: number,
	maxSteps: number,
	target: number,
	crossoverRate: number,
	mutationRate: number
): void => {
	const initialPopulation: Chromosome[] = R.times((_: number) => randomChromosome(1, 20), populationSize)

	const step = (population: Chromosome[], n: number): Chromosome[] => {
		console.log('step:', n)
		if (n >= maxSteps) return population

		const newPopulation: Chromosome[] = R.unnest(R.unfold((pop: Chromosome[]) => {
			if (R.isEmpty(pop)) return false
			
			// Choose 2 chromosomes using roulette wheel
			const [chrom1, pop_]: [Chromosome, Chromosome[]] = rouletteSelect(pop, target)
			const [chrom2, pop__]: [Chromosome, Chromosome[]] = rouletteSelect(pop_, target)

			// Apply crossover
			const [chrom1C, chrom2C]: [Chromosome, Chromosome] = crossover(crossoverRate, chrom1, chrom2)
			
			// Apply mutation
			const chrom1M: Chromosome = mutate(mutationRate, chrom1C)
			const chrom2M: Chromosome = mutate(mutationRate, chrom2C)

			return [[chrom1M, chrom2M], pop__]
		}, population))

		return step(newPopulation, n + 1)
	}

	const finalPopulation: Chromosome[] = step(initialPopulation, 0)
	const best: Chromosome = R.reduce(R.maxBy((x: Chromosome) => fitness(x, target)), 0, finalPopulation)

	console.log('best:', best)
	console.log(showChromosome(best))
	console.log('=', showCleanChromosome(best))
	console.log('=', decodeChromosome(best))
	console.log('fitness:', fitness(best, target))
}

run(100, 20, 42, 0.7, 0.001)

// // 6 + 5 * 4 / 2 + 1
// // = 23
// const chrom1: Bit[] = [
// 	0,1,1,0,
// 	1,0,1,0,
// 	0,1,0,1,
// 	1,1,0,0,
// 	0,1,0,0,
// 	1,1,0,1,
// 	0,0,1,0,
// 	1,0,1,0,
// 	0,0,0,1
// ]
// console.log('chrom1')
// console.log('decoded:', decodeChromosome(chrom1))
// console.log('fitness:', fitness(chrom1, 42))

// //  2 2 + n/a - 7 2
// // = 2 + 7
// // = 9
// const chrom2: Bit[] = [
// 	0,0,1,0,
// 	0,0,1,0,
// 	1,0,1,0,
// 	1,1,1,0,
// 	1,0,1,1,
// 	0,1,1,1,
// 	0,0,1,0
// ]
// console.log('chrom2')
// console.log('decoded:', decodeChromosome(chrom2))
// console.log('fitness:', fitness(chrom2, 42))

// // 1 / 0
// // = 1e12
// const chrom3: Bit[] = [
// 	0,0,0,1,
// 	1,1,0,1,
// 	0,0,0,0
// ]
// console.log('chrom3')
// console.log('decoded:', decodeChromosome(chrom3))
// console.log('fitness:', fitness(chrom3, 42))

// // 6 * 7
// // = 42
// const chrom4: Bit[] = [
// 	0,1,1,0,
// 	1,1,0,0,
// 	0,1,1,1
// ]
// console.log('chrom4')
// console.log('decoded:', decodeChromosome(chrom4))
// console.log('fitness:', fitness(chrom4, 42))
