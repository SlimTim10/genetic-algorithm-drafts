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

// Inclusive
const getRandomInt = (min: number, max: number): number => {
  const x = Math.ceil(min)
  const y = Math.floor(max)
  return Math.floor(Math.random() * (y - x + 1) + x)
}


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

// Evaluate a decoded chromosome
// e.g., ['1', '+', '1'] = 2
export const evaluate = (xs: Char[]): number => {
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

// 6 + 5 * 4 / 2 + 1
// = 23
const chrom1: Bit[] = [
	0,1,1,0,
	1,0,1,0,
	0,1,0,1,
	1,1,0,0,
	0,1,0,0,
	1,1,0,1,
	0,0,1,0,
	1,0,1,0,
	0,0,0,1
]
console.log(decodeChromosome(chrom1))

//  2 2 + n/a - 7 2
// = 2 + 7
// = 9
const chrom2: Bit[] = [
	0,0,1,0,
	0,0,1,0,
	1,0,1,0,
	1,1,1,0,
	1,0,1,1,
	0,1,1,1,
	0,0,1,0
]
console.log(decodeChromosome(chrom2))

// 1 / 0
// = 1e12
const chrom3: Bit[] = [
	0,0,0,1,
	1,1,0,1,
	0,0,0,0
]
console.log(decodeChromosome(chrom3))
