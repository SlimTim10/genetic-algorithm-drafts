const R = require('ramda')

type Bit = 0 | 1
type Gene = [Bit, Bit, Bit, Bit]
type Chromosome = Bit[]
type Char = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '+' | '-' | '*' | '/'

function encodeGene(x: Char): Gene {
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
			: [0, 0, 0, 0]
	)
}

function decodeGene(x: Gene): Char {
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
			: '0'
	)
}

