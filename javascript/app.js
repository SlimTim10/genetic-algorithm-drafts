// http://www.ai-junkie.com/ga/intro/gat3.html

const R = require('ramda')

// TYPES
// Chromosome :: [Bit]
// Gene :: (Bit, Bit, Bit, Bit)
// GeneType :: Number | Operator

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
        .reduceRight(buildChrom, _ => [])(GeneType.number)
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
  return R.compose(R.splitEvery(2), R.tail)(gs)
    .reduce((acc, [g1, g2]) => {
      const op = geneToChar(g1)
      const n = Number(geneToChar(g2))
      return (
        op === '+' ? acc + n
          : op === '-' ? acc - n
          : op === '*' ? acc * n
          : op === '/' ? acc / n
          : acc
      )
    }, Number(geneToChar(R.head(gs))))
}

// (Chromosome, Integer) -> Maybe Float
const fitness = (chrom, target) => {
  const x = decode(chrom)
  if (target - x === 0) return {
    nothing: true
  }
  return {
    nothing: false,
    val: 1 / (target - x)
  }
}

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
