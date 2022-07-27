// http://www.ai-junkie.com/ga/intro/gat3.html

const R = require('ramda')

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

// TYPES
// Chromosome :: [Bit]
// Gene :: (Bit, Bit, Bit, Bit)

// Char -> Gene
const charToGene = x => geneMap[x]

// Gene -> Char
const geneToChar = x => (
  Object.entries(geneMap)
    .find(([k, v]) => R.equals(x, v))[0]
)

// Chromosome -> [Gene]
const genes = x => R.splitEvery(4, x)

// Chromosome -> Integer
const decode = x => {
  const gs = genes(x)
}

// (Chromosome, Integer) -> Maybe Float
const fitness = (chrom, target) => {
  const x = decode(chrom)
  if (target - x === 0) return {nothing: true}
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
  decode,
  fitness
}
