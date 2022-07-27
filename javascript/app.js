// http://www.ai-junkie.com/ga/intro/gat3.html

const eqArrays = (xs, ys) => (xs.length === ys.length) && xs.every((_, i) => {
  if (Array.isArray(xs[i]) && Array.isArray(ys[i])) {
    return eqArrays(xs[i], ys[i]);
  } else {
    return ys[i] === xs[i];
  }
});

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

// Char -> Gene
const charToGene = x => geneMap[x]

// Gene -> Char
const geneToChar = x => (
  Object.entries(geneMap)
    .find(([k, v]) => eqArrays(x, v))[0]
)

