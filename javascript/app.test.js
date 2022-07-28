const {
  geneMap,
  charToGene,
  geneToChar,
  genes,
  clean,
  show,
  decode,
  fitness
} = require('./app')

// 6 + 5 * 4 / 2 + 1
// = 23
const chromosome1 = [0,1,1,0,1,0,1,0,0,1,0,1,1,1,0,0,0,1,0,0,1,1,0,1,0,0,1,0,1,0,1,0,0,0,0,1]

// 2 2 + n/a - 7 2
// = 2 + 7
// = 9
const chromosome2 = [0,0,1,0,0,0,1,0,1,0,1,0,1,1,1,0,1,0,1,1,0,1,1,1,0,0,1,0]

// 7 * 3 * 2 +
// = 7 * 3 * 2
// = 42
const chromosome3 = [0,1,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,0,1,0,1,0]

test('converts 6 to gene [0,1,1,0]', () => {
  expect(charToGene('6')).toEqual([0,1,1,0])
})

test('converts gene to character', () => {
  expect(geneToChar([0,1,1,0])).toBe('6')
  expect(geneToChar([1,1,1,0])).toBe('n/a')
  expect(geneToChar([1,1,1,1])).toBe('n/a')
})

test('splits chromosome genes', () => {
  expect(genes([1,1,1,1,0,0,0,1])).toEqual([[1,1,1,1],[0,0,0,1]])
  expect(genes(chromosome1)).toEqual(
    [[0,1,1,0],[1,0,1,0],[0,1,0,1],[1,1,0,0],[0,1,0,0],[1,1,0,1],[0,0,1,0],[1,0,1,0],[0,0,0,1]]
  )
  expect(genes(chromosome2)).toEqual(
    [[0,0,1,0],[0,0,1,0],[1,0,1,0],[1,1,1,0],[1,0,1,1],[0,1,1,1],[0,0,1,0]]
  )
})

test('clean chromosome', () => {
  expect(clean(chromosome1)).toEqual(chromosome1)
  expect(clean(chromosome2)).toEqual(
    [0,0,1,0,1,0,1,0,0,1,1,1]
  )
})

test('show chromosome', () => {
  expect(show(chromosome1)).toBe('6 + 5 * 4 / 2 + 1')
  expect(show(chromosome2)).toBe('2 2 + n/a - 7 2')
  expect(show(chromosome3)).toBe('7 * 3 * 2 +')
})

test('show chromosome cleanly', () => {
  expect(show(clean(chromosome1))).toBe('6 + 5 * 4 / 2 + 1')
  expect(show(clean(chromosome2))).toBe('2 + 7')
  expect(show(clean(chromosome3))).toBe('7 * 3 * 2')
})

test('decode', () => {
  expect(decode(chromosome1)).toBe(23)
  expect(decode(chromosome2)).toBe(9)
  expect(decode(chromosome3)).toBe(42)
})

test('fitness', () => {
  expect(fitness(chromosome1, 42)).toEqual({
    nothing: false,
    val: 1/(42-23)
  })
  expect(fitness(chromosome2, 42)).toEqual({
    nothing: false,
    val: 1/(42-9)
  })
  expect(fitness(chromosome3, 42)).toEqual({
    nothing: true
  })
})
