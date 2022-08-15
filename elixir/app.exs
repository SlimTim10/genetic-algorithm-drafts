defmodule App do

  @geneLength 4
  @highNumber :math.pow(10, 12)

  def encodeGene(char) do
	map = %{
	  "0" => [0, 0, 0, 0],
	  "1" => [0, 0, 0, 1],
	  "2" => [0, 0, 1, 0],
	  "3" => [0, 0, 1, 1],
	  "4" => [0, 1, 0, 0],
	  "5" => [0, 1, 0, 1],
	  "6" => [0, 1, 1, 0],
	  "7" => [0, 1, 1, 1],
	  "8" => [1, 0, 0, 0],
	  "9" => [1, 0, 0, 1],
	  "+" => [1, 0, 1, 0],
	  "-" => [1, 0, 1, 1],
	  "*" => [1, 1, 0, 0],
	  "/" => [1, 1, 0, 1],
	}
	map[char]
  end

  def decodeGene(gene) do
	map = %{
	  [0, 0, 0, 0] => "0",
	  [0, 0, 0, 1] => "1",
	  [0, 0, 1, 0] => "2",
	  [0, 0, 1, 1] => "3",
	  [0, 1, 0, 0] => "4",
	  [0, 1, 0, 1] => "5",
	  [0, 1, 1, 0] => "6",
	  [0, 1, 1, 1] => "7",
	  [1, 0, 0, 0] => "8",
	  [1, 0, 0, 1] => "9",
	  [1, 0, 1, 0] => "+",
	  [1, 0, 1, 1] => "-",
	  [1, 1, 0, 0] => "*",
	  [1, 1, 0, 1] => "/",
	  [1, 1, 1, 0] => "n/a",
	  [1, 1, 1, 1] => "n/a"
	}
	map[gene]
  end

  def randomBit do
	Enum.random([0, 1])
  end

  def randomGene do
	Enum.map(Enum.to_list(1..@geneLength), fn _ -> randomBit() end)
  end

  def randomChromosome(min, max, target) do
	numGenes = Enum.random(min..max)
	genes = Enum.to_list(0..numGenes)
	|> Enum.map(fn _ -> randomGene() end)
	%{
	  :genes => genes,
	  :fitness => calcFitness(genes, target),
	  :target => target
	}
  end

  def isNumber(char) do
	Enum.member?(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"], char)
  end

  def isOperator(char) do
	Enum.member?(["+", "-", "*", "/"], char)
  end

  def evaluate(xs) do
	f = fn (char, {acc, op, tok}) ->
	  cond do
		tok == :number && isNumber(char) ->
		  n = elem(Integer.parse(char), 0)
		  cond do
			op == :add -> {acc + n, op, :operator}
			op == :subtract -> {acc - n, op, :operator}
			op == :multiply -> {acc * n, op, :operator}
			op == :divide ->
			  # For the purpose of the algorithm, make division by 0 result in a high number (not Infinity or NaN)
			  if n == 0, do: {@highNumber, op, :operator}, else: {acc / n, op, :operator}
		  end
		tok == :operator && isOperator(char) ->
		  cond do
			char == "+" -> {acc, :add, :number}
			char == "-" -> {acc, :subtract, :number}
			char == "*" -> {acc, :multiply, :number}
			char == "/" -> {acc, :divide, :number}
		  end
		true ->
		  {acc, op, tok}
	  end
	end

	Enum.reduce(xs, {0, :add, :number}, f)
	|> elem(0)
  end

  def decodeGenes(genes) do
	genes
	|> Enum.map(fn gene -> decodeGene(gene) end)
	|> evaluate()
  end

  def calcFitness(genes, target) do
	n = decodeGenes(genes)
	if n == target do
	  @highNumber
	else
	  1 / Kernel.abs(target - n)
	end
  end

  # min <= result < max
  def getRandomNumber(min, max) do
	:rand.uniform() * (max - min) + min
  end

  def rouletteSelect(population) do
	fitnesses = Enum.map(population, fn chrom -> chrom.fitness end)
	totalFitness = Enum.sum(fitnesses)
	cumulFitnesses = fitnesses |> Kernel.tl() |> Enum.scan(0, &(&1 + &2))
	r = getRandomNumber(0, totalFitness)
	idx = Enum.find_index(cumulFitnesses, fn f -> f >= r end)
	List.pop_at(population, (if idx == nil, do: 0, else: idx))
  end

  def crossover(crossoverRate, x, y) do
	r = getRandomNumber(0, 1)
	cond do
	  r <= crossoverRate ->
		idx = Enum.random(1..(Kernel.min(length(x.genes), length(y.genes)) - 1))
		{xStart, xEnd} = Enum.split(x.genes, idx)
		{yStart, yEnd} = Enum.split(y.genes, idx)
		xNew = flatten1([xStart, yEnd])
		yNew = flatten1([yStart, xEnd])
		{
		  %{
			:genes => xNew,
			:fitness => calcFitness(xNew, x.target),
			:target => x.target
		  },
		  %{
			:genes => yNew,
			:fitness => calcFitness(yNew, y.target),
			:target => y.target
		  }
		}
	  true ->
		{x, y}
	end
  end

  def mutateGene(mutationRate, gene) do
	flip = fn bit -> if bit == 0, do: 1, else: 0 end
	Enum.map(gene, fn bit ->
	  r = getRandomNumber(0, 1)
	  if r <= mutationRate, do: flip.(bit), else: bit
	end)
  end

  def mutate(mutationRate, chrom) do
	newGenes = Enum.map(chrom.genes, fn gene -> mutateGene(mutationRate, gene) end)
	%{
	  :genes => newGenes,
	  :fitness => calcFitness(newGenes, chrom.target),
	  :target => chrom.target
	}
  end

  # [[1, 2, 3], [4, 5, 6]] -> [1, 2, 3, 4, 5, 6]
  def flatten1(nestedList) do
	Enum.reduce(nestedList, [], fn x, acc -> acc ++ x end)
  end

  # At the beginning of a run of a genetic algorithm a large population of random chromosomes is created. Each one, when decoded will represent a different solution to the problem at hand. Let's say there are N chromosomes in the initial population. Then, the following steps are repeated until a solution is found
  #   1. Test each chromosome to see how good it is at solving the problem at hand and assign a fitness score accordingly. The fitness score is a measure of how good that chromosome is at solving the problem to hand.
  #   2. Select two members from the current population. The chance of being selected is proportional to the chromosomes fitness. Roulette wheel selection is a commonly used method.
  #   3. Dependent on the crossover rate crossover the bits from each chosen chromosome at a randomly chosen point.
  #   4. Step through the chosen chromosomes bits and flip dependent on the mutation rate.
  #   5. Repeat step 2, 3, 4 until a new population of N members has been created.
  def run(populationSize, maxSteps, target, crossoverRate, mutationRate) do
	initialPopulation = Enum.map(
	  1..populationSize,
	  fn _ -> randomChromosome(1, 20, target) end)

	step = fn
	  _f, population, n when n >= maxSteps -> population
	  f, population, n ->
		IO.puts "step: " <> inspect(n)
		addToPop = fn
		  pop when length(pop) < 2 -> nil
		  pop ->
			{chrom1, pop_} = rouletteSelect(pop)
			{chrom2, pop__} = rouletteSelect(pop_)
			{chrom1C, chrom2C} = crossover(crossoverRate, chrom1, chrom2)
			{chrom1M, chrom2M} = {mutate(mutationRate, chrom1C), mutate(mutationRate, chrom2C)}
			{[chrom1M, chrom2M], pop__}
		end
		newPopulation =
		  Stream.unfold(population, addToPop) |> Enum.to_list() |> flatten1()
		f.(f, newPopulation, n + 1)
	end
	
	finalPopulation = step.(step, initialPopulation, 0)
	best = Enum.max_by(finalPopulation, fn chrom -> chrom.fitness end)
	
	IO.puts "best: " <> showChromosome(best)
	IO.puts "= " <> showCleanChromosome(best)
	IO.puts "= " <> inspect(decodeGenes(best.genes))
	IO.puts "fitness: " <> inspect(best.fitness)
  end

  def showChromosome(chrom) do
	chrom.genes
	|> Enum.map(fn gene -> decodeGene(gene) end)
	|> Enum.join(" ")
  end

  def showCleanChromosome(chrom) do
	chars = Enum.map(chrom.genes, fn gene -> decodeGene(gene) end)
	f = fn char, {acc, tok} ->
	  cond do
		tok == :number && isNumber(char) ->
		  {acc ++ [char], :operator}
		tok == :operator && isOperator(char) ->
		  {acc ++ [char], :number}
		true ->
		  {acc, tok}
	  end
	end
	Enum.reduce(chars, {[], :number}, f)
	|> elem(0)
	|> Enum.join(" ")
  end
end

# # 6 + 5 * 4 / 2 + 1
# # = 23
# chrom1 = [
# 	[0,1,1,0],
# 	[1,0,1,0],
# 	[0,1,0,1],
# 	[1,1,0,0],
# 	[0,1,0,0],
# 	[1,1,0,1],
# 	[0,0,1,0],
# 	[1,0,1,0],
# 	[0,0,0,1]
# ]
# IO.puts "chrom1"
# IO.inspect chrom1
# IO.inspect App.decodeGenes(chrom1.genes)
# IO.inspect App.fitness(chrom1, 42)

# # 2 2 + n/a - 7 2
# # = 2 + 7
# # = 9
# chrom2 = [
#   [0,0,1,0],
#   [0,0,1,0],
#   [1,0,1,0],
#   [1,1,1,0],
#   [1,0,1,1],
#   [0,1,1,1],
#   [0,0,1,0]
# ]
# IO.puts "chrom2"
# IO.inspect chrom2
# IO.inspect App.decodeGenes(chrom2.genes)
# IO.inspect App.fitness(chrom2, 42)

# # 1 / 0
# # = 10^12
# chrom3 = [
#   [0,0,0,1],
#   [1,1,0,1],
#   [0,0,0,0]
# ]
# IO.puts "chrom3"
# IO.inspect chrom3
# IO.inspect App.decodeGenes(chrom3.genes)
# IO.inspect App.fitness(chrom3, 42)

# # 6 * 7
# # = 42
# chrom4 = [
#   [0,1,1,0],
#   [1,1,0,0],
#   [0,1,1,1]
# ]
# IO.puts "chrom4"
# IO.inspect chrom4
# IO.inspect App.decodeGenes(chrom4.genes)
# IO.inspect chrom4.fitness

IO.inspect App.run(500, 20, 42, 0.7, 0.001)
