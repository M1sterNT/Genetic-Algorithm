#Random Initial Populations
InitilaPopulation <- function(size) {
  Population <- c()
  for(i in 1:size) Population <- append(Population,  sample(0:1, 1))
  return(Population)
}

#Crossover 
Crossover <- function(Chromosome1 , Chromosome2 ) {
  random_range <- sample(1:length(Chromosome1), 1)
  for(i in 1:random_range) {
  temp = Chromosome2[[i]]
  Chromosome2[[i]] = Chromosome1[[i]]
  Chromosome1[[i]]  = temp
  }
  return(data.frame(Chromosome1, Chromosome2))
}

#Mutation
Mutation <- function(Chromosome){
  random_point <- sample(1:length(Chromosome), 1)
  if(Chromosome[[random_point]] == 1) Chromosome[[random_point]] = 0 
  else Chromosome[[random_point]] = 1
  return (Chromosome)
}

#TEST Function
size  = 10
ChromosomeA = InitilaPopulation(size)
ChromosomeB = InitilaPopulation(size)

CrossoverChromosomeAB = Crossover(ChromosomeA,ChromosomeB)

MutationChromosomeA = Mutation(ChromosomeA)
