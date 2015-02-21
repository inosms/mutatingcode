module MutatingCode where

import Valid
import Data.List
import Data.Ord
import System.Random
import System.IO

type Population = [Genome]
type Fitnesscases = [([Bitstring],Bitstring)]
type Fitness = Integer
type Alphabet = [GeneComponent]
-- with a chance of x out of y
data Chance = Chance Integer Integer deriving Show
type FitnessFuncType = [([Bitstring],Bitstring,Bitstring)] -> Fitness


-- PARAMETERS
geneAlphabet = tg "+itapnrejc0123456789"
maximumGeneRandomLength = 10
maximumGenomeRandomLength = 3
maximumDepthOfExecution = 200

-- MODIFICATION RATES

-- probabilities always measured in 
-- x out of y
mutationRate = Chance 1 10

positionMutationRate = Chance 1 100

appendMutationRate = Chance 1 200

-- the best x members of the population will be cloned into the
-- next generation
reproduceTopNum = 2
------------------------------------

-- RANDOM FUNCTIONS

-- constructs a new random gene with the given alphabet
-- the length is restricted by the global parameters
-- alphabet -> result
getRandomGene :: Alphabet -> IO Gene
getRandomGene []  = return []
getRandomGene alphabet = do
	randomGenerator <- newStdGen
	let (randGeneLength,_) = randomR (0,maximumGeneRandomLength) randomGenerator 
	let randomGene = [ alphabet !! i | i <- (take randGeneLength ((randomRs (0,(length alphabet)-1)) randomGenerator) :: [Int]) ]
	return randomGene

-- constructs a new random genome also with the given alphabet
-- the size is also restricted by the global parameters
-- alphabet -> result
getRandomGenome :: Alphabet -> IO Genome
getRandomGenome alphabet = do
	randomGenerator <- newStdGen
	let (randGenomeLength,_) = randomR (0,maximumGenomeRandomLength) randomGenerator
	getRandomGenomeHelper alphabet randGenomeLength
	where 
		getRandomGenomeHelper :: Alphabet -> Integer -> IO Genome
		getRandomGenomeHelper _ 0 = return []
		getRandomGenomeHelper alphabet num = do
			randGene <- getRandomGene alphabet
			randRestGenome <- getRandomGenomeHelper alphabet (num-1)
			return (randGene:randRestGenome)

-- constructs an entire new random population with an alphabet
-- as well as a exact size of how large the population will be
-- alphabet -> population size -> result
getRandomPopulation :: Alphabet -> Integer -> IO Population
getRandomPopulation _ 0 = return []
getRandomPopulation alphabet num = do
	thisRandomGenome <- getRandomGenome alphabet
	restPopulation <- getRandomPopulation alphabet (num-1)
	return (thisRandomGenome:restPopulation)

-- takes to functions and executes the first with the given
-- chance, otherwise the other
doWithAChanceOf :: Chance -> IO a -> IO a-> IO a
doWithAChanceOf (Chance x y) f g = do
	randomGenerator <- newStdGen
	let (randNumber,_) = randomR (1,y) randomGenerator
	if randNumber <= x then do
		result <- f 
		return result
	else do
		result <- g
		return result

-- takes a list of genomes and fitness and returns
-- a random genome. A higher fitness results in a higher probability
-- that this genome is randomly chosen
-- the fitness MUST be greater than 0!
getOneRandom :: [(Genome,Fitness)] -> IO Genome
getOneRandom genomesAndFitness = do
	randomGenerator <- newStdGen
	let (randomNumber,_) = randomR (0,(sum (map snd genomesAndFitness)) -1 ) randomGenerator
	return (helper randomNumber genomesAndFitness)
	where
		helper :: Fitness -> [(Genome,Fitness)] -> Genome
		helper n ((x,m):xs) 
			| n < m = x
			| otherwise = helper (n-m) xs 

-- works like getOneRandom, but returns a list of n 
-- randomly chosen genomes
getNRandom :: Int -> [(Genome,Fitness)] -> IO [Genome]
getNRandom 0 _ = return []
getNRandom n genomesAndFitness = do
	thisRandom <- getOneRandom genomesAndFitness
	restRandom <- getNRandom (n-1) genomesAndFitness
	return (thisRandom:restRandom)

-- takes a list of genomes and fitness and resturns
-- two different random genes
getTwoRandom :: [(Genome,Fitness)] -> IO (Genome,Genome)
getTwoRandom genomesAndFitness = do
	firstRandom <- getOneRandom genomesAndFitness
	secondRandom <- (getOneRandom (deleteFromList firstRandom genomesAndFitness))
	return (firstRandom,secondRandom)
	where
		-- TODO maybe do this a _bit_ more efficiently?!
		deleteFromList :: Genome -> [(Genome,Fitness)] -> [(Genome,Fitness)]
		deleteFromList toDelete (x@(genome,_):xs)
			| toDelete == genome = xs
			| otherwise = x:(deleteFromList toDelete xs)

randomRange :: Int -> Int -> IO Int
randomRange a b = do
	randomGenerator <- newStdGen
	let (randomValue,_) = randomR (a,b) randomGenerator
	return randomValue
------------------------------------

-- RECOMBINATION

-- takes two genomes and crosses over all genes at one point
onePointRecombination :: Genome -> Genome -> IO Genome
onePointRecombination [] genome = return genome
onePointRecombination genome [] = return genome
onePointRecombination (x:xs) (y:ys) = do
	recombinatedGene <- onePointRecombinationHelper x y
	restResult <- onePointRecombination xs ys
	return (recombinatedGene:restResult)
	where
		onePointRecombinationHelper :: Gene -> Gene -> IO Gene
		onePointRecombinationHelper gene1 gene2 = do
			randomGenerator <- newStdGen
			let minLength = min (length gene1) (length gene2)
			let (randomCrossOverPoint,_) = randomR (0,minLength-1) randomGenerator
			return ((take randomCrossOverPoint gene1)++(drop randomCrossOverPoint gene2))

recombineN :: Int -> [(Genome,Fitness)] -> IO [Genome]
recombineN 0 _ = return []
recombineN n genomesAndFitness = do
	twoRandom@(random1,random2) <- getTwoRandom genomesAndFitness
	recombinated <- onePointRecombination random1 random2
	restRecombinated <- recombineN (n-1) genomesAndFitness
	return (recombinated:restRecombinated)
------------------------------------


-- PRINTER 
printRandomGenome :: Alphabet -> IO()
printRandomGenome alphabet = do
	randGenome <- getRandomGenome alphabet
	putStrLn (concat ( map geneToString randGenome))
	return ()

printRandomGene :: Alphabet -> IO()
printRandomGene alphabet = do
	randGene <- getRandomGene alphabet
	putStrLn (geneToString randGene)
	return ()
------------------------------------


-- MUTATION
-- replaces the element with index n in the given list with the given element
replaceAt :: Int -> [a] -> a -> [a]
replaceAt _ [] _ = []
replaceAt 0 (x:xs) replaceWithThis = replaceWithThis:xs
replaceAt n (x:xs) replaceWithThis = x: (replaceAt (n-1) xs replaceWithThis)

-- takes an alphabet and an genome and randomly chooses one position
-- at which the code will be replaced with another from the alphabet
pointMutation :: Alphabet -> Genome -> IO Genome
pointMutation _ [] = return []
pointMutation alphabet (x:xs) = do
	thisResult <- pointMutationHelper alphabet x
	restResult <- pointMutation alphabet xs
	return (thisResult:restResult)
	where 
		pointMutationHelper :: Alphabet -> Gene -> IO Gene
		pointMutationHelper alphabet gene = do
			randomGenerator <- newStdGen
			let (randomPosition,randomGenerator1) = randomR (0,(length gene)-1) randomGenerator
			let (randomNewCodeIndex,_) = randomR (0,(length alphabet)-1) randomGenerator1
			let randomNewCode = alphabet !! randomNewCodeIndex
			return (replaceAt randomPosition gene randomNewCode)

genePositionMutation :: Genome -> IO Genome
genePositionMutation genome = do
	randomGenerator <- newStdGen
	let genomeLength = length genome
	let (firstRandomGene,randomGenerator1) = randomR (0,genomeLength-1) randomGenerator
	let (secondRandomGene,_) = randomR (0,genomeLength-1) randomGenerator1
	let firstGene = genome !! firstRandomGene
	let secondGene = genome !! secondRandomGene
	return (replaceAt secondRandomGene (replaceAt firstRandomGene genome firstGene) secondGene)

appendMutation :: Genome -> IO Genome
appendMutation [] = return []
appendMutation genome = do
	randomIndex <- randomRange 0 ((length genome)-1)
	mutatedGene <- appendMutationHelper (genome !! randomIndex)
	return (replaceAt randomIndex genome mutatedGene)
	where
		appendMutationHelper :: Gene -> IO Gene
		appendMutationHelper gene = do
			randomGenerator <- newStdGen
			let (randomNewCodeIndex,_) = randomR (0,(length geneAlphabet)-1) randomGenerator
			let randomNewCode = geneAlphabet !! randomNewCodeIndex
			return (randomNewCode:gene)

genePositionMutatePopulation :: Population -> IO Population
genePositionMutatePopulation [] = return []
genePositionMutatePopulation (x:xs) = do
	mutatedOrNot <- doWithAChanceOf positionMutationRate (genePositionMutation x) (return x)
	restMutatedOrNot <- genePositionMutatePopulation xs
	return (mutatedOrNot:restMutatedOrNot)

appendMutatePopulation :: Population -> IO Population
appendMutatePopulation [] = return []
appendMutatePopulation (x:xs) = do
	mutatedOrNot <- doWithAChanceOf appendMutationRate (appendMutation x) (return x)
	restMutatedOrNot <- appendMutatePopulation xs
	return (mutatedOrNot:restMutatedOrNot)

-- todo delete one mutate

pointMutatePopulation :: Population -> IO Population
pointMutatePopulation [] = return []
pointMutatePopulation (x:xs) = do
	mutatedOrNot <- doWithAChanceOf mutationRate (pointMutation geneAlphabet x) (return x)
	restMutatedOrNot <- pointMutatePopulation xs
	return (mutatedOrNot:restMutatedOrNot) 

------------------------------------


-- takes a fitness function, a population and a set of fitness cases and
-- evaluates the given population according to the function. It returns
-- a set of genomes and final fitness
-- ( [(input, desired output, actual output, fitness)] -> population -> cases -> result paired by genome and fitness (not sorted)
evaluateFitness :: FitnessFuncType -> Population -> Fitnesscases -> [(Genome,Fitness)]
evaluateFitness fitnessFunc population cases = [(genome,fitness)| genome <- population, let fitness = fitnessFunc [ (input,desiredOutput,evaluateWithDepthRestriction maximumDepthOfExecution genome input) | (input,desiredOutput) <- cases]]

-- this applies one step of evolution to the population with
-- a given fitness function and fitness cases
-- the other parameters (mutation rate, reproduction rate, etc.) are set
-- using the golbal parameters
evolutionStep :: Population -> FitnessFuncType -> Fitnesscases -> IO Population
evolutionStep inputPopulation fitnessFunc cases = do
	randomGenerator <- newStdGen
	-- evaluate the fitness
	let fitness = evaluateFitness fitnessFunc inputPopulation cases
	-- sort population by fitness descending
	let sortedPopulation = reverse( sortBy (comparing snd) fitness )
	-- clone the best 
	let clonedBest = take reproduceTopNum sortedPopulation
	print (map snd sortedPopulation)
	print (map geneToString (head (map fst sortedPopulation)))

	-- calculate the numbers of how many will be cloned into the next generation
	-- and how many will be recombined into the net generation
	let inputPopulationSize = length inputPopulation
	let (recombinationNum,_) = randomR (0,inputPopulationSize-reproduceTopNum) randomGenerator
	let cloneNum = inputPopulationSize - reproduceTopNum - recombinationNum

	-- clone some into the next generation
	randomClones <- getNRandom cloneNum sortedPopulation
	-- and produce some trough recombination
	recombined <- recombineN recombinationNum sortedPopulation

	-- create the new population
	let newGeneration = randomClones ++ recombined ++ (map fst clonedBest)

	-- finally: also mutate the whole new generation according
	-- to the global mutation rate
	newGenerationPointMutated <- pointMutatePopulation newGeneration
	newGenerationPositionMutated <- genePositionMutatePopulation newGenerationPointMutated
	newGenerationAppendMutated <- appendMutatePopulation newGenerationPositionMutated
	-- complete!
	return newGenerationAppendMutated



evolve :: Population -> FitnessFuncType -> Fitnesscases -> IO Population
evolve inputPopulation fitnessFunc cases = do
	evolvedPopulation <- evolutionStep inputPopulation fitnessFunc cases
	evolve evolvedPopulation fitnessFunc cases

-- FITNESS FUNCTIONS

-- this is an example fitness, which criteria is the 
-- difference of lengths of desired output and actual output lists
fitnessLengthDiff :: FitnessFuncType
fitnessLengthDiff cases = max 0 (sum [ 20 - abs((genericLength desired) - (genericLength output)) | (_,desired,output) <- cases])

fitnessIsEqual :: FitnessFuncType
fitnessIsEqual cases = max 0 ( sum [20 - (if desired == output then 0 else 1) | (_,desired,output) <- cases])

------------------------------------


_TEST_population :: IO ()
_TEST_population = do 
	randPopulation <- getRandomPopulation geneAlphabet 10
	let cases = [([[R]],[L]),([[R]],[L]),([[R,L]],[L,R]),([[L,R]],[R,L]),([[R,R]],[L,L]),([[R,R]],[L,L])]
	result <- evolve randPopulation fitnessIsEqual cases
	return()







