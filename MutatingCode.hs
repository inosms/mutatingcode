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
maximumGeneRandomLength = 99 
maximumGenomeRandomLength = 3
maximumDepthOfExecution = 999

-- MODIFICATION RATES

-- probabilities always measured in 
-- x out of y
mutationRate = Chance 1 100

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
doWithAChanceOf :: Chance -> IO () -> IO ()-> IO ()
doWithAChanceOf (Chance x y) f g = do
	randomGenerator <- newStdGen
	let (randNumber,_) = randomR (1,y) randomGenerator
	if randNumber <= x then do
		f 
		return()
	else do
		g
		return()

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
			let (randomPosition,_) = randomR (0,(length gene)-1) randomGenerator
			let (randomNewCodeIndex,_) = randomR (0,(length alphabet)-1) randomGenerator
			let randomNewCode = alphabet !! randomNewCodeIndex
			return (replaceAt randomPosition gene randomNewCode)



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

	-- calculate the numbers of how many will be cloned into the next generation
	-- and how many will be recombined into the net generation
	let inputPopulationSize = length inputPopulation
	let (recombinationNum,_) = randomR (0,inputPopulationSize-reproduceTopNum) randomGenerator
	let (cloneNum,_) = randomR (0,inputPopulationSize - reproduceTopNum - recombinationNum ) randomGenerator

	-- clone some into the next generation
	randomClones <- getNRandom cloneNum sortedPopulation

	-- TODO recombine

	print (map snd clonedBest)
	return (map fst sortedPopulation)


-- FITNESS FUNCTIONS

-- this is an example fitness, which criteria is the 
-- difference of lengths of desired output and actual output lists
fitnessLengthDiff :: FitnessFuncType
fitnessLengthDiff cases = sum [ 20 - abs((genericLength desired) - (genericLength output)) | (_,desired,output) <- cases]

------------------------------------


_TEST_population :: IO ()
_TEST_population = do 
	randPopulation <- getRandomPopulation geneAlphabet 10
	let cases = [([[R]],[R]),([[L,L]],[L,L]),([[L,L,L]],[L,L,L])]
	result <- evolutionStep randPopulation fitnessLengthDiff cases
	return()







