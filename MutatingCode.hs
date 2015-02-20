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

-- alphabet -> result
getRandomGene :: Alphabet -> IO Gene
getRandomGene []  = return []
getRandomGene alphabet = do
	randomGenerator <- newStdGen
	let (randGeneLength,_) = randomR (0,maximumGeneRandomLength) randomGenerator 
	let randomGene = [ alphabet !! i | i <- (take randGeneLength ((randomRs (0,(length alphabet)-1)) randomGenerator) :: [Int]) ]
	return randomGene

-- alphabet -> result
getRandomGenome :: Alphabet -> IO Genome
getRandomGenome alphabet = do
	randomGenerator <- newStdGen
	let (randGenomeLength,_) = randomR (0,maximumGenomeRandomLength) randomGenerator
	getRandomGenomeHelper alphabet randGenomeLength
getRandomGenomeHelper :: Alphabet -> Integer -> IO Genome
getRandomGenomeHelper _ 0 = return []
getRandomGenomeHelper alphabet num = do
	randGene <- getRandomGene alphabet
	randRestGenome <- getRandomGenomeHelper alphabet (num-1)
	return (randGene:randRestGenome)

-- alphabet -> population size -> result
getRandomPopulation :: Alphabet -> Integer -> IO Population
getRandomPopulation _ 0 = return []
getRandomPopulation alphabet num = do
	thisRandomGenome <- getRandomGenome alphabet
	restPopulation <- getRandomPopulation alphabet (num-1)
	return (thisRandomGenome:restPopulation)


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
replaceAt :: Int -> [a] -> a -> [a]
replaceAt 0 (x:xs) replaceWithThis = replaceWithThis:xs
replaceAt n (x:xs) replaceWithThis = x: (replaceAt (n-1) xs replaceWithThis)

pointMutation :: Alphabet -> Genome -> IO Genome
pointMutation _ [] = return []
pointMutation alphabet (x:xs) = do
	thisResult <- pointMutationHelper alphabet x
	restResult <- pointMutation alphabet xs
	return (thisResult:restResult)

pointMutationHelper :: Alphabet -> Gene -> IO Gene
pointMutationHelper alphabet gene = do
	randomGenerator <- newStdGen
	let (randomPosition,_) = randomR (0,(length gene)-1) randomGenerator
	let (randomNewCodeIndex,_) = randomR (0,(length alphabet)-1) randomGenerator
	let randomNewCode = alphabet !! randomNewCodeIndex
	return (replaceAt randomPosition gene randomNewCode)



------------------------------------


-- ( [(input, desired output, actual output, fitness)] -> population -> cases -> result paired by genome and fitness (not sorted)
evaluateFitness :: FitnessFuncType -> Population -> Fitnesscases -> [(Genome,Fitness)]
evaluateFitness fitnessFunc population cases = [(genome,fitness)| genome <- population, let fitness = fitnessFunc [ (input,desiredOutput,evaluateWithDepthRestriction maximumDepthOfExecution genome input) | (input,desiredOutput) <- cases]]

fitnessLengthDiff :: FitnessFuncType
fitnessLengthDiff cases = sum [ 20 - abs((genericLength desired) - (genericLength output)) | (_,desired,output) <- cases]

evolutionStep :: Population -> FitnessFuncType -> Fitnesscases -> IO Population
evolutionStep inputPopulation fitnessFunc cases = do
	randomGenerator <- newStdGen
	let fitness = evaluateFitness fitnessFunc inputPopulation cases
	let sortedPopulation = reverse( sortBy (comparing snd) fitness )
	let combinedFitness = sum ( map snd sortedPopulation )

	let clonedBest = take reproduceTopNum sortedPopulation
	let inputPopulationSize = length inputPopulation
	let (recombinationNum,_) = randomR (0,inputPopulationSize-reproduceTopNum) randomGenerator
	let (cloneNum,_) = randomR (0,inputPopulationSize - reproduceTopNum - recombinationNum ) randomGenerator
	-- TODO clones and recombination

	print (map snd clonedBest)
	return (map fst sortedPopulation)


_TEST_population :: IO ()
_TEST_population = do 
	randPopulation <- getRandomPopulation geneAlphabet 10
	let cases = [([[R]],[R]),([[L,L]],[L,L]),([[L,L,L]],[L,L,L])]
	result <- evolutionStep randPopulation fitnessLengthDiff cases
	return()







