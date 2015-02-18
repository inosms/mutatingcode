module MutatingCode where

import Valid
import Data.List
import Data.Ord
import System.Random
import System.IO

type Population = [Genome]
type Fitnesscases = [(Bitstring,Bitstring)]
type Fitness = Integer

-- PARAMETERS
geneAlphabet = "+itapnrejc0123456789"
maximumGeneRandomLength = 99 
maximumGenomeRandomLength = 9
maximumDepthOfExecution = 999
------------------------------------

-- RANDOM FUNCTIONS

-- alphabet -> result
getRandomGene :: String -> IO Gene
getRandomGene []  = return (tg "")
getRandomGene alphabet = do
		randomGenerator <- newStdGen
		let (randGeneLength,_) = randomR (0,maximumGeneRandomLength) randomGenerator 
		let randomGene = [ alphabet !! i | i <- (take randGeneLength ((randomRs (0,(length alphabet)-1)) randomGenerator) :: [Int]) ]
		return (tg randomGene)

-- alphabet -> result
getRandomGenome :: String -> IO Genome
getRandomGenome alphabet = do
		randomGenerator <- newStdGen
		let (randGenomeLength,_) = randomR (0,maximumGenomeRandomLength) randomGenerator
		getRandomGenomeHelper alphabet randGenomeLength
getRandomGenomeHelper :: String -> Integer -> IO Genome
getRandomGenomeHelper _ 0 = return []
getRandomGenomeHelper alphabet num = do
		randGene <- getRandomGene alphabet
		randRestGenome <- getRandomGenomeHelper alphabet (num-1)
		return (randGene:randRestGenome)

-- alphabet -> population size -> result
getRandomPopulation :: String -> Integer -> IO Population
getRandomPopulation _ 0 = return []
getRandomPopulation alphabet num = do
		thisRandomGenome <- getRandomGenome alphabet
		restPopulation <- getRandomPopulation alphabet (num-1)
		return (thisRandomGenome:restPopulation)
------------------------------------


-- PRINTER 
printRandomGenome :: String -> IO()
printRandomGenome alphabet = do
		randGenome <- getRandomGenome alphabet
		putStrLn (concat ( map geneToString randGenome))
		return ()

printRandomGene :: String -> IO()
printRandomGene alphabet = do
		randGene <- getRandomGene alphabet
		putStrLn (geneToString randGene)
		return ()
------------------------------------


-- MUTATION
replaceAt :: Int -> [a] -> a -> [a]
replaceAt 0 (x:xs) replaceWithThis = replaceWithThis:xs
replaceAt n (x:xs) replaceWithThis = x: (replaceAt (n-1) xs replaceWithThis)

pointMutation :: String -> Genome -> IO Genome
pointMutation _ [] = return []
pointMutation alphabet (x:xs) = do
		thisResult <- pointMutationHelper alphabet x
		restResult <- pointMutation alphabet xs
		return (thisResult:restResult)

pointMutationHelper :: String -> Gene -> IO Gene
pointMutationHelper alphabet gene = do
		randomGenerator <- newStdGen
		let (randomPosition,_) = randomR (0,(length gene)-1) randomGenerator
		let (randomNewCodeIndex,_) = randomR (0,(length alphabet)-1) randomGenerator
		let randomNewCode = alphabet !! randomNewCodeIndex
		return (replaceAt randomPosition gene (head (tg [randomNewCode])))


------------------------------------


-- ( [(input, desired output, actual output, fitness)] -> population -> parameter -> cases -> result paired by genome and fitness (not sorted)
evaluateFitness :: ([(Bitstring,Bitstring,Bitstring)]->Fitness) -> Population -> [Bitstring] -> Fitnesscases -> [(Genome,Fitness)]
evaluateFitness fitnessFunc population params cases = [(genome,fitness)| genome <- population, let fitness = fitnessFunc [ (input,desiredOutput,evaluateWithDepthRestriction maximumDepthOfExecution genome params) | (input,desiredOutput) <- cases]]

fitnessLengthDiff :: [(Bitstring,Bitstring,Bitstring)] -> Fitness
fitnessLengthDiff cases = sum [ 20 - abs((genericLength desired) - (genericLength output)) | (_,desired,output) <- cases]

_TEST_population :: IO ()
_TEST_population = do 
	randPopulation <- getRandomPopulation geneAlphabet 10
	let cases = [([R],[R]),([L,L],[L,L]),([L,L,L],[L,L,L])]
	let randPopulationFitness = evaluateFitness fitnessLengthDiff randPopulation [[]] cases
	print randPopulationFitness
	return()







