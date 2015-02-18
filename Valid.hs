module Valid where

import Test.QuickCheck
import Data.List
import Data.Char


data Bit = L | R deriving (Eq, Show)
type Bitstring = [Bit]

data GeneComponent = GeneOperator Char | GeneParameter Int
type Gene = [GeneComponent]
type Genome = [Gene]

instance Show GeneComponent where
	show (GeneParameter x) = show x
	show (GeneOperator x) = [x]

toGene :: String -> Gene
toGene (x:xs) 
	| isAlpha x ||  x == '+' = (GeneOperator x) : toGene xs
	| otherwise = (GeneParameter (digitToInt x)) : toGene xs
toGene [] = []
tg = toGene

geneToString :: Gene -> String 
geneToString ((GeneOperator x):xs) = x:(geneToString xs)
geneToString ((GeneParameter x):xs) = (show x)++(geneToString xs)
geneToString [] = []

toBitstring :: String -> Bitstring
toBitstring ('0':xs) = L:(toBitstring xs)
toBitstring ('1':xs) = R:(toBitstring xs)
toBitstring (_:xs) = toBitstring xs
toBitstring [] = []

fromBitstring :: Bitstring -> String
fromBitstring bitstring = map (\x->if x == L then '0' else '1') bitstring

plus :: Bitstring -> Bitstring -> Bitstring
plus a b =  _plus a b L
_plus :: Bitstring -> Bitstring -> Bit -> Bitstring
_plus (R:xs) (L:ys) L = R:(_plus xs ys L)
_plus (R:xs) (L:ys) R = L:(_plus xs ys R)
_plus (R:xs) (R:ys) L = L:(_plus xs ys R)
_plus (R:xs) (R:ys) R = R:(_plus xs ys R)
_plus (L:xs) (R:ys) L = R:(_plus xs ys L)
_plus (L:xs) (R:ys) R = L:(_plus xs ys R)
_plus (L:xs) (L:ys) L = L:(_plus xs ys L)
_plus (L:xs) (L:ys) R = R:(_plus xs ys L)
_plus xs [] R = _plus xs [R] L
_plus xs [] L = xs
_plus [] ys R = _plus ys [R] L
_plus [] ys L = ys


binaryToInt :: Bitstring -> Int
binaryToInt bitstring = sum [if (bitstring !! x) == R then 2^x else 0|x<-[0..((length bitstring)-1)]]

neg :: Bitstring -> Bitstring
neg (L:xs) = R:(neg xs)
neg (R:xs) = L:(neg xs)
neg [] = []

seq_neg :: Bitstring -> Bitstring
seq_neg input = evaluate [tg "e0ci0njct0pjct0"] [input]


getCorrectParameter :: Int -> [Bitstring] -> Bitstring
getCorrectParameter paramNumber parameters 
	| paramNumber < (length parameters) = (!!) parameters paramNumber 
	| otherwise = []


-- evaluates the genome, this takes the first gene, evaluates this with the
-- rest of the gnome as jump library
-- the second parameter is a list of parameters for the genome
evaluate :: Genome -> [Bitstring] -> Bitstring
evaluate [] _ = []
evaluate genome@(x:xs) params = fst3 (evaluateGene (-1) genome x params )

evaluateWithDepthRestriction :: Integer -> Genome -> [Bitstring] -> Bitstring
evaluateWithDepthRestriction _ [] _ = []
evaluateWithDepthRestriction depth genome@(x:xs) params = fst3 (evaluateGene depth genome x params )

bitstringTail :: Bitstring -> Bitstring
bitstringTail [] = []
bitstringTail (x:xs) = xs

getParamsCount :: Gene -> Int
getParamsCount [] = 0
getParamsCount ((GeneParameter x):xs) = max (x+1) (getParamsCount xs)
getParamsCount ((GeneOperator _):xs) = getParamsCount xs 

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

decreaseDepth :: Integer -> Integer
decreaseDepth n
	| n <= 0 = 0
	| otherwise = n -1

-- evaluates a gene with a maximum of depth jumps to other genes to prevent infinite loops
-- if depth is -1 there is no maximum
evaluateGene :: Integer -> Genome -> Gene -> [Bitstring] -> (Bitstring,Gene,Integer)
evaluateGene 0 _ gene _ = ([],gene,0)
evaluateGene depth genome ((GeneOperator '+'):xs) parameters = evaluateWrapper depth genome (\[x,y] -> plus x y) 2 xs parameters
evaluateGene depth genome ((GeneOperator 'i'):xs) parameters = evaluateWrapper depth genome (\[x,y,z] -> if null x || head x == L then z else y) 3 xs parameters
evaluateGene depth genome ((GeneOperator 'e'):xs) parameters = evaluateWrapper depth genome (\[x,y,z] -> if null x then y else z) 3 xs parameters
evaluateGene depth genome ((GeneOperator 't'):xs) parameters = evaluateWrapper depth genome (bitstringTail.head) 1 xs parameters
evaluateGene depth genome ((GeneOperator 'p'):xs) parameters = evaluateWrapper depth genome ((R:).head) 1 xs parameters
evaluateGene depth genome ((GeneOperator 'r'):xs) parameters = evaluateWrapper depth genome (reverse.head) 1 xs parameters
evaluateGene depth genome ((GeneOperator 'n'):xs) parameters = evaluateWrapper depth genome ((L:).head) 1 xs parameters
evaluateGene depth genome ((GeneOperator 'a'):xs) parameters = evaluateWrapper depth genome (concat) 2 xs parameters
evaluateGene depth genome ((GeneOperator 'c'):xs) parameters = ([],xs,depth)
evaluateGene depth genome ((GeneOperator 'j'):xs) parameters = (resultOtherGeneBS,restParam,resultOtherGeneDepth  )--evaluateWrapper depthRest genome (\x->fst3( evaluateGene (depthRest-1) genome destGene x )) destGeneParamsCount destGeneRest parameters
	where (destGeneIndex,destGeneRest,depthRest) = evaluateWrapper depth genome (head) 1 xs parameters
	      destGeneIndexInt = (binaryToInt destGeneIndex) `mod` (length genome)
	      destGene = genome !! destGeneIndexInt
	      destGeneParamsCount = getParamsCount destGene
	      (resultParam,restParam,restDepth) = evaluateNTimes depthRest genome destGeneRest parameters destGeneParamsCount
	      (resultOtherGeneBS,resultOtherGeneGene,resultOtherGeneDepth) = evaluateGene (decreaseDepth restDepth) genome destGene resultParam

evaluateGene depth genome ((GeneParameter x):xs) parameters = (getCorrectParameter x parameters, xs,depth)
evaluateGene depth genome [] _ = ([],[],depth)

evaluateWrapper :: Integer -> Genome -> ([Bitstring]->Bitstring) -> Int -> Gene -> [Bitstring] -> (Bitstring,Gene,Integer)
evaluateWrapper depth genome func paramsCount gene params = (func results,rest,restDepth)
				where (results,rest,restDepth) = evaluateNTimes depth genome gene params paramsCount


-- takes a depth gene, parameters and an integer which specifies how often the rest after
-- one successfull evaluation is evaluated
-- the result of the evaluations are then given back
evaluateNTimes :: Integer -> Genome -> Gene -> [Bitstring] -> Int -> ([Bitstring],Gene,Integer)
evaluateNTimes depth genome xs parameters 0 = ([],xs,depth)
evaluateNTimes depth genome xs parameters 1 = ([value1],rest1,depth1)
				where (value1,rest1,depth1) = evaluateGene depth genome xs parameters 
evaluateNTimes depth genome xs parameters n = (value1:resultValues,resultRest,restultDepth)
				where (value1,rest1,depth1) = evaluateGene depth genome xs parameters 
				      (resultValues,resultRest,restultDepth) = evaluateNTimes depth1 genome rest1 parameters (n-1)
