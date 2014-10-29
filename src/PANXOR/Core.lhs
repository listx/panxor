\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module PANXOR.Core where

import Control.Monad (liftM)
import Data.Bits
import Data.Char
import Data.List (foldl', nub)
import Numeric

import PANXOR.Option

prog
	:: Opts
	-> Integer
	-> [FilePath]
	-> [FilePath]
	-> [FilePath]
	-> [FilePath]
	-> IO ()
prog Opts{..} stdinHashHex filesHex filesDec filesOct filesBin = do
	filesHashHex <- mapM (return . liftM (xorNum NumHex) =<< readFile) filesHex
	filesHashDec <- mapM (return . liftM (xorNum NumDec) =<< readFile) filesDec
	filesHashOct <- mapM (return . liftM (xorNum NumOct) =<< readFile) filesOct
	filesHashBin <- mapM (return . liftM (xorNum NumBin) =<< readFile) filesBin
	let
		hashesHex = map (xorNum NumHex) hex
		hashesDec = map (xorNum NumDec) dec
		hashesOct = map (xorNum NumOct) oct
		hashesBin = map (xorNum NumBin) bin
		hash = foldl' xor stdinHashHex
			( filesHashHex
			++ filesHashDec
			++ filesHashOct
			++ filesHashBin
			++ hashesHex
			++ hashesDec
			++ hashesOct
			++ hashesBin
			)
	putStrLn . padZeros $ showStyle hash []
	where
	showStyle :: (Integral a, Show a) => a -> ShowS
	showStyle
		| out_hex = showHex
		| out_dec = showInt
		| out_oct = showOct
		| out_bin = showIntAtBase 2 intToDigit
		| otherwise = showHex
	padZeros :: String -> String
	padZeros str
		| min_length > 0 = replicate padLen '0' ++ str
		| otherwise = str
		where
		padLen
			| min_length > length str = min_length - length str
			| otherwise = 0

data NumBase
	= NumHex
	| NumDec
	| NumOct
	| NumBin
	deriving (Eq)

-- Takes a sha1sum(1) formatted string (hex hashes), and XORs all of the hashes in there.
xorNum :: NumBase -> String -> Integer
xorNum b = foldl' xor 0
	. map
		( (\x -> if null x
			then 0
			else fst $ head x)
		. (case b of
			NumHex -> readHex
			NumDec -> readDec
			NumOct -> readOct
			NumBin -> readInt 2 isBinaryDigit digitToBinaryInt
			)
		)
	. nub
	. filter (not . null)
	. lines
	where
	isBinaryDigit :: Char -> Bool
	isBinaryDigit c = c == '0' || c == '1'
	digitToBinaryInt :: Char -> Int
	digitToBinaryInt c
		| c == '0' = 0
		| c == '1' = 1
		| otherwise = error
			$ "digitToBinaryInt: not a binary digit `" ++ show c ++ "'"
\end{code}
