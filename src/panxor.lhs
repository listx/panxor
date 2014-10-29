\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (liftM, when)
import Data.Bits
import Data.Char
import Data.List (foldl', nub)
import Numeric
import System.Console.CmdArgs.Implicit
import System.IO
import System.Directory
import System.Exit

data Opts = Opts
	{ hex :: [String]
	, dec :: [String]
	, oct :: [String]
	, bin :: [String]
	, file_hex :: [FilePath]
	, file_dec :: [FilePath]
	, file_oct :: [FilePath]
	, file_bin :: [FilePath]
	, stdin_hex :: Bool
	, out_hex :: Bool
	, out_dec :: Bool
	, out_oct :: Bool
	, out_bin :: Bool
	, min_length :: Int
	} deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
	{ hex = [] &= name "x" &= typ "HEXADECIMAL" &= help "Additional hashes in hexadecimal to XOR into (use this flag once for each additional hash; also, do not prefix the hex with `0x'; e.g., use `f3' instead of `0xf3'). Leading zeroes are ignored; trailing non-hex characters (as well as non-leading-hex strings) are also ignored."
	, dec = [] &= typ "DECIMAL" &= help "Like --hex, but in decimal (0-9)."
	, oct = [] &= name "o" &= typ "OCTAL" &= help "Like --hex, but in octal (0-7)."
	, bin = [] &= typ "BINARY" &= help "Like --hex, but in binary (0s and 1s)."
	, file_hex = [] &= name "X" &= typFile &= help "Read hex hashes from a file; the expected format of the file is the output of the sha1sum(1) program. You can use this flag multiple times for multiple files."
	, file_dec = [] &= name "D" &= typFile &= help "Like --file-hex, but read in decimal values."
	, file_oct = [] &= name "O" &= typFile &= help "Like --file-hex, but read in octal values."
	, file_bin = [] &= name "B" &= typFile &= help "Like --file-hex, but read in binary values."
	, stdin_hex = False &= help "Enable reading from STDIN. Only hexadecimal values (sha1sum(1) format) are read in with this option. If no input files are specified with --file-{hex,dec,bin}, and no other hashes are specified with --{hex,dec,bin}, then this flag is automatically turned on. In other words, if no arguments are specified, then panxor expects input from STDIN."
	, out_hex = False &= help "Output the final hash in hexadecimal (without the leading `0x'). If no output format is specified with --out-{hex,dec,bin}, then this flag is turned on automatically."
	, out_dec = False &= help "Output the final hash in decimal."
	, out_oct = False &= help "Output the final hash in octal."
	, out_bin = False &= help "Output the final hash in binary."
	, min_length = 0 &= help "Output length must be at least INT long (if output is lesser than this length, then the leading digits are padded with 0). Default is 0."
	}
	&= details
		[ "Notes:"
		, ""
		, "  Panxor can read in any arbitrarily long hex, decimal, or binary string, and is also compatible with the sha1sum(1) format."
		, "Examples:"
		, "  Use sha1sum(1) against some files, and then xor all of those hashes into one (note the use of --min-length to ensure that the output 'hash' is also always 40 digits long):"
		, "    sha1sum my_files or_folders | panxor --stdin-hex --min-length 40"
		]

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
	&= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
	&= program _PROGRAM_NAME
	&= help _PROGRAM_DESC
	&= helpArg [explicit, name "help", name "h"]
	&= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]

_PROGRAM_NAME
	, _PROGRAM_VERSION
	, _PROGRAM_INFO
	, _PROGRAM_DESC
	, _COPYRIGHT :: String
_PROGRAM_NAME = "panxor"
_PROGRAM_VERSION = "0.0.2"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_DESC = "numerically XOR multiple hex, decimal, octal, or binary values"
_COPYRIGHT = "(C) Linus Arver 2012-2014"

argsCheck :: Opts -> IO Int
argsCheck Opts{..}
	| otherwise = return 0

-- Verify that the --file and --list arguments actually make sense.
filesCheck :: [Bool] -> IO Int
filesCheck fs
	| elem False fs = errMsgNum "an argument to --file does not exist" 1
	| otherwise = return 0

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	hSetEcho stdin False -- disable terminal echo
	opts <- getOpts
	(\e -> when (e > 0) . exitWith $ ExitFailure e) =<< argsCheck opts
	let
		opts'@Opts{..} = autoOpts opts -- automatically use sane defaults
	stdinHashHex <- return . xorNum NumHex =<< (if stdin_hex
		then getContents
		else return [])
	fs <- mapM doesFileExist (file_hex ++ file_dec ++ file_oct ++ file_bin)
	(\e -> when (e > 0) . exitWith $ ExitFailure e) =<< filesCheck fs
	errNo' <- filesCheck fs
	when (errNo' > 0) $ exitWith $ ExitFailure errNo'
	prog opts' stdinHashHex file_hex file_dec file_oct file_bin

autoOpts :: Opts -> Opts
autoOpts opts@Opts{..} = opts
	{ stdin_hex = null
		(hex
		++ dec
		++ oct
		++ bin
		++ file_hex
		++ file_dec
		++ file_oct
		++ file_bin)
	}

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
		| otherwise = error $ "digitToBinaryInt: not a binary digit `" ++ show c ++ "'"

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

errMsgNum :: String -> Int -> IO Int
errMsgNum str num = errMsg str >> return num
\end{code}
