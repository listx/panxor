\section{PANXOR.Option}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module PANXOR.Option where

import System.Console.CmdArgs.Implicit

import PANXOR.Meta

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
	{ hex = [] &= name "x" &= typ "HEXADECIMAL" &= help
"Additional hashes in hexadecimal to XOR into (use this flag once for each\
\ additional hash; also, do not prefix the hex with `0x'; e.g., use `f3' instead\
\ of `0xf3'). Leading zeroes are ignored; trailing non-hex characters (as well as\
\ non-leading-hex strings) are also ignored."
	, dec = [] &= typ "DECIMAL" &= help "Like --hex, but in decimal (0-9)."
	, oct = [] &= name "o" &= typ "OCTAL" &= help
		"Like --hex, but in octal (0-7)."
	, bin = [] &= typ "BINARY" &= help "Like --hex, but in binary (0s and 1s)."
	, file_hex = [] &= name "X" &= typFile &= help
"Read hex hashes from a file; the expected format of the file is the\
\ output of the sha1sum(1) program. You can use this flag multiple\
\ times for multiple files."
	, file_dec = [] &= name "D" &= typFile &= help
		"Like --file-hex, but read in decimal values."
	, file_oct = [] &= name "O" &= typFile &= help
		"Like --file-hex, but read in octal values."
	, file_bin = [] &= name "B" &= typFile &= help
		"Like --file-hex, but read in binary values."
	, stdin_hex = False &= help
"Enable reading from STDIN. Only hexadecimal values (sha1sum(1) format) are read\
\ in with this option. If no input files are specified with --file-{hex,dec,bin},\
\ and no other hashes are specified with --{hex,dec,bin}, then this flag is\
\ automatically turned on. In other words, if no arguments are specified, then\
\ panxor expects input from STDIN."
	, out_hex = False &= help
"Output the final hash in hexadecimal (without the leading `0x'). If no output\
\ format is specified with --out-{hex,dec,bin}, then this flag is turned on\
\ automatically."
	, out_dec = False &= help "Output the final hash in decimal."
	, out_oct = False &= help "Output the final hash in octal."
	, out_bin = False &= help "Output the final hash in binary."
	, min_length = 0 &= help
"Output length must be at least INT long (if output is lesser than this length,\
\ then the leading digits are padded with 0). Default is 0."
	}
	&= details
		[ "Notes:"
		, ""
		, "  Panxor can read in any arbitrarily long hex, decimal, or binary"
			++ " string, and is also compatible with the sha1sum(1) format."
		, "Examples:"
		, "  Use sha1sum(1) against some files, and then xor all of those"
			++ " hashes into one (note the use of --min-length to ensure that"
			++ " the output 'hash' is also always 40 digits long):"
		, "    sha1sum my_files or_folders | panxor --stdin-hex --min-length 40"
		]

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
	&= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
	&= program _PROGRAM_NAME
	&= help _PROGRAM_DESC
	&= helpArg [explicit, name "help", name "h"]
	&= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]

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
\end{code}
