\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (when)
import System.IO
import System.Directory
import System.Exit

import PANXOR.Core
import PANXOR.Option
import PANXOR.Util

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
\end{code}
