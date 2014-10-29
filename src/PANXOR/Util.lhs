\begin{code}
{-# LANGUAGE RecordWildCards #-}
module PANXOR.Util where

import System.IO

import PANXOR.Option

argsCheck :: Opts -> IO Int
argsCheck Opts{..}
	| otherwise = return 0

-- Verify that the --file and --list arguments actually make sense.
filesCheck :: [Bool] -> IO Int
filesCheck fs
	| elem False fs = errMsgNum "an argument to --file does not exist" 1
	| otherwise = return 0
errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

errMsgNum :: String -> Int -> IO Int
errMsgNum str num = errMsg str >> return num
\end{code}
