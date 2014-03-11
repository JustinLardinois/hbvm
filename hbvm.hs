-- hbvm.hs
-- a component of hbvm
-- latest version available at:
-- https://github.com/JustinLardinois/ls-for-Windows
--
-- Copyright 2014 Justin Lardinois
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--  http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
---------------------------------------------------------------------------

import qualified Data.ByteString
import qualified System.Environment
import qualified System.Exit
import qualified System.IO

main = do
	args <- System.Environment.getArgs
	if (length args) /= 1
		then usage
		else return ()
	let binary = args !! 0
	program <- Data.ByteString.readFile binary
	Data.ByteString.putStr program
	putStrLn ""
	System.Exit.exitSuccess

usage = do
	programName <- System.Environment.getProgName
	System.IO.hPutStrLn System.IO.stderr ("Usage: " ++ programName ++ " BINARY")
	System.Exit.exitFailure