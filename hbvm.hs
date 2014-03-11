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

import Data.Bits ((.&.) , shift)
import qualified Data.ByteString
import qualified Data.Word
import qualified System.Environment
import qualified System.Exit
import qualified System.IO

type ProgramCounter	= Int
type ProgramData	= [Int]
type Stack			= [Int]
type State			= (ProgramCounter, ProgramData, Stack)

data DeviceIO		= None | Get | Put


main = do
	args <- System.Environment.getArgs
	if (length args) /= 1
		then usage
		else return ()
	let binary = args !! 0
	program <- Data.ByteString.readFile binary
	Data.ByteString.putStr program
	putStrLn ""
	printList (splitWords program)
	System.Exit.exitSuccess

usage = do
	programName <- System.Environment.getProgName
	System.IO.hPutStrLn System.IO.stderr ("Usage: " ++ programName ++ " BINARY")
	System.Exit.exitFailure

printList :: [Int] -> IO ()
printList (x:xs)	= do
	putStrLn (show x)
	printList xs
printList []		= return ()

-------------------------------------------------------------------------------
-- the next few few functions are dedicated to converting the ByteString into a list of nibbles

-- converts the bytestring of program data into a list of nibbles
splitWords :: Data.ByteString.ByteString -> [Int]
splitWords s = foldl (\acc x -> acc ++ (breakWord x)) [] (Data.ByteString.unpack s)

word8ToInt :: Data.Word.Word8 -> Int
word8ToInt x = (fromIntegral x) :: Int

-- converts a Word8 into a list of two Ints
breakWord :: Data.Word.Word8 -> [Int]
breakWord x = [word8ToInt (highNibble x) , word8ToInt (lowNibble x)]

highNibble :: Data.Word.Word8 -> Data.Word.Word8
highNibble x = (shift x (-4)) .&. mask

lowNibble :: Data.Word.Word8 -> Data.Word.Word8
lowNibble x = x .&. mask

mask :: Data.Word.Word8
mask = 0x0F

-------------------------------------------------------------------------------