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

import Data.Bits ((.&.) , (.|.) , complement , shift)
import qualified Data.ByteString
import qualified Data.Char
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
	let programData = splitWords program
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
highNibble x = (shift x (-4)) .&. nibbleMask

lowNibble :: Data.Word.Word8 -> Data.Word.Word8
lowNibble x = x .&. nibbleMask

nibbleMask :: Data.Word.Word8
nibbleMask = 0x0F

-------------------------------------------------------------------------------

cpu :: State -> IO ()
cpu x = do
	x' <- interface (execute x)
	cpu x'

execute :: State -> (State, DeviceIO)
execute (pc,p,s) = (op_lookup pc) (pc,p,s)

interface :: (State, DeviceIO) -> IO State
interface ((pc,p,s)     , None) = return (pc,p,s)
interface ((pc,p,s)     , Get ) = do
	c <- getChar
	let n = (Data.Char.ord c) .&. byteMask
	return (pc,p,(n:s))
interface ((pc,p,(x:s)), Put ) = do
	putChar (Data.Char.chr (x .&. byteMask))
	return (pc,p,s)

byteMask :: Int
byteMask = 0x0FF

op_lookup :: Int -> (State -> (State, DeviceIO))
op_lookup opcode
	| opcode >= 0x0 && opcode <= 0xE	= ops !! opcode
	| otherwise							= error ("invalid opcode " ++ (show opcode))

ops :: [State -> (State, DeviceIO)]
ops = [op_push,op_and,op_not,op_or,op_mul,op_div,op_add,op_cmp,op_pop,op_swp,op_dup,op_ppc,op_get,op_put,op_br]

op_push :: State -> (State, DeviceIO)
op_push (pc,p,s) = ((pc+2,p,(p !! (pc+1)):s),None)

op_and :: State -> (State, DeviceIO)
op_and (pc,p,(x:y:s)) = ((pc+1,p,(x .&. y):s),None)

op_not :: State -> (State, DeviceIO)
op_not (pc,p,(x:s)) = ((pc+1,p,(complement x):s),None)

op_or :: State -> (State, DeviceIO)
op_or (pc,p,(x:y:s)) = ((pc+1,p,(x .|. y):s),None)

op_mul :: State -> (State, DeviceIO)
op_mul (pc,p,(x:y:s)) = ((pc+1,p,(x*y):s),None)

op_div :: State -> (State, DeviceIO)
op_div (pc,p,(x:y:s)) = ((pc+1,p,(x `div` y):s),None)

op_add :: State -> (State, DeviceIO)
op_add (pc,p,(x:y:s)) = ((pc+1,p,(x+y):s),None)

op_cmp :: State -> (State, DeviceIO)
op_cmp (pc,p,(x:y:s))
	| x == y	= ((pc+1,p,1:s),None)
	| otherwise	= ((pc+1,p,0:s),None)

op_pop :: State -> (State, DeviceIO)
op_pop (pc,p,(x:s)) = ((pc+1,p,s),None)

op_swp :: State -> (State, DeviceIO)
op_swp (pc,p,(0:y:s)) = (op_lookup y) (pc,p,s)
op_swp (pc,p,(x:y:s)) = ((pc+1,p,(swapElts 0 y s)),None)

-- copied from https://gist.github.com/ijt/2010183 to help with op_swp
swapElts :: Int -> Int -> [a] -> [a]
swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
	where get k x
		| k == i = ls !! j
		| k == j = ls !! i
		| otherwise = x

op_dup :: State -> (State, DeviceIO)
op_dup (pc,p,(x:s)) = ((pc+1,p,x:x:s),None)

op_ppc :: State -> (State, DeviceIO)
op_ppc (pc,p,s) = ((pc+1,p,(pc+1):s),None)

op_get :: State -> (State, DeviceIO)
op_get (pc,p,s) = ((pc+1,p,s),Get)

op_put :: State -> (State, DeviceIO)
op_put (pc,p,s) = ((pc+1,p,s),Put)

op_br :: State -> (State, DeviceIO)
op_br (pc,p,(0:y:s)) = ((pc+1,p,s),None)
op_br (pc,p,(x:y:s)) = ((y,p,s),None)