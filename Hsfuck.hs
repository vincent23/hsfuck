module Main where
import System.Environment ( getArgs )
import Data.Char( ord, chr )
import Data.Array.IO
import System.IO
import Data.Array.Unboxed
import qualified Control.Monad.State as State
import Data.Maybe (mapMaybe)
import Data.List (foldl')

main :: IO ()
main = do args <- getArgs
          source <- readFile (head args)
          state <- initState $ (optimize . parse) source
          State.evalStateT next state

memsize :: Int
memsize = 65535

data Instruction = Plus Int |
                   Move Int |
                   Print |
                   Read |
                   StartLoop |
                   EndLoop deriving(Show, Eq)

data BFState = BFState {
    program :: Array Int Instruction,
    pc :: {-# UNPACK #-} !Int,
    dataIndex :: {-# UNPACK #-} !Int,
    memory :: IOUArray Int Int,
    jumpTargets :: UArray Int Int
}
instance Show BFState where
    show (BFState _ pc' dataIndex' _ _) = show pc' ++ " " ++ show dataIndex' ++ " "

parse :: String -> [Instruction]
parse = mapMaybe instruction
    where instruction x = case x of
                            '-' -> Just $ Plus (-1)
                            '+' -> Just $ Plus 1
                            '<' -> Just $ Move (-1)
                            '>' -> Just $ Move 1
                            '.' -> Just Print
                            ',' -> Just Read
                            '[' -> Just StartLoop
                            ']' -> Just EndLoop
                            _ -> Nothing

optimize :: [Instruction] -> [Instruction]
optimize = optGroup

optGroup :: [Instruction] -> [Instruction]
optGroup = reverse . foldl' f []
    where f (Plus x:xs) (Plus y) = Plus (x+y):xs
          f (Move x:xs) (Move y) = Move (x+y):xs
          f xs x = x:xs

bracketJumps :: [Instruction] -> UArray Int Int
bracketJumps instructions = ((array (0, length instructions - 1)) . snd) . foldl' f ([], []) . zip [0..] $ instructions
    where f (stack, acc) (left, StartLoop) = (left:stack, acc)
          f (left:stack, acc) (right, EndLoop) = (stack, (left, right):(right,left):acc)
          f x _ = x


initState :: [Instruction] -> IO BFState
initState instructions = do memory' <- newArray (0, memsize - 1) 0 :: IO (IOUArray Int Int)
                            return $ BFState program' 0 0 memory' jumpTargets'
    where program' = listArray (0, length instructions - 1) instructions
          jumpTargets' = bracketJumps instructions

getMem :: BFState -> IO Int
{-# INLINE getMem #-}
getMem state = readArray (memory state) (dataIndex state)

setMem :: BFState -> Int -> IO BFState
{-# INLINE setMem #-}
setMem state value = do writeArray (memory state) (dataIndex state) value
                        return state

addMem :: BFState -> Int -> IO BFState
{-# INLINE addMem #-}
addMem state diff = do value <- getMem state
                       setMem state $ value + diff

move :: BFState -> Int -> BFState
{-# INLINE move #-}
move state d = state {dataIndex = dataIndex state + d}

getInstr :: BFState -> Instruction
{-# INLINE getInstr #-}
getInstr state = program state ! pc state

isEnd :: BFState -> Bool
{-# INLINE isEnd #-}
isEnd state = pc state > (snd . bounds . program) state

bracketJump :: BFState -> BFState
{-# INLINE bracketJump #-}
bracketJump state = state {pc = jumpTargets state ! pc state}

next :: State.StateT BFState IO ()
next = do state <- State.get
          state' <- State.liftIO $ nextState state
          State.put state'
          State.unless (isEnd state') next

incPC :: BFState -> BFState
{-# INLINE incPC #-}
incPC state = state {pc = pc state + 1}

nextState :: BFState -> IO BFState
{-# INLINE nextState #-}
nextState state = State.liftM incPC $ case getInstr state of
                       Plus n -> addMem state n
                       Move n -> return $ move state n
                       Print -> do value <- getMem state
                                   putChar $ chr value
                                   hFlush stdout
                                   return state
                       Read ->  do c <- getChar
                                   setMem state $ ord c
                       StartLoop -> do value <- getMem state
                                       return $ if value == 0
                                         then bracketJump state
                                         else state
                       EndLoop -> do value <- getMem state
                                     return $ if value /= 0
                                       then bracketJump state
                                       else state

