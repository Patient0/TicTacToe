module Game where
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State
type Square = (Int, Int)
type Squares = [Square]
type Placement = (Square, Char)
type Placements = [Placement]
data Board = Board {
    placements :: Placements,
    nextToMove :: Char
    }

-- X always goes first
empty = Board [] 'X'

rows = 3
cols = rows

flipxo :: Char -> Char
flipxo 'X' = 'O'
flipxo 'O' = 'X'

lastToMove = flipxo . nextToMove

-- place an 'X' or an 'O' at square s on the board
move :: Square -> Board -> Board
move s (Board ps n) = Board ((s,n):ps) (flipxo n)

charAt :: Board -> Square -> Char
charAt b s = fromMaybe ' ' $ lookup s $ placements b

bar :: String
bar = " +" ++ replicate cols '-' ++ "+"
topLine :: Board -> String
topLine b = [nextToMove b] ++ " " ++ concatMap show [1..cols]

boardLines :: Board -> [String]
boardLines b = [topLine b, bar] ++
    map buildRow [1..rows] ++
    [bar]
    where buildRow r = show r ++ "|" ++ [charAt b (r,c) | c <- [1..cols]] ++ "|"

instance Show Board where
    show b = unlines $ boardLines b

applyMoves :: Board -> [Square] -> Board
applyMoves = foldr move

unfinished = applyMoves empty [(1,3), (3,3), (2,2), (1,1)]
xwin = applyMoves unfinished [(3,2), (2,3), (3,1)]
owin = applyMoves unfinished [(3,1), (3,2)]

full = applyMoves unfinished [(2,1), (1,2), (2,3), (3,1), (3,2)]

nearlywon = move (2,3) unfinished
odoomed = move (3,1) unfinished

type Line = [Square]
row r = [(r,c) | c <- [1..cols]]
col c = [(r,c) | r <- [1..rows]]
diag1 = [(r,r) | r <- [1..rows]]
diag2 = [(r,rows+1-r) | r <- [1..rows]]
winningLines = [f r | r <- [1..rows], f <- [row, col]] ++ [diag1, diag2]

isWinningLine :: Board -> Line -> Bool
isWinningLine b l = 
        all (== lastToMove b) $ map (charAt b) l

hasWinner :: Board -> Bool
hasWinner b = any (isWinningLine b) winningLines

allsquares = [(x,y) | x <- [1..rows], y <- [1..cols]]

-- Get the free squares on this board
free :: Board -> [Square]
free b = [s | s <- allsquares, ' ' == charAt b s]

scoreForMove :: Board -> Square -> Int
scoreForMove b m = score $ move m b

-- This is the main logic for the AI.
-- Minimax algorithm.
-- For each of the moves we can make, what
-- score would our opponent achieve?
-- Pick the minimum of these - that's the move we would make.
-- Our score is the negative of their
-- score because it's a zero sum game
score :: Board -> Int
score b
    -- The other guy has already won. We've lost.
    | hasWinner b = -1
    | otherwise = case [scoreForMove b m | m <- free b] of
        [] -> 0 -- no moves left, no winner. Draw.
        (s:ss) -> -foldr min s ss

type BoardCache = Map.Map String Int
type ScoreState = State BoardCache

boardKey :: Board -> String
boardKey b = [charAt b (r,c) | r <- [1..rows], c <- [1..cols]]

scoreForMoveM :: Board -> Square -> ScoreState Int
scoreForMoveM b m = checkScoreM $ move m b

scoreM :: Board -> ScoreState Int
scoreM b
    | hasWinner b = return $ -1
    | otherwise = do
        scores <- sequence [scoreForMoveM b m | m <- free b]
        case scores of
            [] -> return 0 -- no moves left, no winner. Draw
            (s:ss) -> return $ -foldr min s ss

updateCache :: String -> Int -> ScoreState Int
updateCache key s = do
    cache <- get
    put (Map.insert key s cache)
    return s

checkScoreM :: Board -> ScoreState Int
checkScoreM b = do
    cache <- get
    case Map.lookup key cache of
        Just s -> return s -- seen it before
        Nothing -> do
                   s <- scoreM b
                   updateCache key s
    where key = boardKey b

checkScoreS :: Board -> (Int, BoardCache)
checkScoreS b = runState (checkScoreM b) Map.empty

-- Get available moves and their associated score
moves :: Board -> [(Square, Int)]
moves b = [(m, scoreForMove b m) | m <- free b]

movesM :: Board -> ScoreState [(Square, Int)]
movesM b = sequence [sm s | s <- free b]
    where sm s = do
            score <- scoreForMoveM b s
            return (s, score)


-- choose the move that minimizes the opponents
-- score
bestMoveM :: Board -> ScoreState Square
bestMoveM b = do
    (m:ms) <- movesM b
    return $ fst $ foldr best m ms
                where best m1@(p1,s1) m2@(p2,s2)
                        | s1 > s2 = m2
                        | otherwise = m1

bestMove bc b = runState (bestMoveM b) bc

validate :: BoardCache -> Board -> Square -> IO (Square, BoardCache)
validate bc b s
    | s `elem` free b = return (s, bc)
    | otherwise = getNextMove bc b

getNextMove :: BoardCache -> Board -> IO (Square, BoardCache)
getNextMove bc b
    | nextToMove b == 'O' = return $ (s, bc')
    | otherwise = do
        putStrLn "Enter move - e.g. (2,3)"
        nextMove <- getLine
        validate bc b $ read nextMove
    where (s,bc') = bestMove bc b

readAndApply :: BoardCache -> Board -> IO (Board, BoardCache)
readAndApply bc b =
    do
        print b
        (nextMove, bc') <- getNextMove bc b
        return $ (move nextMove b, bc')

gameLoop :: BoardCache -> Board -> IO (Board, BoardCache)
gameLoop bc b | hasWinner b = return (b, bc)
gameLoop bc b | null $ free b = return (b, bc)
gameLoop bc b = do
    (nextMove, bc') <- readAndApply bc b
    gameLoop bc' nextMove

main :: IO ()
main = do
        putStrLn "Tic Tac Toe!!!"
        (winningBoard, bc) <- gameLoop Map.empty empty
        putStrLn $ show winningBoard ++ "\nEnd of game"
        putStrLn $ show (Map.size bc) ++ " entries in cache"
