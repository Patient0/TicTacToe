module Game where
import Data.List
import Data.Maybe
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

free :: Board -> [Square]
free b = [s | s <- allsquares, ' ' == charAt b s]

scoreForMove :: Board -> Square -> Int
scoreForMove b m = score $ move m b

score :: Board -> Int
score b
    -- The other guy has already won. We've lost.
    | hasWinner b = -1
    -- For each of the moves we can make, what is
    -- the maximum score our opponent can achieve?
    -- Pick the minimum of these - that's the move we would make.
    -- Our score is the negative of their
    -- score because it's a zero sum game
    | otherwise = case [scoreForMove b m | m <- free b] of
        [] -> 0 -- no moves left, no winner. Draw.
        (s:ss) -> -(foldr min s ss)

-- Get available moves and their associated score
moves :: Board -> [(Square, Int)]
moves b = [(m, scoreForMove b m) | m <- free b]

-- choose the move that minimizes the opponents
-- score
bestMove :: Board -> Square
bestMove b = let (m:ms) = moves b in
                fst $ foldr best m ms
                where best m1@(p1,s1) m2@(p2,s2)
                        | s1 > s2 = m2
                        | otherwise = m1

validate :: Board -> Square -> IO Square
validate b s
    | s `elem` (free b) = return s
    | otherwise = getNextMove b

getNextMove :: Board -> IO Square
getNextMove b
    | nextToMove b == 'O' = return $ bestMove b
    | otherwise = do
        putStrLn "Enter move - e.g. (2,3)"
        nextMove <- getLine
        validate b $ read nextMove

readAndApply :: Board -> IO Board
readAndApply b =
    do
        print b
        nextMove <- getNextMove b
        return $ move nextMove b

gameLoop :: Board -> IO Board
gameLoop b | hasWinner b = return b
gameLoop b | null $ free b = return b
gameLoop b = do
    nextMove <- readAndApply b
    gameLoop nextMove

main :: IO ()
main = do
        putStrLn "Tic Tac Toe!!!"
        winningBoard <- gameLoop empty
        putStrLn $ show winningBoard ++ "\nEnd of game"
