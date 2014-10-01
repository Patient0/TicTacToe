module Game where
import Data.List
import Data.Maybe
type Square = (Int, Int)
type Squares = [Square]
type Placement = (Square, Char)
type Placements = [Placement]
data Board = Board {
    placements :: Placements,
    next :: Char
    }

-- X always goes first
empty = Board [] 'X'

rows = 3
cols = rows

flipxo :: Char -> Char
flipxo 'X' = 'O'
flipxo 'O' = 'X'

lastMove = flipxo . next

-- place an 'X' or an 'O' at square s on the board
move :: Square -> Board -> Board
move s (Board ps n) = Board ((s,n):ps) (flipxo n)

charAt :: Board -> Square -> Char
charAt b s =
    case lookup s (placements b) of
        Just c -> c
        Nothing -> ' '

bar :: String
bar = " +" ++ (replicate cols '-') ++ "+"
topLine :: Board -> String
topLine b = [next b] ++ " " ++ (concat $ map show [1..cols])

boardLines :: Board -> [String]
boardLines b = [topLine b, bar] ++
    (map buildRow [1..rows]) ++
    [bar]
    where buildRow r = (show r) ++ "|" ++ [charAt b (r,c) | c <- [1..cols]] ++ "|"

instance Show Board where
    show b = concat $ map (++ "\n") $ boardLines b

applyMoves :: Board -> [Square] -> Board
applyMoves = foldr move

unfinished = applyMoves empty [(1,3), (3,3), (2,2), (1,1)]
xwin = applyMoves unfinished [(3,2), (2,3), (3,1)]
owin = applyMoves unfinished [(3,1), (3,2)]

type Line = [Square]
row r = [(r,c) | c <- [1..cols]]
col c = [(r,c) | r <- [1..rows]]
diag1 = [(r,r) | r <- [1..rows]]
diag2 = [(r,rows+1-r) | r <- [1..rows]]
winningLines = [f r | r <- [1..rows], f <- [row, col]] ++ [diag1, diag2]

isWinningLine :: Board -> Line -> Bool
isWinningLine b l = 
        all (== lastMove b) $ map (charAt b) l

hasWinner :: Board -> Bool
hasWinner b = any (isWinningLine b) winningLines

readAndApply :: Board -> IO Board
readAndApply b =
    do
        putStrLn $ show b
        nextMove <- getLine
        return $ move (read nextMove) b

allsquares = [(x,y) | x <- [1..rows], y <- [1..cols]]

free :: Board -> [Square]
free b = [s | s <- allsquares, ' ' == charAt b s]

gameLoop :: Board -> IO Board
gameLoop b | hasWinner b = return b
gameLoop b = do
    next <- readAndApply b
    gameLoop next

main :: IO ()
main = do
        putStrLn "Tic Tac Toe!!!"
        winningBoard <- gameLoop empty
        putStrLn $ (show winningBoard) ++ "\nEnd of game"
