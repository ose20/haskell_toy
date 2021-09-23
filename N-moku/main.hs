import System.Random
import System.IO
import Data.Char
import Data.List


--------------- type decralation begin ---------------
data Player = O | B Int | X
instance Eq Player where
  O == O = True
  B _ == B _ = True
  X == X = True
  _ == _ = False
instance Ord Player where
  O < B _ = True
  B _ < X = True
  O < X = True
  _ < _ = False
  p <= q = p < q || p == q
  p > q = q < p
  p >= q = q <= p
instance Show Player where
  show O = "O"
  show (B i) = show i
  show X = "X"

type Grid = [[Player]]

data Tree a = Node a [Tree a]
  deriving Show

type Metric = (Player, Int)
--------------- type decralation end -------------------


--------------- constant term begin -------------------
size :: Int
size = 3

depth :: Int
depth = 9

moku :: Int
moku = 3

empty :: Grid
empty = chop size [B i | i <- [0..size^2-1]]
--------------- constant term end ---------------------


--------------- pretty printing begin -----------------------
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate (size*4-1) '-']

showRow :: [Player] -> [String]
showRow = assemble . interleave bar . map showPlayer
  where
    bar = replicate 3 "|"
    assemble = foldr1 (zipWith (++))

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer X = ["   ", " X ", "   "]
showPlayer (B i) = [makespace i, "   ", "   "]

makespace :: Int -> String
makespace i = show i ++ replicate (max 0 (3-l)) ' '
  where l = length (show i)
--------------- pretty printing end -----------------------

nxtp :: Player -> Player
nxtp O = X
nxtp X = O
nxtp (B i) = B i

full :: Grid -> Bool
full g = all written ps
  where
    written p = p == O || p == X
    ps = concat g

wins :: Grid -> Player -> Bool
wins g p = any (line moku p) (rows ++ cols ++ dias)
  where
    rows = g
    cols = transpose g
    dias = [dia g, dia (map reverse g)]
    dia g = [g !! i !! i | i <- [0..size-1]]

won :: Grid -> Bool
won g = wins g O || wins g X

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B i

move :: Grid -> Player -> Int -> [Grid]
move g p i = [chop size (ls ++ [p] ++ rs) | valid g i]
  where (ls, (B _):rs) = splitAt i (concat g)

-- 全部埋まってないのに勝敗が決まる可能性がある
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = [g' | i <- [0..size^2-1], valid g i, g' <- move g p i]

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (nxtp p) | g' <- moves g p]

minimaxtree :: Tree Grid -> Tree (Grid,Player)
minimaxtree (Node g [])
  | wins g O = Node (g, O) []
  | wins g X = Node (g, X) []
  | otherwise = Node (g, B (-1)) []
minimaxtree (Node g gts)
  | turn g == O = Node (g, minimum ps) ots
  | otherwise = Node (g, maximum ps) ots        -- turn g == X
  where
    ots = map minimaxtree gts
    ps = [p | Node (_,p) _ <- ots]

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g',m') _ <- ots', m' == bestm]
  where
    gt = prune depth (gametree g p)
    ot = minimaxtree gt
    Node (_,bestm) ots' = ot


prune :: Int -> Tree a -> Tree a
prune 0 (Node g _) = Node g []
prune n (Node g ts) = Node g [prune (n-1) t | t <- ts]

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1,1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins g O = putStrLn "Player O wins!!\n"
  | wins g X = putStrLn "Player X wins!!\n"
  | full g = putStrLn "It's a draw...\n"
  | p == O = do
      i <- getNat (prompt p)
      case move g p i of
        [] -> do  putStrLn "ERROR: invalid number"
                  play' g p
        [g'] -> play g' (nxtp p)
        other -> putStrLn "ERROR: program error"
  | otherwise = do
      putStr ("Player " ++ show p ++ " is thinking...")
      let moves = bestmoves g p
      i <- randomRIO (0, length moves - 1)
      (play $! (moves !! i)) (nxtp p)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine 
  if xs /= [] && all isDigit xs then
    return (read xs)
  else
    do  putStrLn "ERROR: invalid number"
        getNat prompt

prompt :: Player -> String
prompt p =
  "Player " ++ show p ++ ", Enter your move: "        

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  play empty O
--------------- auxiliary function begin ---------------
cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

-- line n x ys : ys の中に，x が n 個連続で出現するか．
line :: Eq a => Int -> a -> [a] -> Bool
line n x ys = aux n x ys
  where
    aux 0 _ _ = True
    aux m _ [] = False
    aux m x (y:ys)
      | x == y = aux (m-1) x ys
      | otherwise = aux n x ys

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
--------------- auxiliary function end -----------------


--------------- test begin --------------------
g1 = [[B 0, B 1, B 2], [B 3, O, B 5], [X, O, B 8]]
g2 = [[X,O,O],[X,O,O],[O,X,X]]
test1 = move g1 X 4
test2 = move g1 X 1
test3 = moves g1 X
test4 = moves g2 X

--------------- test end ----------------------