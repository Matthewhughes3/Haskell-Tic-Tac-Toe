import Data.List (transpose, delete, elemIndex, findIndex, find, intersect)
import System.Random

type Board = String
type XO = Char
type Space = Char

main :: IO ()
main = do 
    gen <- getStdGen
    let (r,_) = random gen :: (Int, StdGen)
    let xo = case odd r of 
                False -> 'x'
                True -> 'o'
    let space = show ((r `mod` 9) + 1) !! 0
    let (endBoard, winner) = playGame board xo defaultPriority space
    if winner == 'n'
        then putStrLn "Tie!"
        else putStrLn (show winner ++ " wins!")
    putStrLn endBoard

opposite :: XO -> XO
opposite 'x' = 'o'
opposite 'o' = 'x'

board :: Board
board = "123\n456\n789"

getBoardStates :: Board -> [String]
getBoardStates board = boardList ++ transpose boardList ++ getBoardDiagonals boardList
    where boardList = lines board

getWeightedBoardStates :: XO -> Board -> [(Int,Int)]
getWeightedBoardStates xo board = let boardStates = getBoardStates board
                                      xCounts = map (length . filter (=='x')) boardStates
                                      oCounts = map (length . filter (=='o')) boardStates
                                  in  if xo == 'x' 
                                        then zip xCounts oCounts 
                                        else zip oCounts xCounts

-- TODO: Try and make this more efficient
getBoardDiagonals :: [String] -> [String]
getBoardDiagonals boardList = [boardList !! 0 !! 0:boardList !! 1 !! 1:boardList !! 2 !! 2:[], boardList !! 0 !! 2:boardList !! 1 !! 1:boardList !! 2 !! 0:[]]

makeMove :: XO -> Space -> Board -> Board
makeMove xo space board = insertElem xo (delete space board)
                            where (Just i) = space `elemIndex` board
                                  insertElem xo board = let (l,r) = splitAt i board
                                                        in l ++ [xo] ++ r

type Strategy = (Int,Int)
type Priority = [Strategy]

defaultPriority :: Priority
defaultPriority = [(2,0),(0,2),(0,1),(1,0),(0,0),(1,1)]

getSpaceFromStrategy :: Strategy -> XO -> Board -> Maybe Space
getSpaceFromStrategy strategy xo board = let weightedBoardStates = getWeightedBoardStates xo board
                                             boardStates = getBoardStates board
                                             index = findIndex (==True) (map (==strategy) weightedBoardStates)
                                         in  case index of 
                                             Nothing -> Nothing
                                             (Just i) -> case find (\x -> x /= 'x' && x /= 'o') (boardStates !! i) of
                                                           Nothing -> Nothing
                                                           (Just space) -> Just space

determineMove :: Priority -> XO -> Board -> Maybe Space
determineMove [] xo board = Nothing
determineMove (s:ss) xo board = case getSpaceFromStrategy s xo board of
                                    Nothing -> determineMove ss xo board
                                    (Just s) -> Just s

checkVictory :: Board -> Maybe XO
checkVictory board = if anyWins 'x' then Just 'x' else if anyWins 'o' then Just 'o' else if null $ intersect "123456789" board then Just 'n' else Nothing
                     where boardStates = getBoardStates board
                           anyWins c = or $ map (all (==c)) boardStates
                
playGame :: Board -> XO -> Priority -> Space -> (Board,XO)
playGame board xo priority space = case checkVictory board of
                                        (Just x) -> (board, x)
                                        Nothing -> playGame board' xo' priority space'
                                   where board' = makeMove xo space board
                                         xo' = opposite xo
                                         (Just space') = determineMove priority xo' board'