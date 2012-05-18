import Control.Monad
import System.Random
import System.Environment (getArgs)

main = do
     dict <- liftM lines $ readFile "dict.txt"
     args <- getArgs
     matrix <- if ((length args) > 0)
                  then return $ args !! 0
                  else buildMatrix
     let tree = buildTree matrix
     print $ filter (contains tree) $ filter (\l -> (length l) > 3) dict

data LetterTree = LetterNode { letter :: Char
                             , position :: Position
                             , children :: [LetterTree]
                             , parents :: [Position]
                             }
                | Root { children :: [LetterTree] }
                deriving (Show, Eq)

data Position = Position { row :: Int
                , column :: Int
                } deriving (Show, Eq)


buildMatrix = do
     r <- randomIO
     return $ take 16 $randomRs ('a', 'z') (mkStdGen r)

buildTree :: [Char] -> LetterTree
buildTree chars = Root $ map (\i -> buildChild (postionFromPair (i `divMod` 4)) []) [0..15]
  where buildChild pos@(Position row column) parentList = LetterNode { letter = chars !! ((4*row)+column)
                                                                     , position = pos
                                                                     , children = getChildren pos (pos:parentList)
                                                                     , parents = pos:parentList
                                                                     }
        getChildren parentPos@(Position row column) parentList' = let neighbors = [ (Position (row-1) (column-1))
                                                                                  , (Position (row-1) (column))
                                                                                  , (Position (row-1) (column+1))
                                                                                  , (Position (row) (column-1))
                                                                                  , (Position (row) (column+1))
                                                                                  , (Position (row+1) (column-1))
                                                                                  , (Position (row+1) (column))
                                                                                  , (Position (row+1) (column+1))
                                                                                  ]
                                                                  in [ buildChild p parentList' | p <- neighbors, (inRange p) && (not(p `elem` parentList'))]

inRange :: Position -> Bool
inRange (Position row column) = (intInRange row) && (intInRange column)
  where intInRange int = (int > -1) && (int < 4)

postionFromPair :: (Int, Int) -> Position
postionFromPair (a,b) = Position { row = a
                                 , column = b
                                 }

contains :: LetterTree -> [Char] -> Bool
contains _ [] = True
contains (LetterNode _ _ [] _) _ = False
contains (Root children)  (x:xs) = or $ map (\t -> contains t xs) $ filter (\(LetterNode i _ _ _) -> x == i) children
contains (LetterNode l _ children _) (x:xs) = or $ map (\t -> contains t xs) $ filter (\(LetterNode i _ _ _) -> x == i) children


