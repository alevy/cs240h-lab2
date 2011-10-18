module Main where

import Data.Bits
import Data.List
import Data.Time.Clock
import Data.Int
import Data.String.Utils
import System.Environment
import System.IO

-- Constants
dimension = 17
capacity = 100

-- Types
type HilbertValue = Int64

data Point = Point Int Int deriving (Show, Eq, Ord)

data Shape = Shape {plow :: Point, phigh :: Point, hv :: HilbertValue}
              deriving (Show, Eq)

instance Ord Shape where
  compare one two = let hvDiff = compare (hv one) (hv two)
                        plowDiff = compare (plow one) (plow two)
                        phighDiff = compare (phigh one) (phigh two)
                    in if hvDiff /= EQ then hvDiff else
                      if plowDiff /= EQ then plowDiff else
                        phighDiff

-- Children (Shapes or TreeNodes) are sorted in ascending order by hilbert value
data TreeNode = LeafNode HilbertValue [Shape]
              | InnerNode HilbertValue Point Point [TreeNode]
              deriving (Show, Eq)

instance Ord TreeNode where
  compare (LeafNode _ _) (InnerNode _ _ _ _) = GT
  compare (InnerNode _ _ _ _) (LeafNode _ _) = LT
  compare one two = compare (getHV one) (getHV two)
-- Program

getHV :: TreeNode -> HilbertValue
getHV (LeafNode val _) = val
getHV (InnerNode val _ _ _) = val

-- Assumes first argument is a LeafNode
addShapeToLeaf :: TreeNode -> Shape -> TreeNode
addShapeToLeaf (LeafNode val shapes) shape = LeafNode (max val (hv shape)) (sort (shape:shapes))

boundingBox :: TreeNode -> (Point, Point)
boundingBox (InnerNode _ pl ph nodes) = (pl, ph)
boundingBox (LeafNode _ shapes) = foldl' (\(pl, ph) shape ->
                                      (min pl (plow shape), max ph (phigh shape)))
                                      ((plow $ head shapes), (phigh $ head shapes)) shapes

adjust :: TreeNode -> TreeNode
adjust (LeafNode val shapes) = LeafNode (foldl' (\m shape -> max m (hv shape)) val shapes) shapes
adjust (InnerNode val pl ph cs) = InnerNode maxHV minPl maxPh children
    where children = map adjust cs
          maxHV = foldl' (\m n -> max m (getHV n)) val children
          maxPh = foldl' (\ph node -> max pl $ snd $ boundingBox node)
                 (snd $ boundingBox $ head children) children
          minPl = foldl' (\pl node -> min pl $ fst $ boundingBox node)
                 (fst $ boundingBox $ head children) children

treeInsert :: TreeNode -> Shape -> TreeNode
treeInsert node@(LeafNode val shapes) shape = adjust $
    if (length shapes) < capacity
    then LeafNode val (sort (shape:shapes))
    else InnerNode val (Point 0 0) (Point 0 0) [node, (LeafNode (hv shape) [shape])]
treeInsert node@(InnerNode pl ph val children) shape = adjust $
    let child = unboxChild $ find (\c -> (getHV c) > (hv shape)) children
        unboxChild (Just c) = c
        unboxChild Nothing = last children
    in InnerNode pl ph val $ sort $ (treeInsert child shape):(delete child children)

main :: IO()
main = do
  startTime <- getCurrentTime
  (file:_) <- getArgs
  rawText <- readFile file
  let lines = split "\n" (strip rawText)
  let shapes = sort $ map stringToShape lines
  let root = adjust $ foldl' treeInsert (LeafNode 0 []) shapes
  endTime <- seq root getCurrentTime
  let seconds = floor $ toRational $ diffUTCTime endTime startTime * 1000
  putStrLn $ (show $ length lines) ++ " rectangles read in " ++ (show seconds) ++ " milliseconds"
  mainLoop root

mainLoop root = do
  ineof <- isEOF
  if ineof then return () else do
    query <- getLine
    startTime <- getCurrentTime
    let result = searchTree root $ stringToShape query
    endTime <- seq result getCurrentTime
    let seconds = floor $ toRational $ diffUTCTime endTime startTime * 1000
    putStrLn $ "found " ++ (show $ length result) ++ " matches in " ++ (show seconds) ++ " milliseconds"
    let output = concat $ map (\s -> (show $ plow s) ++ ", " ++ (show $ plow s) ++ "\n") result
    putStrLn output
    mainLoop root

searchTree :: TreeNode -> Shape -> [Shape]
searchTree (LeafNode _ shapes) query = filter (\s -> intersect (plow query, phigh query) (plow s, phigh s)) shapes
  where intersect (Point xl1 yl1, Point xh1 yh1) (Point xl2 yl2, Point xh2 yh2) =
                    xl1 <= xh2 && xh1 >= xl2 && yl1 <= yh2 && yh1 >= yh2
searchTree (InnerNode pl ph _ nodes) query = concat $ map (\n -> searchTree n query) nodes

makeShape :: [Int] -> Shape
makeShape [x1,y1,x2,y2,x3,y3,x4,y4] =
  let xlow = min x1 $ min x2 $ min x3 x4
      ylow = min y1 $ min y2 $ min y3 y4
      xhigh = max x1 $ max x2 $ max x3 x4
      yhigh = max y1 $max y2 $ max y3 y4
  in Shape {plow = Point xlow ylow, phigh = Point xhigh yhigh,
         hv = hilbertDistance dimension (Point ((xlow + xhigh) `div` 4) ((ylow + yhigh) `div` 4))}
makeRect list = error ("Error " ++ (show $ length list))

stringToShape :: String -> Shape
stringToShape line = makeShape $ map read (split "," line)

hilbertDistance :: Int -> Point -> HilbertValue
hilbertDistance d (Point x y)
    | x < 0 || x >= maxPixel = error "x coordinate out of bounds"
    | y < 0 || y >= maxPixel = error "y coordinate out of bounds"
    | otherwise = distance (1 `shiftL` (d - 1)) 0 x y
    where maxPixel = 1 `shiftL` d
          distance 0 result _ _ = result
          distance side result x y
              | x < side && y < side = distance (side `shiftR` 1) result y x
              | x < side = distance (side `shiftR` 1)
                                    (result + area) x (y - side)
              | y < side = distance (side `shiftR` 1) (result + area * 3)
                                    (side - y - 1) (side * 2 - x - 1)
              | otherwise = distance (side `shiftR` 1) (result + area * 2)
                                     (x - side) (y - side)
              -- Allow compiler to convert to Int64
              where area = fromIntegral (side * side)

