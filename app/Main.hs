module Main where

data Note
    = C
    | C'
    | D
    | D'
    | E
    | F
    | F'
    | G
    | G'
    | A
    | A'
    | B
    deriving (Eq, Ord, Enum, Bounded, Show)

data Mode
    = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian
    deriving (Eq, Ord, Enum, Bounded, Show)

intervals :: [Int]
intervals = [ 2, 2, 1, 2, 2, 2, 1 ]

rotate :: [Int] -> [Int]
rotate (x:xs) = xs ++ [ x ]

generate :: Int -> Int -> [Int]
generate n m = scanl (+) n $ iterate rotate intervals!!m

scale :: Note -> Mode -> [Note]
scale note mode = map (toEnum . (`mod` 12)) . generate (fromEnum note) $ fromEnum mode

main :: IO ()
main = do
    mapM_ (print . scale C) [Ionian .. Locrian]

