module Main where

import Numeric 

type MIDI = Int

data Note = C | C' | D | D' | E | F | F' | G | G' | A | A' | B deriving (Eq, Ord, Enum, Bounded)

instance Show Note where
    show C  = "C"
    show C' = "C#"
    show D  = "D"
    show D' = "D#"
    show E  = "E"
    show F  = "F"
    show F' = "F#"
    show G  = "G"
    show G' = "G#"
    show A  = "A"
    show A' = "A#"
    show B  = "B"

type Octave = Int

newtype Pitch = Pitch { unPitch :: (Note, Octave) }

instance Show Pitch where
    show (Pitch { unPitch = (n, o) }) = show n ++ show o

data Mode
    = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian
    deriving (Eq, Ord, Enum, Bounded, Show)

pitch :: Note -> Octave -> Pitch
pitch n o = Pitch (n, o)

fromMIDI :: MIDI -> Pitch
fromMIDI d = let (i, j) = (d - 12) `divMod` 12 in Pitch (toEnum j, i)

toMIDI :: Pitch -> MIDI
toMIDI p = let (n, o) = unPitch p in 12 + fromEnum n + o * 12

intervals :: [Int]
intervals = [ 2, 2, 1, 2, 2, 2, 1 ]

rotate :: [Int] -> [Int]
rotate (x:xs) = xs ++ [ x ]

generate :: Int -> Int -> [Int]
generate n m = scanl (+) n $ iterate rotate intervals!!m

scale :: Pitch -> Mode -> [Pitch]
scale pitch mode = map fromMIDI . generate (toMIDI pitch) $ fromEnum mode

freq :: MIDI -> Double
freq d = 440*2**((fromIntegral d - 69)/12)

f2s :: Double -> String
f2s x = showFFloat (Just 2) x ""

cv :: MIDI -> Double
cv d = fromIntegral (d - toMIDI (pitch A 4)) / 12

v2s :: Double -> String
v2s x = showFFloat (Just 4) x ""

main :: IO ()
main = do
    putStrLn "MIDI\tPitch\tFreq\tCV"
    putStrLn "-------\t-------\t-------\t-------"
    mapM_ (\d -> putStrLn $ show d ++ "\t" ++ show (fromMIDI d) ++ "\t" ++ f2s (freq d) ++ "\t" ++ v2s (cv d))
        [ toMIDI (pitch A 3) .. toMIDI (pitch A 5) ]
    putStrLn ""
    mapM_ (\m -> print (m,  scale (pitch C 4) m)) [Ionian .. Locrian]
