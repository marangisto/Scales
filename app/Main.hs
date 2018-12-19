module Main where

import Numeric 

type MIDI = Int

data Note = C | C' | D | D' | E | F | F' | G | G' | A | A' | B deriving (Eq, Ord, Enum, Bounded)

showSharp :: Note -> String
showSharp C  = "C"
showSharp C' = "C#"
showSharp D  = "D"
showSharp D' = "D#"
showSharp E  = "E"
showSharp F  = "F"
showSharp F' = "F#"
showSharp G  = "G"
showSharp G' = "G#"
showSharp A  = "A"
showSharp A' = "A#"
showSharp B  = "B"

showFlat :: Note -> String
showFlat C  = "C"
showFlat C' = "Db"
showFlat D  = "D"
showFlat D' = "Eb"
showFlat E  = "E"
showFlat F  = "F"
showFlat F' = "Gb"
showFlat G  = "G"
showFlat G' = "Ab"
showFlat A  = "A"
showFlat A' = "Bb"
showFlat B  = "B"

instance Show Note where
    show = showSharp

type Octave = Int

newtype Pitch = Pitch { unPitch :: (Note, Octave) }

instance Show Pitch where
    show (Pitch { unPitch = (n, o) }) = show n ++ show o

class Enum a => Mode a where
    intervals :: a -> [Int]
    generate :: Int -> a -> [Int]
    generate n m = scanl (+) n $ iterate rotate (intervals m)!!(fromEnum m)

data Pentatonic
    = Minor
    | Major
    | Egyptian
    | ManGong
    | Ritsusen
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Mode Pentatonic where
    intervals _ = [ 3, 2, 2, 3, 2 ]

data Heptatonic
    = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Mode Heptatonic where
    intervals _ = [ 2, 2, 1, 2, 2, 2, 1 ]

pitch :: Note -> Octave -> Pitch
pitch n o = Pitch (n, o)

fromMIDI :: MIDI -> Pitch
fromMIDI d = let (i, j) = (d - 12) `divMod` 12 in Pitch (toEnum j, i)

toMIDI :: Pitch -> MIDI
toMIDI p = let (n, o) = unPitch p in 12 + fromEnum n + o * 12

rotate :: [Int] -> [Int]
rotate (x:xs) = xs ++ [ x ]

scale :: Mode a => Pitch -> a -> [Pitch]
scale pitch mode = map fromMIDI . generate (toMIDI pitch) $ mode

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
    mapM_ (\(m, d) -> print (m,  scale (pitch d 4) m)) $ zip [Minor .. Ritsusen] [D', F', G', A', C']
