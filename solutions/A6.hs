module A6 where

import Provided

import Data.List( intersperse, sort, elemIndex)

-- *** A6-0: WARM-UP *** --

-- Q#01

type Chances = Int
type Guess   = String
type Move    = Char
type Secret  = String
type Dictionary = [String]


-- Q#02
data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver

-- Q#03

{- _LENGTH_ :: Int
_LENGTH_ = 20 -}

lengthInRange :: Secret -> Bool
lengthInRange s = (length s >= fst _LENGTH_) && (length s <= snd _LENGTH_)

-- Q#04

invalidMove :: Move -> Bool
invalidMove m = not(('a' <= m && m <= 'z') || ('A' <= m && m <= 'Z'))

-- Q#05

getNumber :: Maybe Int -> Int
getNumber (Just n) = n
getNumber _    = 0

matchElem :: Char -> String -> [Int]
matchElem n xs = [y | (y,z) <- zip [1..] xs, z==n]

replace _ _ [] = []
replace k _ xs | k < 0 = xs
replace 0 c (x:xs) = c : xs
replace k c (x:xs) = x : replace (k-1) c xs

revealLetters :: Move -> Secret -> Guess -> Guess
-- Start with underscores equaling secret phrase
-- check move if character matches any element in secret
-- take indices of matching characters and replace _ with move char
-- Guess is what has been guessed so far
revealLetters m s g = (replicate (length(s)) '_')

(matchElem m s)

-- elemIndex 'm' "Hellom"
-- (zipWith (,) [1..] s)

-- Q#06

updateChances :: Move-> Secret -> Chances -> Chances
updateChances m s c = undefined

-- Q#07

setSecret :: IO ()
setSecret = do
  putStr "Enter a secret word:\t"
-- *** A6-1: Records & Instances *** --

-- Q#08
data Game

-- Q#09

repeatedMove = undefined

-- Q#10

makeGame = undefined

-- Q#11

updateGame = undefined

-- Q#12

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]


-- Q#13


-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe = undefined

-- Q#15

validateSecret = undefined

-- Q#16

hasValidChars = undefined


isValidLength = undefined


isInDict = undefined

-- Q#17

validateNoDict = undefined

validateWithDict = undefined

-- Q#18

processTurn = undefined