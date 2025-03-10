{-# LANGUAGE InstanceSigs #-}

module A6 where

import Provided

import Data.List( intersperse, sort, elemIndex, intercalate)
import Data.List ( intersperse, sort )
import Data.Char ( isAlpha, toLower, toUpper )

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

matchElem :: Char -> String -> [Int]
matchElem n xs = [y | (y,z) <- zip [1..] xs, z==n]


-- Start with underscores equaling secret phrase
-- check move if character matches any element in secret
-- take indices of matching characters and replace _ with move char
-- Guess is what has been guessed so far

revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m _ [] = []
revealLetters m [] _ = []
revealLetters m (s:ss) (g:gs) = if m == s then m : revealLetters m ss gs else g : revealLetters m ss gs

-- Q#06

updateChances :: Move-> Secret -> Chances -> Chances
updateChances m s c = if (toUpper m) `elem` s then c else c-1

-- Q#07

setSecret :: IO String
setSecret = do
  putStr "Enter a secret word:\t"
  showInput False
  s <- getLine
  showInput True
  _SPACE_ 
  return s

-- *** A6-1: Records & Instances *** --

-- Q#08
data Game = Game { secret :: Secret
                 , thisGuess :: Guess
                 , moveList  :: [Move]
                 , chances   :: Chances}
                 {- deriving (Eq, Show) -}

-- Q#09

repeatedMove :: Move -> Game -> Bool
repeatedMove m g = m `elem` (moveList g)

-- Q#10




makeGame :: Secret -> Game
makeGame s = Game (map toUpper s) (map (const '_') [1.. (length s)] ) [] _CHANCES_

-- Q#11

updateGame :: Move -> Game -> Game
updateGame m g = g {thisGuess = updateG, moveList = updateM, chances = updateC }
  where
    updateG   = revealLetters (toUpper m) (secret g) (thisGuess g)
    updateM   = (toUpper m) : (moveList g)
    updateC   = updateChances m (secret g) (chances g)

-- revealLetters 'a' (secret z) (thisGuess z) -- update the guess field
-- 'm':(moveList z) -- Add the move character 
-- updateChances m (secret z) (chances z)

-- Q#12

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]

{- z = makeGame "lalala"
zz = updateGame 'm' z
zzz = updateGame 'a' zz
(showGameHelper (thisGuess zzz) (moveList zzz) (chances zzz)) -}

instance Show Game where
  show :: Game -> String
  show g = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' (thisGuess g) ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort (moveList g)) ++ "\n"
    , "\tChances:\t" ++ show (chances g)
    , _STARS_
    ]



-- Q#13

-- data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver

instance Show GameException where
  show :: GameException -> String
  show InvalidWord = unlines [
        _STARS_
        , "\tYou Entered An Invalid Word!"
        , _STARS_
        ]
  show InvalidMove = unlines [
        _STARS_
        , "\tYou Entered An Invalid Move!"
        , _STARS_
        ]
  show RepeatMove = unlines [
        _STARS_
        , "\tYou Entered A Letter That Has Already Been Tried"
        , _STARS_
        ]
  show GameOver = unlines [
        _STARS_
        , "\tGame Over! Try Again?"
        , _STARS_
        ]



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