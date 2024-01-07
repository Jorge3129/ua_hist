-- Taken from https://gist.github.com/sgronblo/e3d73a61c5dd968b7d29
module Roman where

import Control.Applicative hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String

-- Parses a roman numeral based on base low mid top,
-- eg genericRomanize 809 100 'C' 'D', 'M' -> "DCCC"
genericRomanize :: Int -> Int -> Char -> Char -> Char -> String
genericRomanize n base low mid top
  | n >= 9 * base = [low, top]
  | n >= 5 * base = mid : replicate ((n - 5 * base) `div` base) low
  | n >= 4 * base = [low, mid]
  | otherwise = replicate (n `div` base) low

romanize :: Int -> String
romanize n
  | n >= 1000 = replicate (n `div` 1000) 'M' ++ romanize (n `mod` 1000)
  | n >= 100 = genericRomanize n 100 'C' 'D' 'M' ++ romanize (n `mod` 100)
  | n >= 10 = genericRomanize n 10 'X' 'L' 'C' ++ romanize (n `mod` 10)
  | n >= 1 = genericRomanize n 1 'I' 'V' 'X'
  | n == 0 = ""
  | otherwise = error $ "invalid input to romanize " ++ show n ++ " must be non-negative"

type IntegerParser = Parser Int

-- The full parser for parsing an arabic numeral from a roman
parseRoman :: IntegerParser
parseRoman = do
  thousand <- manyUpToN (char 'M') 3 >>= \s -> return $ length s * 1000
  hundred <- genericRomanParse 100 'C' 'D' 'M'
  ten <- genericRomanParse 10 'X' 'L' 'C'
  one <- genericRomanParse 1 'I' 'V' 'X'
  return $ thousand + hundred + ten + one

-- Combinator that matches {0,n} times in regexp terminology
manyUpToN :: ParsecT s u m a -> Int -> ParsecT s u m [a]
manyUpToN p n
  | n <= 0 = pure []
  | otherwise = liftA2 (:) p (manyUpToN p (n - 1)) <|> pure []

-- Parses a roman part from an arabic numeral
-- eg. parsing "DCCCIX" with genericRomanParse 100 'C' 'D' 'M' would parse
-- out 800 and leave "IX" unparsed
genericRomanParse :: Int -> Char -> Char -> Char -> IntegerParser
genericRomanParse base low mid top = try oneUnderTop <|> try overMid <|> try oneUnderMid <|> try overLow
  where
    oneUnderTop = string [low, top] >> return (9 * base)
    overMid = char mid >> manyUpToN (char low) 3 >>= (\s -> return $ (length s + 5) * base)
    oneUnderMid = string [low, mid] >> return (4 * base)
    overLow = manyUpToN (char low) 3 >>= (\s -> return $ length s * base)

arabize :: String -> Either ParseError Int
arabize = parse parseRoman ""