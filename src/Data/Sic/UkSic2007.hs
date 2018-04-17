{-|
Module      : Data.Sic.UkSic2007
Description : UK Standard Industrial Classification of Economic Activities 2007
Copyright   : Copyright 2018 Mike Pilgrem
Licence     : See LICENSE (BSD-3-Clause)

-}

module Data.Sic.UkSic2007
  ( UkSic2007 (UkSic2007)
  , industry
  , isSic
  , toSic
  , ukSic2007P
  ) where

import Data.Char (digitToInt, isDigit)
import Data.Text (Text, unpack)
import Text.ParserCombinators.ReadP (ReadP, (<++), char, count, readP_to_S,
  satisfy)

import qualified Data.HashMap.Strict as HM (lookup)
import Data.HashMap.Strict (HashMap, fromList, member)

import Data.Sic.Types (UkSic2007 (UkSic2007))
import qualified Data.Sic.UkSic2007.Internal as I (ukSic2007)

-- |Look up the industry description for a purported UK SIC (2007). Returns
-- 'Nothing' if the code is invalid.
industry :: UkSic2007 -> Maybe Text
industry (UkSic2007 code) = HM.lookup code ukSic2007

-- |Check if a purported UK SIC (2007) is valid.
isSic :: UkSic2007 -> Bool
isSic (UkSic2007 code) = member code ukSic2007

-- |Attempt to create a valid UK SIC (2007), based on divisions (\"01\"),
-- groups (\"01.2\" or \"012\"), classes (\"01.23\" or \"0123\") or subclasses
-- (\"01.23\\4\" or \"01234\").
toSic :: Text -> Maybe UkSic2007
toSic s
  | [(code, [])] <- p = if isSic code then Just code else Nothing
  | otherwise       = Nothing
 where
  p = readP_to_S ukSic2007P (unpack s)

ukSic2007 :: HashMap Int Text
ukSic2007 = fromList I.ukSic2007

-- |A parser for a UK SIC (2007). Processes divisions (\"01\"), groups (\"01.2\"
-- or \"012\"), classes (\"01.23\" or \"0123\") or subclasses (\"01.23\\4\" or
-- \"01234\").
ukSic2007P :: ReadP UkSic2007
ukSic2007P =
  fmap UkSic2007 (plainP <++ subclassP <++ classP <++ groupP <++ divisionP)

plainP :: ReadP Int
plainP = foldr1 (<++) $ fmap plainP' [5, 4 .. 3]

plainP' :: Int -> ReadP Int
plainP' n = do
  x <- read <$> count n digit
  return $ x * 10 ^ (5 - n)

subclassP :: ReadP Int
subclassP = do
  x     <- classP
  _     <- char '\\'
  subclass <- digitToInt <$> digit
  return $ x + subclass

classP :: ReadP Int
classP = do
  x     <- groupP
  sicClass <- digitToInt <$> digit
  return $ x + sicClass * 10

groupP :: ReadP Int
groupP = do
  x     <- divisionP
  _     <- char '.'
  group <- digitToInt <$> digit
  return $ x + group * 100

divisionP :: ReadP Int
divisionP = do
  division <- read <$> count 2 digit
  return $ division * 1000

digit :: ReadP Char
digit = satisfy isDigit
