{-|
Module      : Data.Sic.Types
Description : UK Standard Industrial Classification of Economic Activities 2007
Copyright   : Copyright 2018 Mike Pilgrem
Licence     : See LICENSE (BSD-3-Clause)
-}

module Data.Sic.Types
  ( UkSic2007 (UkSic2007)
  ) where

-- |A representation of a UK SIC (2007).
newtype UkSic2007 = UkSic2007 Int deriving (Eq, Ord, Show)
