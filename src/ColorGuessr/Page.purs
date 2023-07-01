module ColorGuessr.Page
  ( Page(..)
  ) where

import Prelude

import Color (Color)

data Page = Guess | GuessResult Color Color
