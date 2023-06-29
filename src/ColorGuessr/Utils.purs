module ColorGuessr.Utils
  ( cls
  , data_
  ) where

import Prelude

import Halogen.HTML (AttrName(..), ClassName(..), IProp, attr)
import Halogen.HTML.Properties (class_)

cls :: forall r i. String -> IProp (class :: String | r) i
cls = class_ <<< ClassName

data_ :: forall r i. String -> String -> IProp r i
data_ name = attr $ AttrName ("data-" <> name)

