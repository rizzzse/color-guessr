module ColorPicker.HSVPicker
  ( svPalette
  ) where

import Prelude

import Color (cssStringHSLA, hsva)
import ColorGuessr.Utils (cls)
import ColorPicker.Component (Action(..))
import Control.Plus (empty)
import Data.String (joinWith)
import Halogen (RefLabel)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

svPalette
  :: forall w
   . RefLabel
  -> Number
  -> Number
  -> Number
  -> HH.HTML w Action
svPalette ref h s v = HH.div
  [ HE.onMouseDown $ DragStart ref
  , HP.ref ref
  , cls "sv-palette"
  , HP.style $ joinWith "; "
      [ "--bg-color: " <> (cssStringHSLA $ hsva h s v 1.0)
      , "--hue: " <> show h
      ]
  ]
  [ HH.div
      [ cls "sv-pointer"
      , HP.style $ "translate: "
          <> show (s * 100.0)
          <> "% "
          <> show ((1.0 - v) * 100.0)
          <> "%"
      ]
      empty
  ]
