module ColorPicker.HueWheel where

import Prelude

import ColorGuessr.Utils (cls)
import ColorPicker.Component (Action(..), ColorSpaceName, colorSpace)
import Control.Plus (empty)
import Halogen (RefLabel)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

hueWheel
  :: forall w
   . ColorSpaceName
  -> RefLabel
  -> Number
  -> Array (HH.HTML w Action)
  -> HH.HTML w Action
hueWheel colorSpaceName ref hue children = HH.div
  [ HE.onMouseDown $ DragStart ref
  , HP.ref ref
  , cls "hue-wheel"
  , colorSpace colorSpaceName
  ]
  [ HH.div
      [ cls "hue-wheel-center"
      , HE.onMouseDown StopPropagation
      ]
      children
  , HH.div
      [ cls "hue-wheel-handle"
      , HP.style $ "rotate: " <> show hue <> "deg"
      ]
      empty
  ]
