module ColorPicker.HSV
  ( hsvInput
  ) where

import Prelude

import ColorGuessr.Utils (cls)
import ColorPicker.Component (Action, Color(..), ColorSpaceName(..), Input)
import ColorPicker.HSVPicker (svPalette)
import ColorPicker.HueWheel (hueWheel)
import Data.Int (floor)
import Data.Number (atan2, tau)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

hsvHueWheelRef :: H.RefLabel
hsvHueWheelRef = H.RefLabel "HSVHueWheel"

hsvPaletteRef :: H.RefLabel
hsvPaletteRef = H.RefLabel "SVPalette"

renderHSVColorPicker :: forall slots. Color -> HH.ComponentHTML Action slots Aff
renderHSVColorPicker (Color h s v) =
  HH.div [ cls "hsv-color-picker" ]
    [ hueWheel (ColorSpaceName "hsv") hsvHueWheelRef h
        [ HH.div [ cls "hue-info" ]
            [ HH.p_ [ HH.text "Hue - HSL / HSV(HSB)" ]
            , HH.text $ show $ floor h `mod` 360
            ]
        ]
    , svPalette hsvPaletteRef h s v
    ]

clamp1 :: Number -> Number
clamp1 = clamp 0.0 1.0

onDragHSVColorPicker
  :: H.RefLabel
  -> { x :: Number, y :: Number }
  -> Color
  -> Color
onDragHSVColorPicker ref { x, y } (Color h s v)
  | ref == hsvPaletteRef =
      Color h (clamp1 x) (1.0 - clamp1 y)
  | ref == hsvHueWheelRef =
      Color (hue / tau * 360.0) s v
      where
      hue = atan2 (y - 0.5) (x - 0.5)
  | otherwise = Color h s v

hsvInput :: forall slots. Color -> Input slots
hsvInput color =
  { color
  , render: renderHSVColorPicker
  , onDrag: onDragHSVColorPicker
  }
