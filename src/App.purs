module App (appRoot) where

import Prelude

import ColorGuessr.Utils (cls)
import ColorPicker.Component as ColorPicker
import ColorPicker.HSV (hsvInput)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

data Action = Noop

appRoot :: forall query input output. H.Component query input output Aff
appRoot = H.mkComponent
  { initialState: const {}
  , render
  , eval: H.mkEval H.defaultEval
  }

type ChildSlots = (colorPicker :: forall query. H.Slot query ColorPicker.Message Unit)

render :: {} -> HH.ComponentHTML Action ChildSlots Aff
render {} = HH.div [ cls "hoge" ]
  [ HH.slot (Proxy :: _ "colorPicker") unit ColorPicker.component
      (hsvInput $ ColorPicker.Color 0.0 0.0 0.0)
      (const Noop)
  ]
