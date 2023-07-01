module ColorGuessr.Page.GuessResult
  ( guessResultPage
  ) where

import Prelude

import Color (Color, distance, toHexString)
import ColorGuessr.Page as Page
import ColorGuessr.Utils (cls)
import ColorPicker.Component as ColorPicker
import Control.Plus (empty)
import Data.Int (floor)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = { correct :: Color, answer :: Color }

guessResultPage :: forall query. H.Component query State Page.Page Aff
guessResultPage = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
  }

type ChildSlots = (colorPicker :: forall query. H.Slot query ColorPicker.Message Unit)

render :: State -> HH.ComponentHTML Unit ChildSlots Aff
render { correct, answer } =
  HH.div_
    [ HH.p_ [ HH.text "distance: ", HH.text $ show d ]
    , HH.p_ [ HH.text "point: ", HH.text $ show point, HH.text " / 5000" ]
    , HH.div [ cls "guess-result" ]
        [ HH.div [ cls "color-info" ]
            [ HH.p_ [ HH.text "Correct: ", HH.text $ toHexString correct ]
            , colorBox correct
            ]
        , HH.div [ cls "color-info" ]
            [ HH.p_ [ HH.text "Your answer: ", HH.text $ toHexString answer ]
            , colorBox answer
            ]
        ]
    ]
  where
  d = distance correct answer
  -- point = clamp 0 5000 $ floor $ 7000 - 1443 * Math.log d
  point = clamp 0 5000 $ floor $ 10000.0 / d

colorBox :: forall w i. Color -> HH.HTML w i
colorBox color = HH.div
  [ cls "color-box"
  , HP.style $ "background-color:" <> toHexString color
  ]
  empty
