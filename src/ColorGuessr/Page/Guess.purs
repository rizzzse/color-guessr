module ColorGuessr.Page.Guess
  ( guessPage
  ) where

import Prelude

import Color (Color, black, hsv, rgb', toHexString)
import ColorGuessr.Page as Page
import ColorGuessr.Utils (cls)
import ColorPicker.Component as ColorPicker
import ColorPicker.HSV (hsvInput)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type State = { correct :: Color, answer :: Color }

data Action = Initialize | AnswerColor ColorPicker.Message | Guess

guessPage :: forall query input. H.Component query input Page.Page Aff
guessPage = H.mkComponent
  { initialState: const { correct: black, answer: black }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

type ChildSlots = (colorPicker :: forall query. H.Slot query ColorPicker.Message Unit)

render :: State -> HH.ComponentHTML Action ChildSlots Aff
render { correct } = HH.div [ cls "hoge" ]
  [ HH.p [ cls "text" ]
      [ HH.text "Guess the color of "
      , HH.span [ cls "color-hex" ] [ HH.text $ toHexString correct ]
      ]
  , HH.slot (Proxy :: _ "colorPicker") unit ColorPicker.component
      (hsvInput $ ColorPicker.Color 0.0 0.0 0.0)
      AnswerColor
  , HH.button [ cls "guess-button", HE.onClick $ const Guess ] [ HH.text "Guess" ]
  ]

handleAction
  :: forall slots m
   . MonadEffect m
  => Action
  -> H.HalogenM State Action slots Page.Page m Unit
handleAction = case _ of
  Initialize -> do
    color <- H.liftEffect randomColor
    H.modify_ _ { correct = color }
  AnswerColor (ColorPicker.Change (ColorPicker.Color a b c)) ->
    H.modify_ _ { answer = hsv a b c }
  Guess ->
    H.get >>= \{ correct, answer } -> H.raise $ Page.GuessResult correct answer

randomColor :: Effect Color
randomColor = rgb' <$> random <*> random <*> random
