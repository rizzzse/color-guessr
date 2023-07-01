module App
  ( appRoot
  ) where

import Prelude

import ColorGuessr.Page as Page
import ColorGuessr.Page.Guess (guessPage)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type State = { page :: Page.Page }

appRoot :: forall query input message. H.Component query input message Aff
appRoot = H.mkComponent
  { initialState: const { page: Page.Guess }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

type ChildSlots = (guessPage :: forall query. H.Slot query Page.Page Unit)

render :: State -> HH.ComponentHTML Page.Page ChildSlots Aff
render { page } = case page of
  Page.Guess ->
    HH.slot (Proxy :: _ "guessPage") unit guessPage unit identity
  Page.GuessResult correct answer ->
    HH.div_ []

handleAction
  :: forall slots message m
   . MonadEffect m
  => Page.Page
  -> H.HalogenM State Page.Page slots message m Unit
handleAction page =
  H.modify_ _ { page = page }
