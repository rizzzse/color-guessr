module ColorPicker.Component
  ( Action(..)
  , Color(..)
  , ColorSpaceName(..)
  , Input
  , Message(..)
  , OnDrag
  , Render
  , colorSpace
  , component
  ) where

import Prelude

import ColorGuessr.Utils (data_)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.Event (eventListener)
import Web.DOM.Element (getBoundingClientRect)
import Web.Event.Event (stopPropagation)
import Web.HTML (window)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, fromEvent, toEvent)
import Web.UIEvent.MouseEvent.EventTypes as MET

newtype ColorSpaceName = ColorSpaceName String

derive instance Newtype ColorSpaceName _

colorSpace :: forall r i. ColorSpaceName -> HH.IProp r i
colorSpace = data_ "color-space" <<< unwrap

data Color = Color Number Number Number

type Render slots = Color -> HH.ComponentHTML Action slots Aff

type OnDrag = H.RefLabel -> { x :: Number, y :: Number } -> Color -> Color

type Input slots =
  { color :: Color
  , render :: Render slots
  , onDrag :: OnDrag
  }

data Message = Change Color

type State slots =
  { grabbing :: Maybe H.RefLabel
  , color :: Color
  , render :: Render slots
  , onDrag :: OnDrag
  }

data Action
  = Initialize
  | StopPropagation MouseEvent
  | DragStart H.RefLabel MouseEvent
  | Drag MouseEvent
  | DragEnd MouseEvent

component :: forall query slots. H.Component query (Input slots) Message Aff
component = H.mkComponent
  { initialState
  , render: renderComponent
  , eval: H.mkEval eval
  }
  where
  eval = H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }

initialState :: forall slots. Input slots -> State slots
initialState { color, render, onDrag } =
  { grabbing: Nothing, color, render, onDrag }

handleAction
  :: forall slots m
   . MonadEffect m
  => Action
  -> H.HalogenM (State slots) Action slots Message m Unit
handleAction = case _ of
  Initialize -> do
    window <- H.liftEffect $ toEventTarget <$> window
    H.subscribe' \_ ->
      window # eventListener MET.mousemove $ map Drag <<< fromEvent
    H.subscribe' \_ ->
      window # eventListener MET.mouseup $ map DragEnd <<< fromEvent
  StopPropagation event ->
    H.liftEffect $ stopPropagation (event # toEvent)
  DragStart ref event ->
    H.modify_ _ { grabbing = Just ref }
      <> (handleAction $ Drag event)
  Drag event -> H.get <#> _.grabbing >>= traverse_ \grabbing ->
    H.getHTMLElementRef grabbing >>= traverse_ \el -> do
      clientRect <- H.liftEffect $ getBoundingClientRect $ toElement el
      { color, onDrag } <- H.get
      let
        x = ((clientX event # toNumber) - clientRect.left) / clientRect.width
        y = ((clientY event # toNumber) - clientRect.top) / clientRect.height
        newColor = onDrag grabbing { x, y } color
      H.modify_ _ { color = newColor }
      H.raise $ Change newColor
  DragEnd _ ->
    H.modify_ _ { grabbing = Nothing }

renderComponent :: forall slots. State slots -> HH.ComponentHTML Action slots Aff
renderComponent { color, render } = render color
