import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- TYPES
type alias Model =
  { player: PLAYER
  , board: Board
  }
type alias Board = List Row
type alias Row = List Box
type alias Box = (Coordinates, BoxValue)
type alias Coordinates = (Int, Int)
type BoxValue = Empty | Xtype | Otype
type PLAYER = PLAYER1 | PLAYER2

-- UPDATE TYPES
type MSG = UPDATE_BOARD Box

--PROGRAM

main : Program Never
main =
  Html.App.beginnerProgram
    { model = model
    , update = update
    , view = view
    }

-- MODEL
model : Model
model =
  { player = PLAYER1
  , board =
      [ [((0,0),Empty),((0,1),Empty),((0,2),Empty)]
      , [((1,0),Empty),((1,1),Empty),((1,2),Empty)]
      , [((2,0),Empty),((2,1),Empty),((2,2),Empty)]
      ]
  }

-- UPDATE
update : MSG -> Model -> Model
update message model =
  case message of
    UPDATE_BOARD box -> updateBox box model

updateBox: Box -> Model -> Model
updateBox box model =
  let mapRow row =
    List.map (\x -> if (fst x == fst box) then ((fst x), assignBoxValue model.player) else x) row
  in
    { model | player = pickNextPlayer model.player, board = List.map mapRow model.board }

pickNextPlayer : PLAYER -> PLAYER
pickNextPlayer player =
  case player of
    PLAYER1 -> PLAYER2
    PLAYER2 -> PLAYER1

assignBoxValue : PLAYER -> BoxValue
assignBoxValue player =
  case player of
    PLAYER1 -> Xtype
    PLAYER2 -> Otype

-- VIEW
view : Model -> Html MSG
view model =
  div [style mainGridStyle] (renderBoard model.board)

-- RENDER FUNCTIONS
renderBoard : Board -> List (Html MSG)
renderBoard board =
  List.map renderRow board

renderRow : Row -> Html MSG
renderRow row =
  div [style rowStyling] (List.map renderBox row)

renderBox: Box -> Html MSG
renderBox box =
  let
    ticTacChar int =
      case (snd box) of
        Empty -> "-"
        Xtype -> "X"
        Otype -> "O"
   in
     h1 [onClick (UPDATE_BOARD box), style valueStyling] [text (ticTacChar box)]

--STYLE GENERATORS
mainGridStyle : List (String, String)
mainGridStyle =
  [ ("display", "flex")
  , ("flex-direction", "column")
  , ("height", "inherit")
  ]

rowStyling : List (String, String)
rowStyling =
  [ ("display", "flex")
  , ("flex", "33")
  , ("justify-content", "space-around")
  , ("align-items", "center")
  ]

valueStyling : List (String, String)
valueStyling =
  [("cursor", "pointer")]
