import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- TYPES
type alias Model =
  { player: PLAYER
  , board: Board
  , winner: WINNER
  }
type alias Board = List Row
type alias Row = List Box
type alias Box = (Coordinates, BoxValue)
type alias Coordinates = (Int, Int)

type BoxValue = Empty | Xtype | Otype
type PLAYER = PLAYER1 | PLAYER2
type WINNER = NOWINNER | WINNER1 | WINNER2

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
  , winner = NOWINNER
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
    UPDATE_BOARD box ->
      updateBox box model
      |> updateWinner

updateWinner: Model -> Model
updateWinner model =
  let
    assignWinner player =
      case player of
        PLAYER1 -> WINNER2
        PLAYER2 -> WINNER2
  in
    if thereIsAWinner model then { model | winner = assignWinner model.player} else model

thereIsAWinner : Model -> Bool
thereIsAWinner model =
  False

updateBox: Box -> Model -> Model
updateBox box model =
  let
    mapRow row =
      List.map (\x -> if (isSelectedBoxAndIsAvaiable box x) then ((fst x), assignBoxValue model.player) else x) row
  in
    { model | player = pickNextPlayer model.player box, board = List.map mapRow model.board }

isSelectedBoxAndIsAvaiable : Box -> Box -> Bool
isSelectedBoxAndIsAvaiable (pickedCoordinates, pickedValue) (currentCoordinates, currentValue) =
  if (pickedCoordinates == currentCoordinates) then
    case currentValue of
      Empty -> True
      Xtype -> False
      Otype -> False
  else
    False

pickNextPlayer : PLAYER -> Box -> PLAYER
pickNextPlayer player (pickedCoodinates, pickedValue) =
  let
    nextPlayer p =
      case p of
        PLAYER1 -> PLAYER2
        PLAYER2 -> PLAYER1

  in
    case pickedValue of
      Empty -> nextPlayer player
      Xtype -> player
      Otype -> player

assignBoxValue : PLAYER -> BoxValue
assignBoxValue player =
  case player of
    PLAYER1 -> Xtype
    PLAYER2 -> Otype

-- VIEW
view : Model -> Html MSG
view model =
     div [style mainGridStyle] (renderBoard model.board)

winningMessage : WINNER -> String
winningMessage winner =
    let
      message = "Good Job"
    in
      case winner of
        NOWINNER -> ""
        WINNER1 -> message ++ "Player 1!"
        WINNER2 -> message ++ "Player 2!"

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
        Empty -> ""
        Xtype -> "X"
        Otype -> "O"
   in
    a [style (boxStyling ++ selectBoxColor box), onClick (UPDATE_BOARD box)] [
      h1 [style valueStyling] [
        text (ticTacChar box)
      ]
    ]



selectBoxColor : Box -> List (String, String)
selectBoxColor box =
  let
    pickColor boxValue =
      case boxValue of
        Empty -> "#f67575"
        Xtype -> "#c45d5d"
        Otype -> "#e10071"
  in
    [("background-color", pickColor (snd box))]

--STYLE GENERATORS
mainGridStyle : List (String, String)
mainGridStyle =
  [ ("display", "flex")
  , ("flex-direction", "column")
  , ("height", "inherit")
  , ("background-color", "#ffc697")
  ]


rowStyling : List (String, String)
rowStyling =
  [ ("display", "flex")
  , ("flex", "33")
  , ("justify-content", "space-around")
  ]

boxStyling : List (String, String)
boxStyling =
  [ ("display", "flex")
  , ("cursor", "pointer")
  , ("flex", "1")
  , ("align-items", "center")
  , ("justify-content", "center")
  , ("border-radius", "160px")
  , ("margin", "2% 4%")
  ]

valueStyling : List (String, String)
valueStyling =
  [ ("font-size", "100px")
  , ("margin", "0"  )
  , ("text-align", "center")
  , ("color", "#FFF")
  ]
