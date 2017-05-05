import Html exposing (..)
import Html.Events exposing (..)
import Random
import Debug exposing (..)

main =
  Html.beginnerProgram
    { model = init
    , view = view
    , update = update
    }

-- MODEL

type alias Point = (Int, Int)

type alias Model =
  { board : List(List(Tile))
  , lastAction : Point
  , winner : Winner
  , turn : Turn
  }

type Tile = Empty (Int, Int) | RedBlob (Int, Int) Bool | BlueBlob (Int, Int) Bool | PossibleMovement (Int, Int)

type Winner = BlueWin | RedWin | MatchInProgress -- TODO Empate

type Turn = BlueTurn | RedTurn

size : Int
size = 8

init : Model
init = Model initializeBoard (0, 0) MatchInProgress BlueTurn

initializeBoard : List(List(Tile))
initializeBoard = List.map (\row -> (List.map (\col ->
    if (row, col) == (0, 0) || (row, col) == (0, size-1) then
      BlueBlob (row, col) False
    else if (row, col) == (size-1, 0) || (row, col) == (size-1, size-1) then
      RedBlob (row, col) False
    else
      Empty (row, col)) (List.range 0 (size-1)))) (List.range 0 (size-1))

-- UPDATE

type Msg = Click Point

update : Msg -> Model -> Model
update msg model =
  case msg of
    Click (x, y) ->
      case getTile x y model.board of
        Just tile ->
          case tile of
            BlueBlob (x, y) False -> handleBlobSelection x y model
            RedBlob (x, y) False -> handleBlobSelection x y model
            Empty (x, y) -> { model | board = model.board |> clearPossibleMovements }
            PossibleMovement (x, y) -> handleMovement x y model
            BlueBlob (x, y) True -> { model | board = model.board |> clearPossibleMovements }
            RedBlob (x, y) True -> { model | board = model.board |> clearPossibleMovements }

        Nothing -> model -- This should never happend

handleMovement : Int -> Int -> Model -> Model
handleMovement x y model =
  let newModel = moveBlob x y model
  in
    if checkFinalState newModel.board then
      { newModel | winner = calculateWinner model.board }
    else
      newModel

moveBlob : Int -> Int -> Model -> Model
moveBlob x y model =
  let
    distance = calculateDistance (Tuple.first model.lastAction) (Tuple.second model.lastAction) x y
  in
    if distance == 1 then
      { model | board = model.board
                           |> addBlobAt x y model.turn
                           |> clearPossibleMovements,
                turn = changeTurn model.turn }
    else if distance == 2 then
      { model | board = model.board
                            |> removeBlobAt (Tuple.first model.lastAction) (Tuple.second model.lastAction) model.turn
                            |> addBlobAt x y model.turn
                            |> clearPossibleMovements,
                turn = changeTurn model.turn }
    else
      { model | board = clearPossibleMovements model.board }

-- TODO: Manchar
addBlobAt : Int -> Int -> Turn -> List(List(Tile)) -> List(List(Tile))
addBlobAt x y turn board =
  List.map (\ tiles ->
       List.map (\ tile ->
            case tile of
              Empty (x2, y2) ->
                if x == x2 && y == y2 then
                  createCurrentBlob turn x y
                else
                  tile
              _ -> tile
         ) tiles
    ) board

removeBlobAt : Int -> Int -> Turn -> List(List(Tile)) -> List(List(Tile))
removeBlobAt x y turn board =
  List.map (\ tiles ->
       List.map (\ tile ->
            case tile of
              RedBlob (x2, y2) _ ->
                if x == x2 && y == y2 && turn == BlueTurn then
                  Empty (x, y)
                else
                  tile
              BlueBlob (x2, y2) _ ->
                if x == x2 && y == y2 && turn == BlueTurn then
                  Empty (x, y)
                else
                  tile
              _ -> tile
         ) tiles
    ) board

changeTurn : Turn -> Turn
changeTurn turn =
  case turn of
    BlueTurn -> RedTurn
    RedTurn -> BlueTurn

createCurrentBlob : Turn -> Int -> Int -> Tile
createCurrentBlob turn x y =
  case turn of
    BlueTurn -> BlueBlob (x, y) False
    RedTurn -> RedBlob (x, y) False

calculateDistance : Int -> Int -> Int -> Int -> Int
calculateDistance x1 y1 x2 y2 =
  max (abs (x1 - x2)) (abs (y1 - y2))

handleBlobSelection : Int -> Int -> Model -> Model
handleBlobSelection x y model =
  if validSelection x y model.turn model.board then
    { model | board = model.board |> clearPossibleMovements
                                  |> showMovements x y,
              lastAction = (x, y) }
  else
    { model | board = clearPossibleMovements model.board }

checkFinalState : List(List(Tile)) -> Bool
checkFinalState board =
  (List.foldl (\tiles acum ->
      acum + (List.foldl (\ tile acum ->
          case tile of
            Empty _ -> acum + 1
            _ -> acum
        ) 0 tiles)
    ) 0 board) == 0

calculateWinner : List(List(Tile)) -> Winner
calculateWinner board =
    let
      red = List.foldl (\tiles acum ->
          acum + (List.foldl (\ tile acum ->
              case tile of
                RedBlob _ _-> acum + 1
                _ -> acum
            ) 0 tiles)
        ) 0 board

      blue = List.foldl (\tiles acum ->
          acum + (List.foldl (\ tile acum ->
              case tile of
                BlueBlob _ _-> acum + 1
                _ -> acum
            ) 0 tiles)
        ) 0 board
    in
      if red > blue then
        RedWin
      else
        BlueWin

getTile : Int -> Int -> List(List(Tile)) -> Maybe Tile
getTile x y board =
  List.head (List.foldl (\tiles result ->
      result ++ (List.foldl (\tile acum ->
          case tile of
            RedBlob (x2, y2) _ -> acum ++ (isTileAt x y x2 y2 tile)
            BlueBlob (x2, y2) _ -> acum ++ (isTileAt x y x2 y2 tile)
            PossibleMovement (x2, y2) -> acum ++ (isTileAt x y x2 y2 tile)
            Empty (x2, y2) -> acum ++ (isTileAt x y x2 y2 tile)
        ) [] tiles)
    ) [] board)

isTileAt : Int -> Int -> Int -> Int -> Tile -> List(Tile)
isTileAt x1 y1 x2 y2 tile =
  if x1 == x2 && y1 == y2 then
    [tile]
  else
    []

showMovements : Int -> Int -> List(List(Tile)) -> List(List(Tile))
showMovements x y board =
    List.map (\ tiles ->
        List.map (\ tile ->
          let distance = calculateDistance (Tuple.first (getTilePosition tile)) (Tuple.second (getTilePosition tile)) x y
          in
            if distance == 1 || distance == 2 then
              PossibleMovement (x, y)
            else if distance == 0 then
              selectBlob tile
            else
              tile
          ) tiles
      ) board

selectBlob : Tile -> Tile
selectBlob tile =
  case tile of
    RedBlob (x, y) _ -> RedBlob (x, y) True
    BlueBlob (x, y) _ -> BlueBlob (x, y) True
    _ -> tile

unselectBlob : Tile -> Tile
unselectBlob tile =
  case tile of
    RedBlob (x, y) _ -> RedBlob (x, y) False
    BlueBlob (x, y) _ -> BlueBlob (x, y) False
    _ -> tile

getTilePosition : Tile -> (Int, Int)
getTilePosition tile =
  case tile of
    RedBlob (x, y) _-> (x, y)
    BlueBlob (x, y) _ -> (x, y)
    Empty (x, y) -> (x, y)
    PossibleMovement (x, y) -> (x, y)

validSelection : Int -> Int -> Turn -> List(List(Tile)) -> Bool
validSelection x y turn board =
    case turn of
      BlueTurn ->
        case getTile x y board of
          Just tile ->
            case tile of
              BlueBlob _ _ -> True
              _ -> False
          _ -> False
      RedTurn ->
        case getTile x y board of
          Just tile ->
            case tile of
              RedBlob _ _ -> True
              _ -> False
          _ -> False

clearPossibleMovements : List(List(Tile)) -> List(List(Tile))
clearPossibleMovements board =
  List.map (\ tiles -> List.map (\ tile ->
    case tile of
      PossibleMovement (x, y) -> Empty (x, y)
      _ -> unselectBlob tile
    ) tiles) board

-- VIEW

view : Model -> Html Msg
view model =
  case model.winner of
    BlueWin ->
      defaultViewWithContent [h2 [] [text "Blue wins!"]]
    RedWin ->
      defaultViewWithContent [h2 [] [text "Red wins!"]]
    MatchInProgress ->
      defaultViewWithContent [turnToHtml model.turn, table [] (boardToHtml model.board)]

defaultViewWithContent : List(Html Msg) -> Html Msg
defaultViewWithContent content =
  div [] ([ h1 [] [ text "Blob Wars!" ]] ++ content)

turnToHtml : Turn -> Html Msg
turnToHtml turn =
    case turn of
      RedTurn -> h3 [] [text "Red turn..."]
      BlueTurn -> h3 [] [text "Blue turn..."]

boardToHtml : List(List(Tile)) -> List(Html Msg)
boardToHtml board =
  List.map (\tiles ->
        tr [] (List.map (\ elem ->
            case elem of
              Empty (x, y) -> td [onClick (Click (x, y))] [text "X"]
              RedBlob (x, y) False -> td [onClick (Click (x, y))] [text "r"]
              BlueBlob (x, y) False -> td [onClick (Click (x, y))] [text "b"]
              RedBlob (x, y) True -> td [onClick (Click (x, y))] [text "R"]
              BlueBlob (x, y) True -> td [onClick (Click (x, y))] [text "B"]
              PossibleMovement (x, y) -> td [onClick (Click (x, y))] [text "o"]
          ) tiles)) board
