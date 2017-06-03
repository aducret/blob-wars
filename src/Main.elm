import Html exposing (..)
import Html.Events exposing (..)
import Debug exposing (..)
import Random

main = Html.beginnerProgram
   {
      model   = initialModel,
      view    = view,
      update  = update
   }

-- MODEL

type alias Transform a b = a -> b

type alias IndexedTransform a b = Row -> Col -> a -> b

type alias Matrix a = List(List(a))

type alias Row = Int

type alias Col = Int

type alias Size = Int

type alias Distance = Int

type alias Point = (Row, Col)

type alias Board = List(List(Tile))

type alias Amount = Int

type alias Model =
   {
      board           : Board,
      selected        : Maybe Point,
      match           : Match,
      turn            : Turn,
      amountOfRed     : Amount,
      amountOfBlue    : Amount
   }

type Tile = Empty
          | PossibleMovement
          | Selected Blob
          | Unselected Blob

type Blob = Blue
          | Red

type Match = BlueWin
           | RedWin
           | Tie
           | InProgress

type Turn = BlueTurn
          | RedTurn

size : Size
size = 8

initialBoard : Board
initialBoard =
   let
      rows = (List.range 0 (size-1))
      cols = (List.range 0 (size-1))
      getBlob row col =
         if (row, col) == (0, 0) || (row, col) == (0, size-1) then
            Unselected Blue
         else if (row, col) == (size-1, 0) || (row, col) == (size-1, size-1) then
            Unselected Red
         else
            Empty
   in
      List.map (\row -> List.map (getBlob row) cols) rows

initialModel : Model
initialModel = Model initialBoard Maybe.Nothing InProgress BlueTurn 2 2

-- UPDATE

type Msg = Click Point
         | Restart

update : Msg -> Model -> Model
update msg model =
   case Debug.log "msg" msg of
      Click point ->
         case getTile point model.board of
            Just tile ->
               case Debug.log "selected tile" tile of
                  Empty              -> model |> clear
                  PossibleMovement   -> model
                                          |> handleBlobMovement point
                                          |> clear
                                          |> updateAmountOfblobs
                                          |> checkGameStatus
                  Unselected blob    -> model
                                          |> clear
                                          |> handleBlobSelection point
                  _                  -> model
            Nothing -> Debug.log "This should never happend!!!" model
      Restart -> initialModel

getTile : Point -> Board -> Maybe Tile
getTile point board =
   let
      row = Tuple.first point
      col = Tuple.second point
   in
      case List.head (List.drop row board) of
         Just cols   -> List.head (List.drop col cols)
         Nothing     -> Nothing

clear : Model -> Model
clear model =
   { model |
     board = model.board
                   |> matrixMap possibleMovementToEmpty
                   |> matrixMap selectedToUnselected,
     selected = Nothing
   }

matrixMap : Transform a b -> Matrix a -> Matrix b
matrixMap transform board =
   List.map (\tiles -> List.map transform tiles) board

matrixIndexedMap : IndexedTransform a b -> Matrix a -> Matrix b
matrixIndexedMap transform board =
    List.indexedMap (\row tiles -> List.indexedMap (transform row) tiles) board

possibleMovementToEmpty : Tile -> Tile
possibleMovementToEmpty tile =
   case tile of
      PossibleMovement -> Empty
      _ -> tile

selectedToUnselected : Tile -> Tile
selectedToUnselected tile =
   case tile of
      Selected Blue -> Unselected Blue
      Selected Red -> Unselected Red
      _ -> tile

handleBlobSelection : Point -> Model -> Model
handleBlobSelection point model =
  let
    tile = getTile point model.board
  in
    case (tile, model.turn) of
      (Just (Unselected Red), RedTurn)   -> updateModelOnSelection point model
      (Just (Unselected Blue), BlueTurn) -> updateModelOnSelection point model
      (_, _)             -> model

updateAmountOfblobs : Model -> Model
updateAmountOfblobs model =
    {  model |
       amountOfRed = countBlobs Red model.board,
       amountOfBlue = countBlobs Blue model.board
    }

checkGameStatus : Model -> Model
checkGameStatus model =
  if isBoardCompleted model.board then
    { model |
      match = updateWinner model
    }
  else
    model

updateWinner : Model -> Match
updateWinner model =
  if model.amountOfRed < model.amountOfBlue then
    BlueWin
  else if model.amountOfRed > model.amountOfBlue then
    RedWin
  else
    Tie

isBoardCompleted : Board -> Bool
isBoardCompleted board =
  let
    innerReduce tile completed =
      case tile of
        Empty -> False
        _ -> completed
    outerReduce rowCompleted matrixCompleted = rowCompleted && matrixCompleted
  in
    matrixReduce innerReduce outerReduce True board

countBlobs : Blob -> Board -> Amount
countBlobs blob board =
  let
    check blobType = if blobType == blob then 1 else 0
    innerCount tile acum =
      case tile of
        Unselected blobType -> acum + check blobType
        Selected blobType -> acum + check blobType
        _ -> acum
    outerCount amount acum = amount + acum
  in
    matrixReduce innerCount outerCount 0 board

matrixReduce : (a -> b -> b) -> (b -> b -> b) -> b -> Matrix a -> b
matrixReduce innerReduce outerReduce initialValue matrix =
  List.foldr (\row acum -> outerReduce (List.foldr innerReduce initialValue row) acum) initialValue matrix

handleBlobMovement : Point -> Model -> Model
handleBlobMovement point model =
  let
    selectedPoint =
      case model.selected of
        Just p -> p
        Nothing -> Debug.log "This should never happend!!!" (0, 0)
    tile = unwrapTile (getTile selectedPoint model.board)
  in
    case model.selected of
      Nothing -> model |> clear
      Just lastSelectedpoint ->
        {  model |
           board = model.board
                     |> updateLastSelection lastSelectedpoint point tile
                     |> swap lastSelectedpoint point
                     |> stainArea point model.turn,
           selected = Nothing,
           turn = changeTurn model.turn
        }

changeTurn : Turn -> Turn
changeTurn turn =
  case turn of
    RedTurn -> BlueTurn
    BlueTurn -> RedTurn

stainArea : Point -> Turn -> Board -> Board
stainArea point turn board =
  let
    stain row col tile =
      if calculateDistance point (row, col) == 1 then
        stainTile turn tile
      else
        tile
  in
    matrixIndexedMap stain board

stainTile : Turn -> Tile -> Tile
stainTile turn tile =
  case turn of
    RedTurn   ->
      case tile of
        Unselected Blue -> Unselected Red
        Selected Blue -> Selected Red
        _ -> tile
    BlueTurn  ->
      case tile of
        Unselected Red -> Unselected Blue
        Selected Red -> Selected Blue
        _ -> tile

swap : Point -> Point -> Board -> Board
swap p1 p2 board =
  let
    tile1 = unwrapTile (getTile p1 board)
    tile2 = unwrapTile (getTile p2 board)
    swapTransform row col tile =
      if (row, col) == p1 then
        tile2
      else if (row, col) == p2 then
        tile1
      else
        tile
  in
    matrixIndexedMap swapTransform board

updateLastSelection : Point -> Point -> Tile -> Board -> Board
updateLastSelection p1 p2 originalTile board =
  let
    distance = calculateDistance p1 p2
    update row col tile =
      if (row, col) == p2 then
        if distance == 2 then
          Empty
        else
          originalTile
      else
        tile
  in
    matrixIndexedMap update board

unwrapTile : Maybe Tile -> Tile
unwrapTile maybeTile =
  case maybeTile of
    Just (Unselected Blue) -> Unselected Blue
    Just (Unselected Red) -> Unselected Red
    Just (Selected Blue) -> Selected Blue
    Just (Selected Red) -> Selected Red
    Just Empty -> Empty
    Just PossibleMovement -> PossibleMovement
    Nothing -> Empty

updateModelOnSelection : Point -> Model -> Model
updateModelOnSelection point model =
  let
      selectBoard board =
         board
            |> updateBlob point selectTile
            |> updateMovements point showMovement
      unselectboard lastSelectedpoint board =
         board
            |> updateBlob lastSelectedpoint unselectTile
            |> updateMovements lastSelectedpoint unshowMovement
  in
    case model.selected of
      Nothing ->
        {  model |
           board = selectBoard model.board,
           selected = Just point
        }
      Just lastSelectedpoint ->
        {  model |
           board = model.board
                     |> unselectboard lastSelectedpoint
                     |> selectBoard,
           selected = Just point
        }

updateMovements : Point -> (Tile -> Tile) -> Board -> Board
updateMovements point transform board =
   let
      update row col tile =
         if isOnMoveRange point (row, col) then
            transform tile
         else
            tile
   in
      matrixIndexedMap update board

isOnMoveRange : Point -> Point -> Bool
isOnMoveRange p1 p2 =
   let
      distance = calculateDistance p1 p2
   in
      distance == 1 || distance == 2

-- Actualiza la blob dada en el tablero, aplicando la funciona dada.
updateBlob : Point -> (Tile -> Tile) -> Board -> Board
updateBlob point transform board =
   let
      update row col tile =
        if point == (row, col) then
            transform tile
        else
           tile
   in
      matrixIndexedMap update board

selectTile : Tile -> Tile
selectTile tile =
   case tile of
      Unselected blob -> Selected blob
      _ -> tile

unselectTile : Tile -> Tile
unselectTile tile =
   case tile of
      Selected blob -> Unselected blob
      _ -> tile

showMovement : Tile -> Tile
showMovement tile =
   case tile of
      Empty -> PossibleMovement
      _ -> tile

unshowMovement : Tile -> Tile
unshowMovement tile =
   case tile of
      PossibleMovement -> Empty
      _ -> tile

calculateDistance : Point -> Point -> Distance
calculateDistance p1 p2 =
  max (abs (Tuple.first p1 - Tuple.first p2)) (abs (Tuple.second p1 - Tuple.second p2))

-- VIEW

view : Model -> Html Msg
view model =
  let
    restartButton = button [onClick Restart] [text "Restart"]
    default = [amountToHtml model.amountOfBlue model.amountOfRed, table [] (boardToHtml model.board), restartButton]
  in
   case model.match of
     BlueWin ->
        defaultViewWithContent ([h2 [] [text "Blue wins!"]] ++ default)
     RedWin ->
        defaultViewWithContent ([h2 [] [text "Red wins!"]] ++ default)
     Tie ->
        defaultViewWithContent ([h2 [] [text "Tie!"]] ++ default)
     InProgress ->
        defaultViewWithContent ([turnToHtml model.turn] ++ default)

defaultViewWithContent : List(Html Msg) -> Html Msg
defaultViewWithContent content =
   div [] ([ h1 [] [ text "Blob Wars!" ]] ++ content)

turnToHtml : Turn -> Html Msg
turnToHtml turn =
   case turn of
      RedTurn -> h3 [] [text "Red turn..."]
      BlueTurn -> h3 [] [text "Blue turn..."]

amountToHtml : Amount -> Amount -> Html Msg
amountToHtml amountOfBlue amountOfRed =
  h3 [] [text ("Blue blobs: " ++ (toString amountOfBlue)), text ("Red blobs: " ++ (toString amountOfRed))]

boardToHtml : List(List(Tile)) -> List(Html Msg)
boardToHtml board =
  let
    update row col tile =
      case tile of
         Empty            -> td [onClick (Click (row, col))] [text "X"]
         PossibleMovement -> td [onClick (Click (row, col))] [text "o"]
         Unselected blob ->
            case blob of
               Red        -> td [onClick (Click (row, col))] [text "r"]
               Blue       -> td [onClick (Click (row, col))] [text "b"]
         Selected blob ->
            case blob of
               Red        -> td [onClick (Click (row, col))] [text "R"]
               Blue       -> td [onClick (Click (row, col))] [text "B"]
  in
    List.map (\tiles -> tr [] tiles) (matrixIndexedMap update board)
