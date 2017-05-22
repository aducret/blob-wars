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

type alias Row = Int

type alias Col = Int

type alias Size = Int

type alias Distance = Int

type alias Point = (Row, Col)

type alias Board = List(List(Tile))

type alias Model =
   {
      board           : Board,
      selected        : Maybe Point,
      match           : Match,
      turn            : Turn
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
initialModel = Model initialBoard Maybe.Nothing InProgress BlueTurn

-- UPDATE

type Msg = Click Point

update : Msg -> Model -> Model
update msg model =
   case Debug.log "msg" msg of
      Click point ->
         case getTile point model.board of
            Just tile ->
               case tile of
                  Empty              -> model
                  PossibleMovement   -> model
                  Selected blob      -> model
                  Unselected blob    -> model |> handleBlobSelection point
            Nothing -> Debug.log "This should never happend!!!" model

getTile : Point -> Board -> Maybe Tile
getTile point board =
   let
      row = Tuple.first point
      col = Tuple.second point
   in
      case List.head (List.drop row board) of
         Just cols   -> List.head (List.drop col cols)
         Nothing     -> Nothing

handleBlobSelection : Point -> Model -> Model
handleBlobSelection point model =
  let
    tile = getTile point model.board
  in
    case (tile, model.turn) of
      (Just (Unselected Red), RedTurn)   -> updateModelOnSelection point model
      (Just (Unselected Blue), BlueTurn) -> updateModelOnSelection point model
      (_, _)             -> model

updateModelOnSelection : Point -> Model -> Model
updateModelOnSelection point model =
  let
    updateBoard board =
      board
        |> updateBlob point selectTile
  in
    case model.selected of
      Nothing    ->
        {  model |
           board = updateBoard model.board,
           selected = Just point
        }
      Just blobPoint ->
        {  model |
           board = (updateBoard model.board)
                          |> updateBlob blobPoint unselectTile,
           selected = Just point
        }

-- updateMovements : Maybe Blob -> (Tile -> Tile) -> Board -> Board
-- updateMovements optionalBlolb transform board =
--    case optionalBlob of
--       Just blob ->
--          let
--             point = extractPointFromBlob blob
--          in
--             List.map (\ tiles ->
--                List.map (\ tile ->
--                   let
--                      tilePoint = extractPointFromTile tile
--                      distance = calculateDistance point tilePoint
--                   in
--                      if isSamePosition point tilePoint then
--                         transform tile
--                      else
--                         tile
--                ) tiles
--             ) board
--       Nothing -> board

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
      List.indexedMap (\row tiles -> List.indexedMap (update row) tiles) board

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

calculateDistance : Point -> Point -> Distance
calculateDistance p1 p2 =
  max (abs (Tuple.first p1 - Tuple.first p2)) (abs (Tuple.second p1 - Tuple.second p2))

-- VIEW

view : Model -> Html Msg
view model =
 case model.match of
   BlueWin ->
      defaultViewWithContent [h2 [] [text "Blue wins!"]]
   RedWin ->
      defaultViewWithContent [h2 [] [text "Red wins!"]]
   Tie ->
      defaultViewWithContent [h2 [] [text "Tie!"]]
   InProgress ->
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
   List.indexedMap (\x tiles ->
      tr [] (List.indexedMap (\y tile ->
               case tile of
                  Empty            -> td [onClick (Click (x, y))] [text "X"]
                  PossibleMovement -> td [onClick (Click (x, y))] [text "o"]
                  Unselected blob ->
                     case blob of
                        Red        -> td [onClick (Click (x, y))] [text "r"]
                        Blue       -> td [onClick (Click (x, y))] [text "b"]
                  Selected blob ->
                     case blob of
                        Red        -> td [onClick (Click (x, y))] [text "R"]
                        Blue       -> td [onClick (Click (x, y))] [text "B"]
            ) tiles)
   ) board
