module Main exposing (main)

{-| An illustration of interference in the famous double-slits physics experiment.

The number of slits and the size of the slits can be changed, with the resulting diffraction
pattern being updated in real-time.

@docs main
-}

import Html exposing (..)
import Json.Decode as Decode
import Maybe
import Mouse exposing (Position)
import Svg exposing (..)
import Svg.Attributes exposing (style, fill, width, height, x, y)
import Svg.Events exposing (on)


{-| The slits model -}
type Slit = Slit Int Int

type alias Slits = List Slit

type DragType
    = WholeSlit
    | Bottom
    | Top

--| The slit we are dragging and the type of drag
type alias Drag = (DragType, Slit)

--| Model is the array of static slits plus the slit being dragged
type alias Model =
    { slits : Slits
    , drag : Maybe (DragType, Slit)
    }

--| Remove any dragging state from the model
resetModel : Model -> Model
resetModel m =
    case m.drag of
        Just (_, s) -> Model (s :: m.slits) Nothing
        Nothing -> m

slitAtY : Slits -> Int -> Maybe Slit
slitAtY slits y =
    case slits of
        [] -> Nothing
        (s :: ss) -> if containsY y s then (Just s) else slitAtY ss y

containsY : Int -> Slit -> Bool
containsY y (Slit y1 y2) = (y > y1 - delta) && (y < y2 + delta)

--| Tolerance within which a coordinate will still be assumed to be "within" a slit
delta : Int
delta = 8

--| For dragging, we only care about the y coordinate
type Msg
    = DragStart Int
    | DragAt    Int
    | DragEnd

init : (Model, Cmd Msg)
init =
    let
        slits = [Slit 10 50, Slit 260 300]
    in
        ( { slits = slits, drag = Nothing } , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing -> Sub.none
        Just _  -> Sub.batch [ Mouse.moves (\p -> DragAt p.y), Mouse.ups (\_ -> DragEnd) ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (doUpdate msg model, Cmd.none)

doUpdate : Msg -> Model -> Model
doUpdate msg ({slits, drag} as m) =
    case msg of
        DragStart y ->
            case drag of
                --| Reset if we get a spurious DragStart message (sometimes we miss the drag end)
                Just _  -> resetModel m
                Nothing ->
                    let
                        selectedSlit = slitAtY slits y
                        -- Remove the selected slit from the list while we drag it about
                        staticSlits = case selectedSlit of
                            Nothing -> slits
                            Just s  -> List.filter ((/=) s) slits
                        dragType s = WholeSlit
                    in
                        Model staticSlits (Maybe.map (\s -> (dragType s, s)) selectedSlit)
        DragAt y ->
            case drag of
                Nothing -> Model slits Nothing
                Just ((dt, originalSlit) as d) ->
                    doDrag y d
                        |> checkSlitPosition slits originalSlit
                        |> Just
                        |> Model slits
        DragEnd  ->
            resetModel m

doDrag : Int -> Drag -> Drag
doDrag y (drag, Slit y1 y2 as s) = (,) drag <|
    case drag of
        WholeSlit -> moveSlit (Debug.log "moving slit" s) y
        Top       -> changeSlitWidth (y - y2) s
        Bottom    -> changeSlitWidth (y1 - y) s

checkSlitPosition : Slits -> Slit -> Drag -> Drag
checkSlitPosition slits originalSlit (dt, Slit y1 y2 as s) = (,) dt <|
    if y1 < 0 || y2 > 600 || List.any (intersects s) slits
        then originalSlit
        else s

intersects : Slit -> Slit -> Bool
intersects (Slit y1 y2) (Slit yy1 yy2) =
    y2 >= yy1 && y1 <= yy2         --| overlaps bottom
        || y1 <= yy2 && y2 >= yy1  --| overlaps top
        || y1 >= yy1 && y2 <= yy2  --| is contained in
        || y1 <= yy1 && y2 >= yy2  --| contains

moveSlit : Slit -> Int -> Slit
moveSlit (Slit y1 y2) y =
    let
        currentPosition = (y1 + y2) // 2
        delta = y - currentPosition
    in
        Debug.log "slit" <| Slit (y1 + delta) (y2 + delta)

--| Change the slit width by a positive or negative amount, maintaining the same midpoint.
-- The width is changed by twice the given amount since it is added to the top coordinate and subtracted
-- from the bottom.
changeSlitWidth : Int -> Slit -> Slit
changeSlitWidth delta (Slit y1 y2) = (Slit (y1 - delta) (y2 + delta))

view : Model -> Html Msg
view m =
    let
        selectedSlit = Maybe.map Tuple.second m.drag
        slits = case selectedSlit of
            Nothing -> m.slits
            Just s  -> s :: m.slits
    in
        svg
            [ width "900"
            , height "600"
            ]
            [ drawSlits slits
            ]

drawSlits : Slits -> Svg Msg
drawSlits slits =
    let
        onMouseDown = on "mousedown" (Decode.map (\p -> DragStart p.y) Mouse.position)
        background = rect [width "80", height "600", fill "gray" ] []
        drawSlit (Slit y1 y2) = rect [onMouseDown, Svg.Attributes.style "cursor: move", y (toString y1), width "80", height (toString (y2 - y1)), fill "black" ] []
    in
        g []
            (background :: (List.map drawSlit slits))

{-| Run the program.
-}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
