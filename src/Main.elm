module Main exposing (main)

{-| An illustration of interference in the famous double-slits physics experiment.

The number of slits and the size of the slits can be changed, with the resulting diffraction
pattern being updated in real-time.

@docs main
-}

import Html exposing (program)
import Maybe
import Model exposing (Model, Msg(..), Slit(..), startDrag, doDrag, stopDrag)
import Mouse exposing (moves, ups)
import View exposing (view)


type alias Flags =
    { width : Int
    , height : Int
    }

init : Flags -> (Model, Cmd Msg)
init { width, height } =
    let
        slits = [Slit 10 20, Slit 400 410]
        screen = { x = width, y1 = 0, y2 = height, theta1 = 50, theta2 = -50 }
    in
        ( { slits = slits
          , drag = Nothing
          , screen = screen
          , lambda = 50
          , width = toString width
          , height = toString height
          }
        , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing -> Sub.none
        Just _  -> Sub.batch [ Mouse.moves (\p -> DragAt p.y), Mouse.ups (\_ -> DragEnd) ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (doUpdate msg model, Cmd.none)

doUpdate : Msg -> Model -> Model
doUpdate msg m =
    case msg of
        DragStart y0 y1 ->
            case m.drag of
                --| Reset if we get a spurious DragStart message (sometimes we miss the drag end)
                Just _  -> stopDrag m
                Nothing -> startDrag (y0, y1) m
        DragAt y ->
            doDrag y m
        DragEnd  ->
            stopDrag m


{-| Run the program.
-}
main : Program Flags Model Msg
main =
    {-
    Html.program
        { init = init (Flags 800 600)
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
-}

    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
