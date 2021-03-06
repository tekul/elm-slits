port module Main exposing (main)

{-| An illustration of interference in the famous double-slits physics experiment.

The number of slits and the size of the slits can be changed, with the resulting diffraction
pattern being updated in real-time.

@docs main
-}

import Browser
import Browser.Events exposing (onClick, onMouseMove, onMouseUp)
import Json.Decode as Json
import Maybe
import Model exposing (Model, Msg(..), Slit(..), startDrag, doDrag, stopDrag)
import View exposing (view)

port numberOfSlits : (Int -> msg) -> Sub msg

type alias Flags =
    { width : Int
    , height : Int
    }

init : Flags -> (Model, Cmd Msg)
init { width, height } =
    let
        slits = layoutSlits 2 height
        screen = { x = width, y1 = 0, y2 = height }
    in
        ( { slits = slits
          , slitsXY = (200, height // 2)
          , drag = Nothing
          , screen = screen
          , lambda = 50
          , width = String.fromInt width
          , height = height
          }
        , Cmd.none )


layoutSlits : Int -> Int -> List Slit
layoutSlits n_ h =
    let
        n = if n_ < 1
               then 1
            else if n_ > 10
               then 10
            else n_
        slitSpacing = h // (n + 1)
        halfSlitWidth = h // (5 * n)
        mkSlit i = Slit (i * slitSpacing - halfSlitWidth) (i * slitSpacing + halfSlitWidth)
        -- Special case for double slits
        slitSpacing2 = h // 6
        slitWidth2 = h // 10
    in
        if n == 2
            then [ Slit slitSpacing2 (slitSpacing2 + slitWidth2)
                 , Slit (h - slitSpacing2 - slitWidth2) (h - slitSpacing2)
                 ]
            else List.map mkSlit (List.range 1 n)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
    numberOfSlits NumSlits ::
    case model.drag of
        Nothing -> []
        Just _  ->
            [ onMouseMove (Json.map DragAt (Json.field "pageY" Json.int))
            , onMouseUp (Json.succeed DragEnd)
            ]


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
        NumSlits n ->
            { m | slits = layoutSlits n m.height, drag = Nothing }


{-| Run the program.
  Change "()" to "Flags" for embedding and remove hard-coded Flags
-}
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init (Flags 800 600)
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
