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
import Svg.Attributes exposing (style, fill, stroke, points, width, height, x, y)
import Svg.Events exposing (on)


{-| The slits model -}
type Slit = Slit Int Int

type alias Slits = List Slit

type alias Wavelength = Float

type alias Screen =
    { x : Int
    , y1 : Int
    , y2 : Int
    , theta1 : Int
    , theta2 : Int
    }

type DragType
    = WholeSlit
    | Bottom
    | Top

--| The slit we are dragging and the type of drag
type alias Drag = (DragType, Slit)

--| A list of y coordinate and intensity value pairs
type alias DiffractionPattern = List (Int, Int)

--| Model is the array of static slits plus the slit being dragged
type alias Model =
    { slits : Slits
    , screen : Screen
    , lambda : Wavelength
    , drag : Maybe (DragType, Slit)
    }

--| Remove any dragging state from the model
resetModel : Model -> Model
resetModel m =
    case m.drag of
        Just (_, s) -> { m | slits = (s :: m.slits), drag = Nothing }
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
        slits = [Slit 10 20, Slit 400 410]
        screen = { x = 550, y1 = 0, y2 = 600, theta1 = 50, theta2 = -50 }
    in
        ( { slits = slits
          , drag = Nothing
          , screen = screen
          , lambda = 50
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
doUpdate msg ({slits, screen, lambda, drag} as m) =
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
                        { m | slits = staticSlits, drag = Maybe.map (\s -> (dragType s, s)) selectedSlit }
        DragAt y ->
            case drag of
                Nothing -> m
                Just ((dt, originalSlit) as d) ->
                    doDrag y d
                        |> checkSlitPosition slits originalSlit
                        |> Just
                        |> Model slits screen lambda
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
        pattern = calculateDiffractionPattern slits m.screen m.lambda
    in
        svg
            [ width "900"
            , height "600"
            ]
            [ drawSlits slits
            , drawScreen m.screen
            , drawDiffractionPattern pattern
            ]


drawScreen : Screen -> Svg Msg
drawScreen {x , y1, y2} = line [stroke "black", Svg.Attributes.x1 (toString x), Svg.Attributes.y1 (toString y1), Svg.Attributes.x2 (toString x), Svg.Attributes.y2 (toString y2)] []


drawSlits : Slits -> Svg Msg
drawSlits slits =
    let
        onMouseDown = on "mousedown" (Decode.map (\p -> DragStart p.y) Mouse.position)
        background = rect [width "80", height "600", fill "gray" ] []
        drawSlit (Slit y1 y2) = rect [onMouseDown, Svg.Attributes.style "cursor: move", y (toString y1), width "80", height (toString (y2 - y1)), fill "black" ] []
    in
        g []
            (background :: (List.map drawSlit slits))


drawDiffractionPattern : DiffractionPattern -> Svg Msg
drawDiffractionPattern pattern =
    let
        pointsStr = pattern
            |> List.map (\(y, x) -> toString x ++ "," ++ toString y)
            |> String.join " "
    in
        g []
            [polygon [fill "gold", stroke "gold", points pointsStr] []]


calculateDiffractionPattern : Slits -> Screen -> Wavelength -> DiffractionPattern
calculateDiffractionPattern slits screen lambda =
    let
        (xSlits, ySlits) = calculateSlitsPosition
        y0Screen = toFloat screen.y1
        y1Screen = toFloat screen.y2
        xScreen  = toFloat screen.x
        screenHeight = y1Screen - y0Screen

        calculateSlitsPosition =
            let
                tanT1 = tan (toFloat screen.theta1 * pi/180)
                tanT2 = tan (toFloat screen.theta2 * pi/180)
                ySlits = (y0Screen + screenHeight * tanT2) / (tanT2 - tanT1)
                xSlits = if screen.theta1 == 0
                            then xScreen + (ySlits - y0Screen) / tanT2
                            else xScreen - screenHeight / tanT2
            in
                (xSlits, ySlits)

        l = xScreen - xSlits
        twoPiOverLambda = 2 * pi / lambda
        zip = List.map2 (,)

        calculateIntensityFromY y (is, maxI) =
            if y > screenHeight - ySlits
                then
                    (is, maxI)
                else
                    let
                        r2 = y*y + l*l -- radial distance from the slits to the current point on the screen
                        r  = sqrt r2
                        factor = twoPiOverLambda * (y/r - 1e-7)
                        (k, m) = doSlits factor (0,0) slits
                        i = (k*k + m*m) / (factor * factor * r2)
                        newMax = if i > maxI then i else maxI
                    in
                        calculateIntensityFromY (y + 1) (i :: is, newMax)

        scaleIntensities : (List Float, Float) -> List Int
        scaleIntensities (is, maxI) =
            let
                -- Let the maximum point on the graph be half the distance between the screen and slits
                iFactor = l / 2
                scale i = screen.x - round (i * iFactor / maxI)
            in
                List.map scale is

    in
        calculateIntensityFromY (y0Screen - ySlits) ([], 0)
            |> scaleIntensities
            |> List.reverse
            |> zip (List.range screen.y1 screen.y2 )
            -- Add the screen endpoints so that the pattern fill works correctly as a polygon
            |> (::) (screen.y1, screen.x)
            |> (::) (screen.y2, screen.x)

doSlits : Float -> (Float, Float) -> Slits -> (Float, Float)
doSlits factor (k, m) slits =
    case slits of
        [] -> (k, m)
        (Slit y1_ y2_ :: ss) ->
            let
                y1 = toFloat y1_
                y2 = toFloat y2_
                k_ = k + cos (y1 * factor) - cos (y2 * factor)
                m_ = m + sin (y1 * factor) - sin (y2 * factor)
            in
                doSlits factor (k_, m_) ss

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
