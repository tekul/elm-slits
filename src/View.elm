module View exposing (view)

import Html exposing (..)
import Json.Decode as Json
import Model exposing (Model, Msg(..), Screen, Slit(..), DiffractionPattern, Wavelength, getSlits)
import Svg exposing (..)
import Svg.Attributes exposing (style, fill, fillOpacity, stroke, points, width, height, x, y)
import Svg.Events exposing (on)

view : Model -> Html Model.Msg
view m =
    let
        slits = getSlits m
        pattern = calculateDiffractionPattern slits m.screen m.lambda
    in
        svg
            [ width m.width
            , height m.height
            ]
            [ drawSlits slits
            , drawScreen m.screen
            , drawDiffractionPattern pattern
            ]


drawScreen : Screen -> Svg Msg
drawScreen {x , y1, y2} = line [stroke "black", Svg.Attributes.x1 (toString x), Svg.Attributes.y1 (toString y1), Svg.Attributes.x2 (toString x), Svg.Attributes.y2 (toString y2)] []


drawSlits : List Slit -> Svg Msg
drawSlits slits =
    let
        intField f = Json.field f Json.int
        onMouseDown = on "mousedown" <| Json.map2 DragStart (intField "pageY") (intField "offsetY")
        background = rect [ onMouseDown, width "80", height "600", fill "gray" ] []
        overlay = rect [ onMouseDown, width "80", height "600", fillOpacity "0" ] []
        drawSlit (Slit y1 y2) = rect [Svg.Attributes.style "cursor: move", y (toString y1), width "80", height (toString (y2 - y1)), fill "black" ] []
    in
        g []
            (background :: (List.map drawSlit slits) ++ [overlay])


drawDiffractionPattern : DiffractionPattern -> Svg Msg
drawDiffractionPattern pattern =
    let
        pointsStr = pattern
            |> List.map (\(y, x) -> toString x ++ "," ++ toString y)
            |> String.join " "
    in
        g []
            [polygon [fill "gold", stroke "gold", points pointsStr] []]


calculateDiffractionPattern : List Slit -> Screen -> Wavelength -> DiffractionPattern
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

        l = abs (xScreen - xSlits)
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

doSlits : Float -> (Float, Float) -> List Slit -> (Float, Float)
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
