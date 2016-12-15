module Model exposing (Model, DiffractionPattern, Msg(..), Screen, Slit(..), Wavelength, getSlits, startDrag, doDrag, stopDrag)

{-| The slits model -}
type Slit = Slit Int Int

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
type Drag = Drag DragType Slit

--| A list of y coordinate and intensity value pairs
type alias DiffractionPattern = List (Int, Int)

--| Model is the array of static slits plus the slit being dragged
type alias Model =
    { slits : List Slit
    , screen : Screen
    , lambda : Wavelength
    , drag : Maybe Drag
    }

{-| The messages that are handled by the application

For dragging we only care about the y coordinate.
-}
type Msg
    = DragStart Int
    | DragAt    Int
    | DragEnd


{-| Provides the complete list of slits, including the dragged one, if any. -}
getSlits : Model -> List Slit
getSlits m = case m.drag of
    Nothing -> m.slits
    Just (Drag _ s) -> s :: m.slits


{-| Begin dragging a slit, if the given y-coordinate intesects one. -}
startDrag : Int -> Model -> Model
startDrag y ({slits, screen, lambda, drag} as m) =
    let
        selectedSlit = slitAtY slits y
        -- Remove the selected slit from the list while we drag it about
        staticSlits = case selectedSlit of
            Nothing -> slits
            Just s  -> List.filter ((/=) s) slits
        dragType s = WholeSlit
    in
        { m | slits = staticSlits, drag = Maybe.map (\s -> Drag (dragType s) s) selectedSlit }


{-| Continue dragging a slit in response to a changing y-coordinate. -}
doDrag : Int -> Model -> Model
doDrag y m =
    let
        checkSlitPosition oldSlit (Slit y1 y2 as newSlit) =
            if y1 < 0 || y2 > 600 || List.any (intersects newSlit) m.slits
                then oldSlit
                else newSlit
        dragSlit (Drag dragType (Slit y1 y2 as s)) =
            Drag dragType
                <| checkSlitPosition s
                <| case dragType of
                    WholeSlit -> moveSlit s y
                    Top       -> changeSlitWidth (y - y2) s
                    Bottom    -> changeSlitWidth (y1 - y) s
    in
        { m | drag = Maybe.map dragSlit m.drag}

--| Remove any dragging state from the model
stopDrag : Model -> Model
stopDrag m =
    case m.drag of
        Just (Drag _ s) -> { m | slits = (s :: m.slits), drag = Nothing }
        Nothing -> m

slitAtY : List Slit -> Int -> Maybe Slit
slitAtY slits y =
    case slits of
        [] -> Nothing
        (s :: ss) -> if containsY y s then (Just s) else slitAtY ss y


containsY : Int -> Slit -> Bool
containsY y (Slit y1 y2) = y >= y1 && y <= y2


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
