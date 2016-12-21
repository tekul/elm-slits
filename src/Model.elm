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

{-| The slit we are dragging, the type of drag and the initial browser y coordinate -}
type Drag = Drag DragType Int Slit

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

For dragging we only care about the y coordinate. When we start dragging, we get both the browser y coordinate and
the offsetY coordinate within the slit svg. Storing the browser coordinate allows the relative offset to be
calculated from the elm mouse move events (which are browser coordinates).
-}
type Msg
    = DragStart Int Int
    | DragAt    Int
    | DragEnd


{-| Provides the complete list of slits, including the dragged one, if any. -}
getSlits : Model -> List Slit
getSlits m = case m.drag of
    Nothing -> m.slits
    Just (Drag _ _ s) -> s :: m.slits


{-| Begin dragging a slit, if the given y-coordinate intersects one. -}
startDrag : (Int, Int) -> Model -> Model
startDrag (yPage, y) ({slits, screen, lambda, drag} as m) =
    let
        -- The y-origin relative to the svg
        y0 = yPage - y
        selectedSlit = slitAtY slits y
        -- Remove the selected slit from the list while we drag it about
        staticSlits = case selectedSlit of
            Nothing -> slits
            Just s  -> List.filter ((/=) s) slits
        dragType (Slit y1 y2) =
            let
                mid = abs (y - (y1 + y2) // 2)
                top = abs (y - y2)
                bot = abs (y - y1)
            in
               if mid <= top && mid <= bot then
                   WholeSlit
               else if top < mid && top < bot then
                   Top
               else Bottom
    in
        { m | slits = staticSlits, drag = Maybe.map (\s -> Drag (dragType s) y0 s) selectedSlit }


{-| Continue dragging a slit in response to a changing y-coordinate. -}
doDrag : Int -> Model -> Model
doDrag y m =
    let
        minWidth = 1
        checkSlitPosition oldSlit (Slit y1 y2 as newSlit) =
            if y1 < 0 || y2 > 600 || (y2 - y1 < minWidth) || List.any (intersects newSlit) m.slits
                then oldSlit
                else newSlit
        dragSlit (Drag dragType y0 (Slit y1 y2 as s)) =
            Drag dragType y0
                <| checkSlitPosition s
                <| case dragType of
                    -- y relative to svg is y - y0
                    WholeSlit -> moveSlit s (y - y0)
                    Top       -> changeSlitWidth (y - y0 - y2) s
                    Bottom    -> changeSlitWidth (y1 - y - y0) s
    in
        { m | drag = Maybe.map dragSlit m.drag}

--| Remove any dragging state from the model
stopDrag : Model -> Model
stopDrag m =
    case m.drag of
        Just (Drag _ _ s) -> { m | slits = (s :: m.slits), drag = Nothing }
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
