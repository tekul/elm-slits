module Model exposing (..)

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

{-| The messages that are handled by the application

For dragging we only care about the y coordinate.
-}
type Msg
    = DragStart Int
    | DragAt    Int
    | DragEnd

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

--| Tolerance within which a coordinate will still be assumed to be "within" a slit
delta : Int
delta = 8

containsY : Int -> Slit -> Bool
containsY y (Slit y1 y2) = (y > y1 - delta) && (y < y2 + delta)

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
