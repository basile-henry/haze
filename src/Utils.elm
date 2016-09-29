module Utils exposing (..)

import Color exposing (..)
import List exposing (..)


type alias Pos =
    ( Float, Float )


indexedFilterMap : (Int -> a -> Maybe b) -> List a -> List b
indexedFilterMap f list =
    let
        indexedFilterMap' : (Int -> a -> Maybe b) -> List a -> Int -> List b
        indexedFilterMap' f list i =
            case list of
                [] ->
                    []

                a :: xs ->
                    case f i a of
                        Nothing ->
                            indexedFilterMap' f xs (i + 1)

                        Just b ->
                            b :: indexedFilterMap' f xs (i + 1)
    in
        indexedFilterMap' f list 0


isEven : Int -> Bool
isEven x =
    x % 2 == 0


timeColor : Color
timeColor =
    rgb 37 110 255


timeColorT : Color
timeColorT =
    rgba 37 110 255 0


hexBackgroundColor : Color
hexBackgroundColor =
    rgb 234 230 239


wallColor : Color
wallColor =
    rgb 61 220 151


outlineColor : Color
outlineColor =
    rgb 216 209 226


backgroundColor : Color
backgroundColor =
    rgb 252 252 252


backgroundColorT : Color
backgroundColorT =
    rgba 252 252 252 0


shuffleColor : Color
shuffleColor =
    rgb 255 73 92


shuffleColorT : Color
shuffleColorT =
    rgba 255 73 92 0


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat a b dt =
    (1 - dt) * a + dt * b


interpolateInt : Int -> Int -> Float -> Int
interpolateInt a b dt =
    round <|
        interpolateFloat
            (toFloat a)
            (toFloat b)
            dt


interpolateColor : Bool -> Color -> Color -> Float -> Color
interpolateColor reverseHue a b dt =
    let
        ca =
            toHsl a

        t =
            toHsl b

        cb =
            if reverseHue then
                { t | hue = t.hue + degrees 360 }
            else
                t
    in
        hsla
            (interpolateFloat ca.hue cb.hue dt)
            (interpolateFloat ca.saturation cb.saturation dt)
            (interpolateFloat ca.lightness cb.lightness dt)
            (interpolateFloat ca.alpha cb.alpha dt)


interpolatePos : Pos -> Pos -> Float -> Pos
interpolatePos ( ax, ay ) ( bx, by ) dt =
    ( interpolateFloat ax bx dt
    , interpolateFloat ay by dt
    )
