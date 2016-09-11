module Utils exposing (..)

import Color exposing (..)
import List exposing (..)

type alias Pos = (Float, Float)

indexedFilterMap : (Int -> a -> Maybe b) -> List a -> List b
indexedFilterMap f list =
  let
    indexedFilterMap' : (Int -> a -> Maybe b) -> List a -> Int -> List b
    indexedFilterMap' f list i = case list of
      []        -> []
      (a :: xs) -> case f i a of
        Nothing -> indexedFilterMap' f xs (i + 1)
        Just b  -> b :: indexedFilterMap' f xs (i + 1)
  in
    indexedFilterMap' f list 0

isEven : Int -> Bool
isEven x = x % 2 == 0

timeColor : Color
timeColor = rgba 37 110 255 1

timeColorT : Color
timeColorT = rgba 37 110 255 0

hexBackgroundColor : Color
hexBackgroundColor = rgba 70 35 122 0.1

wallColor : Color
wallColor = rgba 61 220 151 1

outlineColor : Color
outlineColor = rgba 70 35 122 0.2

backgroundColor : Color
backgroundColor = rgba 252 252 252 1

backgroundColorT : Color
backgroundColorT = rgba 252 252 252 0

shuffleColor : Color
shuffleColor = rgba 255 73 92 1

shuffleColorT : Color
shuffleColorT = rgba 255 73 92 0

interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat a b dt =
  (1 - dt) * a + dt * b

interpolateInt : Int -> Int -> Float -> Int
interpolateInt a b dt = round
                     <| interpolateFloat
                          (toFloat a)
                          (toFloat b)
                          dt

interpolateColor : Color -> Color -> Float -> Color
interpolateColor a b dt =
  let
    ca = toHsl a
    cb = toHsl b
  in
    hsla
      (interpolateFloat ca.hue        cb.hue        dt)
      (interpolateFloat ca.saturation cb.saturation dt)
      (interpolateFloat ca.lightness  cb.lightness  dt)
      (interpolateFloat ca.alpha      cb.alpha      dt)

interpolatePos : Pos -> Pos -> Float -> Pos
interpolatePos (ax, ay) (bx, by) dt =
  ( interpolateFloat ax bx dt
  , interpolateFloat ay by dt
  )
