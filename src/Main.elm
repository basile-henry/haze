module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Task exposing (..)
import Window exposing (..)
import Mouse exposing (..)
import Color exposing (..)
import Element exposing (toHtml, color)
import Collage exposing (..)
import Maze
import Ease
import Utils exposing (..)
import Random exposing (initialSeed)
import AnimationFrame exposing (..)
import Time exposing (Time, now)
import List exposing (member)
import Text exposing (monospace, fromString)

type State
  = Moving   { nextCell : Maze.Index, alpha : Float }
  | CheckOrbs
  | Choosing Float
  | NewMaze  { oldMaze : Maze.Model, newMaze : Maze.Model, alpha : Float }
  | GameOver

type alias Model =
  { windowSize  : Size
  , mousePos    : Position
  , state       : State
  , speedBonus  : Float
  , maze        : Maze.Model
  , cell        : Maze.Index
  , shuffleOrbs : List Maze.Index
  , timeOrbs    : List Maze.Index
  , points      : Int
  , timeLeft    : Time
  , fov         : Float
  , radius      : Float
  }

initialModel : Model
initialModel =
  { windowSize  =
    { width  = 0
    , height = 0
    }
  , mousePos    =
    { x = 0
    , y = 0
    }
  , state       = Choosing 0.7
  , speedBonus  = 0
  , maze        = Maze.init 40 40
  , cell        = { x = 20, y = 20 }
  , shuffleOrbs = []
  , timeOrbs    = []
  , points      = 0
  , timeLeft    = 130
  , fov         = 300
  , radius      = 30
  }

type Msg
  = Window Size
  | Mouse Position
  | Step Time
  | GenMaze Maze.Model
  | UpdateOrbs Time

updateRadius : Model -> Float
updateRadius model = 0.5 * model.radius + 0.5 * model.fov * 15 / model.timeLeft

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if model.timeLeft < 15 then
    { model | state = GameOver } ! [Cmd.none]
  else
    case (msg, model.state) of
      (_, GameOver) -> model ! [Cmd.none]

      (Window s, _) ->
        let
          fov = min (toFloat s.width/2) (toFloat s.height/2)
        in
          { model | windowSize = s
                  , fov        = fov
          } ! [Cmd.none]
      
      (Mouse p, _) -> { model | mousePos = p } ! [Cmd.none]

      (Step dt, Choosing coolDown) ->
        let
          w = model.windowSize.width
          h = model.windowSize.height
          x = toFloat model.mousePos.x - toFloat w / 2
          y = toFloat h / 2 - toFloat model.mousePos.y
          angle = atan2 y x
          dir = floor (3 * angle / pi) % 6
          newCoolDown = max 0.0 <| coolDown - (dt / 1000)
        in
          (if member dir (Maze.validDirections model.maze model.cell)
          then
            { model | state = Moving { nextCell = Maze.getNeighbour model.cell dir
                                     , alpha    = 0
                                     }
                    , speedBonus = if newCoolDown > 0
                                   then min 1.0 <| model.speedBonus + 0.03
                                   else 0
                    , timeLeft   = model.timeLeft - dt / 1000
                    , radius     = updateRadius model
            }
          else
            { model | state      = Choosing newCoolDown
                    , speedBonus = if newCoolDown > 0
                                   then model.speedBonus
                                   else 0
                    , timeLeft   = model.timeLeft - dt / 1000
                    , radius     = updateRadius model
              }
          ) ! [Cmd.none]

      (Step dt, Moving mov) ->
        let
          mt = (1.2 + model.speedBonus) * dt / 1000
        in
          (if mov.alpha + mt > 1.0
            then
              { model | state    = CheckOrbs
                      , cell     = mov.nextCell
                      , points   = model.points + 1
                      , timeLeft = model.timeLeft - dt / 1000
                      , radius   = updateRadius model
              }
            else
              { model | state    = Moving { mov | alpha = mov.alpha + mt }
                      , timeLeft = model.timeLeft - dt / 1000
                      , radius   = updateRadius model
              }
          ) ! [Cmd.none]

      (Step dt, CheckOrbs) ->
        if member model.cell model.shuffleOrbs
        then
          { model | state       = Choosing 10
                  -- Choosing here is just in case newMaze fails, but it shouldn't
                  , shuffleOrbs = List.filter ((/=) model.cell) model.shuffleOrbs
                  , points      = model.points + 25
                  , timeLeft    = model.timeLeft - dt / 1000
                  , radius      = updateRadius model
          } ! [newMaze, newOrbs]
        else
          if member model.cell model.timeOrbs
          then
            { model | state    = Choosing 1.0 -- a bit more time than usual
                    , timeOrbs = List.filter ((/=) model.cell) model.timeOrbs
                    , timeLeft = min 150 <| model.timeLeft + 50 - dt / 1000
                    , radius   = updateRadius model
            } ! [newOrbs]
          else
            { model | state    = Choosing 0.7
                    , timeLeft = model.timeLeft - dt / 1000
                    , radius   = updateRadius model
            } ! [Cmd.none]

      (Step dt, NewMaze m) ->
        (if m.alpha + dt / 1000 > 1.0
          then
            { model | maze     = m.newMaze
                    , state    = Choosing 1.0 -- a bit more time than usual
                    , timeLeft = model.timeLeft - dt / 1000
                    , radius   = updateRadius model
            }
          else
            { model | state    = NewMaze { m | alpha = m.alpha + dt / 1000 }
                    , timeLeft = model.timeLeft - dt / 1000
                    , radius   = updateRadius model
            }
        ) ! [Cmd.none]

      (GenMaze m, _) ->
        ({ model | state = NewMaze { oldMaze = model.maze
                                   , newMaze = m
                                   , alpha   = 0
                                   }
        }) ! [Cmd.none]

      (UpdateOrbs t, _) ->
        let
          maxIndex = { x = 39, y = 39 }
          seed = initialSeed <| round t

          (newShuffleOrbs, newSeed) =
            updateOrbList 20 maxIndex seed (model.cell :: model.timeOrbs) model.shuffleOrbs
          (newTimeOrbs, _) =
            updateOrbList 30 maxIndex newSeed (model.cell :: newShuffleOrbs) model.timeOrbs
        in
          ({ model | shuffleOrbs = Debug.log "shuffle" newShuffleOrbs
                   , timeOrbs    = Debug.log "time"    newTimeOrbs
          }) ! [Cmd.none]

view : Model -> Html Msg
view model =
  let
    w = model.windowSize.width
    h = model.windowSize.height
    x = toFloat model.mousePos.x - toFloat w / 2
    y = toFloat h / 2 - toFloat model.mousePos.y

    settings'' = Maze.defaultViewSettings
    settings' = { settings'' | radius = model.radius
                             , cellA  = model.cell
                             , alpha  = 0
                }
    settings = case model.state of
      Moving mov ->
        { settings' | cellA  = model.cell
                    , cellB  = mov.nextCell
                    , alpha  = Ease.inOutQuad mov.alpha
        }

      NewMaze m ->
        { settings' | transition = Just m.newMaze }

      _ -> settings'

    r    = settings.radius / settings.scale -- adjusting for scaling
    func = drawOrbs r model.fov model.timeLeft
    pos  = interpolatePos
            (Maze.getCellPos settings.radius model.cell)
            (Maze.getCellPos settings.radius settings.cellB)
            settings.alpha

    shuffleOrbsF = List.map (func shuffleColor shuffleColorT << Maze.getRelativeCellPos settings.radius pos)
                <| model.shuffleOrbs
    timeOrbsF    = List.map (func timeColor    timeColorT    << Maze.getRelativeCellPos settings.radius pos)
                <| model.timeOrbs
  in
    toHtml
      <| color backgroundColor
      <| collage w h
      <| [ Maze.draw
             settings
             model.maze
         , move (0, 0)
             <| rotate (atan2 y x)
             <| filled (interpolateColor shuffleColor wallColor
                        <| Ease.inOutSine model.speedBonus)
             <| ngon 3 (0.8 * r)
         , gradient
             (radial
                (0, 0) (0.8 * model.fov)
                (0, 0) model.fov
                [(0, backgroundColorT), (1.0, backgroundColor)])
             (rect (toFloat w) (toFloat h))
         ] ++ shuffleOrbsF ++ timeOrbsF ++
         [ moveY (toFloat h/2 - 20)
             <| Collage.text
             <| Text.color lightCharcoal
             <| Text.height 30
             <| monospace
             <| fromString
             <| toString model.points
         ] ++ if model.state == GameOver
              then
                [ Collage.text
                   <| Text.color lightCharcoal
                   <| Text.height 80
                   <| monospace
                   <| fromString "Game Over"
                ]
              else []

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ resizes Window
    , moves Mouse
    , diffs Step
    ]

initialCmd : Cmd Msg
initialCmd =
  Cmd.batch
    [ perform (\_ -> Window { width = 500, height = 500 }) Window size
    , newMaze
    , newOrbs
    ]

newMaze : Cmd Msg
newMaze = perform (always <| GenMaze <| Maze.init 40 40) GenMaze
       <| Task.map (\t -> Maze.generate (initialSeed <| round t)
                       <| Maze.init 40 40)
       <| now

newOrbs : Cmd Msg
newOrbs = perform (always <| UpdateOrbs 0) UpdateOrbs now

indexGen : Maze.Index -> Random.Generator Maze.Index
indexGen maxIndex =
  let
    x = Random.int 0 maxIndex.x
    y = Random.int 0 maxIndex.y
    g = Random.pair x y
  in
    Random.map (\(x, y) -> { x = x, y = y }) g

updateOrbList : Int -> Maze.Index -> Random.Seed -> List Maze.Index -> List Maze.Index -> (List Maze.Index, Random.Seed)
updateOrbList len maxIndex seed blacklist list =
  if len > List.length list
  then
    let
      (newOrb, newSeed) = Random.step (indexGen maxIndex) seed
    in
      updateOrbList len maxIndex newSeed blacklist
        <| if member newOrb (blacklist ++ list)
           then list
           else newOrb :: list
  else (list, seed)

drawOrbs : Float -> Float -> Time -> Color -> Color -> Pos -> Form
drawOrbs radius maxRadius t f b (x, y) =
  let
    dist = sqrt <| x*x + y*y
    pos  = if dist > maxRadius
           then ( maxRadius * x / dist
                , maxRadius * y / dist
                )
           else (x, y)
    r = radius * (0.2 + (abs <| sin t) * max 0 (2 * maxRadius - dist) / maxRadius)
    g = radial (0, 0) (0.1 * radius) (0, 0) r [(0, f), (0.9, b)]
  in
    move pos <| gradient g <| circle r

main : Program Never
main =
  Html.program
    { init          = (initialModel, initialCmd)
    , update        = update
    , view          = view
    , subscriptions = subscriptions
    }
