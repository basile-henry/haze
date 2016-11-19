module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, attribute, class, href)
import Task exposing (..)
import Window exposing (..)
import Mouse exposing (..)
import Keyboard
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
    = Moving { nextCell : Maze.Index, alpha : Float }
    | CheckOrbs
    | Choosing Float
    | NewMaze Float
    | GameOver
    | Pause State


type alias Model =
    { windowSize : ( Float, Float )
    , mousePos : Pos
    , state : State
    , speedBonus : Float
    , maze : Maze.Model
    , newMaze : Maze.Model
    , cell : Maze.Index
    , shuffleOrbs : List Maze.Index
    , timeOrbs : List Maze.Index
    , points : Int
    , timeLeft : Time
    , fov : Float
    , radius : Float
    }



-- These need to be even (for consistancy throughout the code base)


( mazeWidth, mazeHeight ) =
    ( 18, 18 )


winScale : Float
winScale =
    0.98


initMaze : Maze.Model
initMaze =
    Maze.init mazeWidth mazeHeight


initialModel : Model
initialModel =
    { windowSize = ( 0, 0 )
    , mousePos = ( 0, 0 )
    , state = Choosing 2.0
    , speedBonus = 0
    , maze = initMaze
    , newMaze = initMaze
    , cell = { x = 10, y = 10 }
    , shuffleOrbs = []
    , timeOrbs = []
    , points = 0
    , timeLeft = 130
    , fov = 300
    , radius = 30
    }


type Msg
    = Window ( Float, Float )
    | Mouse Pos
    | Step Time
    | GenMaze Maze.Model
    | GenMazeStart Maze.Model
    | UpdateOrbs Time
    | TogglePause


updateRadius : Model -> Float
updateRadius model =
    0.5 * model.radius + 0.5 * model.fov * 15 / model.timeLeft


updateTimeLeft : Model -> Float -> Float
updateTimeLeft model dt =
    let
        mult =
            1 + 4 * (min 1500 <| toFloat model.points) / 1500
    in
        model.timeLeft - (mult * dt / 1000)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.timeLeft < 15 then
        { model | state = GameOver } ! [ Cmd.none ]
    else
        case ( msg, model.state ) of
            ( TogglePause, Pause savedState ) ->
                { model | state = savedState } ! [ Cmd.none ]

            ( TogglePause, _ ) ->
                { model | state = Pause model.state } ! [ Cmd.none ]

            ( _, Pause _ ) ->
                model ! [ Cmd.none ]

            ( _, GameOver ) ->
                model ! [ Cmd.none ]

            ( Window s, _ ) ->
                let
                    ( w, h ) =
                        s

                    fov =
                        0.95 * min (w / 2) (h / 2)
                in
                    { model
                        | windowSize = s
                        , fov = fov
                    }
                        ! [ Cmd.none ]

            ( Mouse p, _ ) ->
                { model | mousePos = p } ! [ Cmd.none ]

            ( Step dt, Choosing coolDown ) ->
                let
                    ( w, h ) =
                        model.windowSize

                    ( mx, my ) =
                        model.mousePos

                    ( x, y ) =
                        ( mx - w / 2
                        , h / 2 - my
                        )

                    angle =
                        atan2 y x

                    dir =
                        floor (3 * angle / pi) % 6

                    newCoolDown =
                        max 0.0 <| coolDown - (dt / 1000)
                in
                    (if member dir (Maze.validDirections model.maze model.cell) then
                        { model
                            | state =
                                Moving
                                    { nextCell = Maze.getNeighbour model.cell dir
                                    , alpha = 0
                                    }
                            , speedBonus =
                                if newCoolDown > 0 then
                                    min 1.0 <| model.speedBonus + 0.03
                                else
                                    0
                            , timeLeft = updateTimeLeft model dt
                            , radius = updateRadius model
                        }
                     else
                        { model
                            | state = Choosing newCoolDown
                            , speedBonus =
                                if newCoolDown > 0 then
                                    model.speedBonus
                                else
                                    0
                            , timeLeft = updateTimeLeft model dt
                            , radius = updateRadius model
                        }
                    )
                        ! [ Cmd.none ]

            ( Step dt, Moving mov ) ->
                let
                    mt =
                        1.5 * (1.0 + model.speedBonus) * dt / 1000
                in
                    (if mov.alpha + mt > 1.0 then
                        { model
                            | state = CheckOrbs
                            , cell = Maze.getModuloIndex model.maze.grid mov.nextCell
                            , points = model.points + 1
                            , timeLeft = updateTimeLeft model dt
                            , radius = updateRadius model
                        }
                     else
                        { model
                            | state = Moving { mov | alpha = mov.alpha + mt }
                            , timeLeft = updateTimeLeft model dt
                            , radius = updateRadius model
                        }
                    )
                        ! [ Cmd.none ]

            ( Step dt, CheckOrbs ) ->
                if member model.cell model.shuffleOrbs then
                    { model
                        | state = NewMaze 0.7
                        , shuffleOrbs = List.filter ((/=) model.cell) model.shuffleOrbs
                        , points = model.points + 25
                        , timeLeft = updateTimeLeft model dt
                        , radius = updateRadius model
                    }
                        ! [ newOrbs ]
                else if member model.cell model.timeOrbs then
                    { model
                        | state =
                            Choosing 0.7
                            -- a bit more time than usual
                        , timeOrbs = List.filter ((/=) model.cell) model.timeOrbs
                        , timeLeft = min 150 <| model.timeLeft + 30 - dt / 1000
                        , radius = updateRadius model
                    }
                        ! [ newOrbs ]
                else
                    { model
                        | state = Choosing 0.5
                        , timeLeft = updateTimeLeft model dt
                        , radius = updateRadius model
                    }
                        ! [ Cmd.none ]

            ( Step dt, NewMaze alpha ) ->
                if alpha < 0.0 then
                    ({ model
                        | maze = model.newMaze
                        , state =
                            Choosing 0.7
                            -- a bit more time than usual
                        , radius = updateRadius model
                     }
                    )
                        ! [ newMaze ]
                else
                    ({ model
                        | state = NewMaze <| alpha - (dt / 1000)
                        , radius = updateRadius model
                     }
                    )
                        ! [ Cmd.none ]

            ( GenMaze m, _ ) ->
                ({ model | newMaze = m }) ! [ Cmd.none ]

            ( GenMazeStart m, _ ) ->
                ({ model | maze = m }) ! [ newMaze ]

            ( UpdateOrbs t, _ ) ->
                let
                    maxIndex =
                        Maze.Index (mazeWidth - 1) (mazeHeight - 1)

                    seed =
                        initialSeed <| round t

                    ( newShuffleOrbs, newSeed ) =
                        updateOrbList 15 maxIndex seed (model.cell :: model.timeOrbs) model.shuffleOrbs

                    ( newTimeOrbs, _ ) =
                        updateOrbList 20 maxIndex newSeed (model.cell :: newShuffleOrbs) model.timeOrbs
                in
                    ({ model
                        | shuffleOrbs = newShuffleOrbs
                        , timeOrbs = newTimeOrbs
                     }
                    )
                        ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    let
        ( w, h ) =
            model.windowSize

        ( mx, my ) =
            model.mousePos

        ( x, y ) =
            ( mx - w / 2
            , h / 2 - my
            )

        settings__ =
            Maze.defaultViewSettings

        settings_ =
            { settings__
                | radius = model.radius
                , cellFrom = model.cell
                , alpha = 0
            }

        settings =
            case model.state of
                Moving mov ->
                    { settings_
                        | cellFrom = model.cell
                        , cellTo = Just mov.nextCell
                        , alpha = Ease.inOutQuad mov.alpha
                    }

                NewMaze m ->
                    { settings_ | transition = Just model.newMaze }

                _ ->
                    settings_

        r =
            settings.radius / settings.scale

        -- adjusting for scaling
        func =
            drawOrbs r model.fov model.timeLeft

        pos =
            interpolatePos
                (Maze.getCellPos settings.radius model.cell)
                (settings.cellTo
                    |> Maybe.withDefault model.cell
                    |> Maze.getCellPos settings.radius
                )
                settings.alpha

        shuffleOrbsF =
            List.map (func shuffleColor shuffleColorT << Maze.getRelativeCellPos ( mazeWidth, mazeHeight ) settings.radius pos) <|
                model.shuffleOrbs

        timeOrbsF =
            List.map (func timeColor timeColorT << Maze.getRelativeCellPos ( mazeWidth, mazeHeight ) settings.radius pos) <|
                model.timeOrbs
    in
        div
            [ style
                [ ( "width", toString w ++ "px" )
                , ( "height", toString h ++ "px" )
                , ( "position", "relative" )
                ]
            ]
            [ toHtml <|
                color backgroundColor <|
                    collage (floor w) (floor h) <|
                        [ Maze.draw
                            settings
                            model.maze
                        , player
                            |> filled
                                (interpolateColor True shuffleColor wallColor <| Ease.inOutSine model.speedBonus)
                            |> scale r
                            |> rotate (atan2 y x)
                        , gradient
                            (radial
                                ( 0, 0 )
                                (0.7 * model.fov)
                                ( 0, 0 )
                                model.fov
                                [ ( 0, backgroundColorT ), ( 1.0, backgroundColor ) ]
                            )
                            (rect w h)
                        ]
                            ++ shuffleOrbsF
                            ++ timeOrbsF
                            ++ [ model.points
                                    |> toString
                                    |> fromString
                                    |> monospace
                                    |> Text.height 30
                                    |> Text.color lightCharcoal
                                    |> Collage.text
                                    |> moveY (model.fov - 20)
                               ]
                            ++ case model.state of
                                GameOver ->
                                    [ "Game Over"
                                        |> fromString
                                        |> monospace
                                        |> Text.height 80
                                        |> Text.color lightCharcoal
                                        |> Collage.text
                                    ]

                                Pause _ ->
                                    [ "Pause"
                                        |> fromString
                                        |> monospace
                                        |> Text.height 80
                                        |> Text.color lightCharcoal
                                        |> Collage.text
                                    ]

                                NewMaze alpha ->
                                    let
                                        m =
                                            5 * (0.7 - alpha)
                                    in
                                        [ "+25"
                                            |> fromString
                                            |> monospace
                                            |> Text.height 30
                                            |> Text.color lightCharcoal
                                            |> Collage.text
                                            |> Collage.move ( 20 * m, m ^ 4 )
                                        ]

                                _ ->
                                    []
            ]


player : Shape
player =
    let
        f n i =
            let
                a =
                    (degrees 130) + (degrees 100) * i / n
            in
                ( negate (0.7 * cos a + 1.1), 0.7 * sin a )

        a =
            degrees 120

        ( dx, dy ) =
            ( cos a, sin a )
    in
        polygon <| ( 1, 0 ) :: ( dx, dy ) :: List.map (f 10 << toFloat) (List.range 0 10) ++ [ ( dx, -dy ) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ resizes windowMsg
        , moves mouseMsg
        , diffs Step
        , Keyboard.downs (always TogglePause)
        ]


windowMsg : Size -> Msg
windowMsg s =
    Window
        ( winScale * toFloat s.width
        , winScale * toFloat s.height
        )


mouseMsg : Position -> Msg
mouseMsg m =
    Mouse
        ( toFloat m.x
        , toFloat m.y
        )


initialCmd : Cmd Msg
initialCmd =
    Cmd.batch
        [ perform windowMsg size
        , newOrbs
        , newMaze
        , perform GenMazeStart genMaze
        ]


genMaze : Task Never Maze.Model
genMaze =
    Task.map (\t -> Maze.generate (initialSeed <| round t) initMaze) now


newMaze : Cmd Msg
newMaze =
    perform GenMaze genMaze


newOrbs : Cmd Msg
newOrbs =
    perform UpdateOrbs now


indexGen : Maze.Index -> Random.Generator Maze.Index
indexGen maxIndex =
    let
        ( x, y ) =
            ( Random.int 0 maxIndex.x
            , Random.int 0 maxIndex.y
            )

        g =
            Random.pair x y
    in
        Random.map (\( x, y ) -> { x = x, y = y }) g


updateOrbList : Int -> Maze.Index -> Random.Seed -> List Maze.Index -> List Maze.Index -> ( List Maze.Index, Random.Seed )
updateOrbList len maxIndex seed blacklist list =
    if len > List.length list then
        let
            ( newOrb, newSeed ) =
                Random.step (indexGen maxIndex) seed
        in
            updateOrbList len maxIndex newSeed blacklist <|
                if member newOrb (blacklist ++ list) then
                    list
                else
                    newOrb :: list
    else
        ( list, seed )


drawOrbs : Float -> Float -> Time -> Color -> Color -> Pos -> Form
drawOrbs radius maxRadius t f b ( x, y ) =
    let
        sqrDist =
            x * x + y * y

        pos =
            if sqrDist > maxRadius * maxRadius then
                let
                    d =
                        sqrt sqrDist
                in
                    ( maxRadius * x / d
                    , maxRadius * y / d
                    )
            else
                ( x, y )

        r =
            radius * (0.2 + (abs <| sin (t * 0.6)) * max 0 (2 * maxRadius * maxRadius - sqrDist) / (maxRadius * maxRadius))

        g =
            radial ( 0, 0 ) (0.1 * radius) ( 0, 0 ) r [ ( 0, f ), ( 0.9, b ) ]
    in
        move pos <| gradient g <| circle r


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, initialCmd )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
