module Main exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Ease
import Element exposing (toHtml, color)
import Html exposing (..)
import Html.Attributes exposing (style, attribute, class, href)
import Keyboard
import List exposing (member)
import Maze
import Mouse exposing (..)
import Random exposing (initialSeed)
import Random.List
import Task exposing (..)
import Text exposing (Text, typeface, fromString)
import Time exposing (Time, now, inMilliseconds, inSeconds)
import Touch
import SingleTouch as Touch
import Utils exposing (..)
import Window exposing (..)


-- Debug

import Debug


type State
    = Moving { nextCell : Maze.Index, alpha : Float }
    | CheckOrbs
    | Choosing Float
    | NewMaze Float
    | GameOver
    | Pause State


type PowerUp
    = BreakWall
    | SpeedUp Float
    | GetPoints


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
    , powerUpOrbs : List Maze.Index
    , points : Int
    , timeLeft : Time
    , fov : Float
    , radius : Float
    , storedPowerUp : Maybe PowerUp
    , currentPowerUp : Maybe PowerUp
    }



-- These need to be even (for consistency throughout the code base)


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
    , powerUpOrbs = []
    , points = 0
    , timeLeft = 130
    , fov = 300
    , radius = 0
    , storedPowerUp = Nothing
    , currentPowerUp = Nothing
    }


type Msg
    = Window ( Float, Float )
    | Mouse Pos
    | Touch Touch.Touch
    | Step Time
    | GenMaze Maze.Model
    | GenMazeStart Maze.Model
    | UpdateOrbs Time
    | TogglePause
    | NewPowerUp Float
    | UsePowerUp


updateRadius : Model -> Float
updateRadius model =
    0.7 * model.radius + 0.3 * model.fov * 15 / model.timeLeft


updateTimeLeft : Model -> Float -> Float
updateTimeLeft model dt =
    let
        mult =
            1 + 4 * (min 1500 <| toFloat model.points) / 1500
    in
        model.timeLeft - (mult * inSeconds dt)


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

            ( Touch t, _ ) ->
                { model | mousePos = ( t.clientX, t.clientY ) } ! [ Cmd.none ]

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
                        max 0.0 <| coolDown - (inSeconds dt)

                    successfulMoveModel =
                        { model
                            | state =
                                Moving
                                    { nextCell = Maze.getNeighbour model.cell dir
                                    , alpha = 0
                                    }
                            , speedBonus =
                                if newCoolDown > 0 then
                                    min 1.0 <| model.speedBonus + 0.04
                                else
                                    0
                            , timeLeft = updateTimeLeft model dt
                            , radius = updateRadius model
                        }
                in
                    (if member dir (Maze.validDirections model.maze model.cell) then
                        successfulMoveModel
                     else if model.currentPowerUp == Just BreakWall then
                        { successfulMoveModel | currentPowerUp = Nothing }
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
                    dts =
                        inSeconds dt

                    mt_ =
                        1.5 * (1.0 + model.speedBonus) * dts

                    ( mt, newCurrentPowerUp ) =
                        case model.currentPowerUp of
                            Just (SpeedUp t) ->
                                if t - dts < 0 then
                                    ( mt_, Nothing )
                                else
                                    ( 6 * dts, Just (SpeedUp <| t - dts) )

                            _ ->
                                ( mt_, model.currentPowerUp )
                in
                    (if mov.alpha + mt > 1.0 then
                        { model
                            | state = CheckOrbs
                            , cell = Maze.getModuloIndex model.maze.grid mov.nextCell
                            , points = model.points + 1
                            , timeLeft = updateTimeLeft model dt
                            , radius = updateRadius model
                            , currentPowerUp = newCurrentPowerUp
                        }
                     else
                        { model
                            | state = Moving { mov | alpha = mov.alpha + mt }
                            , timeLeft = updateTimeLeft model dt
                            , radius = updateRadius model
                            , currentPowerUp = newCurrentPowerUp
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
                        , timeLeft =
                            min 150 <| model.timeLeft + 30.7876 - inSeconds dt
                            -- 30.7876 is a multiple of 0.7 * 2 * pi (no animation skip animation for the orbs pulsing)
                        , radius = updateRadius model
                    }
                        ! [ newOrbs ]
                else if member model.cell model.powerUpOrbs then
                    { model
                        | state = Choosing 0.7
                        , powerUpOrbs = List.filter ((/=) model.cell) model.powerUpOrbs
                        , timeLeft = updateTimeLeft model dt
                        , radius = updateRadius model
                    }
                        ! [ newOrbs, perform NewPowerUp now ]
                else
                    { model
                        | state = Choosing 0.5
                        , timeLeft = updateTimeLeft model dt
                        , radius = updateRadius model
                    }
                        ! [ Cmd.none ]

            ( Step dt, NewMaze alpha ) ->
                if alpha < 0.0 then
                    { model
                        | maze = model.newMaze
                        , state =
                            Choosing 0.7
                            -- a bit more time than usual
                        , radius = updateRadius model
                    }
                        ! [ newMaze ]
                else
                    { model
                        | state = NewMaze <| alpha - (inSeconds dt)
                        , radius = updateRadius model
                    }
                        ! [ Cmd.none ]

            ( GenMaze m, _ ) ->
                { model | newMaze = m } ! [ Cmd.none ]

            ( GenMazeStart m, _ ) ->
                { model | maze = m } ! [ newMaze ]

            ( UpdateOrbs t, _ ) ->
                let
                    seed =
                        initialSeed <| round t

                    ( newShuffleOrbs, seed_ ) =
                        updateOrbList 15 seed (model.cell :: model.timeOrbs ++ model.powerUpOrbs) model.shuffleOrbs

                    ( newTimeOrbs, seed__ ) =
                        updateOrbList 20 seed_ (model.cell :: newShuffleOrbs ++ model.powerUpOrbs) model.timeOrbs

                    ( newPowerUpOrbs, _ ) =
                        updateOrbList 7 seed__ (model.cell :: newShuffleOrbs ++ newTimeOrbs) model.powerUpOrbs
                in
                    { model
                        | shuffleOrbs = newShuffleOrbs
                        , timeOrbs = newTimeOrbs
                        , powerUpOrbs = newPowerUpOrbs
                    }
                        ! [ Cmd.none ]

            ( NewPowerUp t, _ ) ->
                let
                    seed =
                        t
                            |> inMilliseconds
                            |> round
                            |> initialSeed

                    ( ( powerUp, _ ), _ ) =
                        Random.step (Random.List.choose [ BreakWall, SpeedUp 3, GetPoints ]) seed
                in
                    { model | storedPowerUp = powerUp } ! [ Cmd.none ]

            ( UsePowerUp, _ ) ->
                case model.storedPowerUp of
                    Nothing ->
                        model ! [ Cmd.none ]

                    Just GetPoints ->
                        { model | storedPowerUp = Nothing, points = model.points + 50 } ! [ Cmd.none ]

                    _ ->
                        { model | storedPowerUp = Nothing, currentPowerUp = model.storedPowerUp } ! [ Cmd.none ]


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

        movingSettings mov =
            { settings_
                | cellFrom = model.cell
                , cellTo = Just mov.nextCell
                , alpha = Ease.inOutQuad mov.alpha
            }

        newMazeSettings m =
            { settings_ | transition = Just model.newMaze }

        settings =
            case model.state of
                Moving mov ->
                    movingSettings mov

                Pause (Moving mov) ->
                    movingSettings mov

                NewMaze m ->
                    newMazeSettings m

                Pause (NewMaze m) ->
                    newMazeSettings m

                _ ->
                    settings_

        r =
            settings.radius / settings.scale

        -- adjusting for scaling
        func color colorT orb =
            orb
                |> Maze.getRelativeCellPos ( mazeWidth, mazeHeight ) settings.radius pos
                |> drawOrbs r model.fov model.timeLeft color colorT

        pos =
            interpolatePos
                (Maze.getCellPos settings.radius model.cell)
                (settings.cellTo
                    |> Maybe.withDefault model.cell
                    |> Maze.getCellPos settings.radius
                )
                settings.alpha

        shuffleOrbsF =
            List.map (func shuffleColor shuffleColorT) model.shuffleOrbs

        timeOrbsF =
            List.map (func timeColor timeColorT) model.timeOrbs

        powerUpOrbsF =
            List.map (func powerUpColor powerUpColorT) model.powerUpOrbs
    in
        div
            [ style
                [ ( "width", toString w ++ "px" )
                , ( "height", toString h ++ "px" )
                , ( "position", "relative" )
                ]
            , Touch.onSingleTouch Touch.TouchStart Touch.preventAndStop (Touch << .touch)
            , Touch.onSingleTouch Touch.TouchMove Touch.preventAndStop (Touch << .touch)
            , Touch.onSingleTouch Touch.TouchEnd Touch.preventAndStop (always UsePowerUp)
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
                            ++ powerUpOrbsF
                            ++ [ model.points
                                    |> toString
                                    |> fromString
                                    |> typefaced
                                    |> Text.height 30
                                    |> Text.color charcoal
                                    |> Collage.text
                                    |> moveY (model.fov - 20)
                               ]
                            ++ (case model.storedPowerUp of
                                    Nothing ->
                                        []

                                    Just powerUp ->
                                        [ powerUp
                                            |> powerUpForm
                                            |> moveY (40 - model.fov)
                                        ]
                               )
                            ++ case model.state of
                                GameOver ->
                                    [ "Game Over"
                                        |> fromString
                                        |> typefaced
                                        |> Text.height 80
                                        |> Text.color charcoal
                                        |> Collage.text
                                    ]

                                Pause _ ->
                                    [ "Pause"
                                        |> fromString
                                        |> typefaced
                                        |> Text.height 80
                                        |> Text.color charcoal
                                        |> Collage.text
                                    ]

                                NewMaze alpha ->
                                    let
                                        m =
                                            5 * (0.7 - alpha)
                                    in
                                        [ "+25"
                                            |> fromString
                                            |> typefaced
                                            |> Text.height 30
                                            |> Text.color charcoal
                                            |> Collage.text
                                            |> Collage.move ( 20 * m, m ^ 4 )
                                        ]

                                _ ->
                                    []
            ]


typefaced : Text -> Text
typefaced =
    typeface [ "helvetica", "arial", "sans-serif" ]


powerUpForm : PowerUp -> Form
powerUpForm powerUp =
    case powerUp of
        BreakWall ->
            outlined { defaultLine | color = powerUpColor, width = 3 } (ngon 6 30)

        SpeedUp _ ->
            let
                l =
                    path [ ( -25, 0 ), ( 0, 20 ), ( 25, 0 ) ]
                        |> traced { defaultLine | color = powerUpColor, width = 3 }
            in
                group [ moveY 10 l, moveY -10 l ]

        GetPoints ->
            "+50"
                |> fromString
                |> typefaced
                |> Text.height 60
                |> Text.color powerUpColor
                |> Collage.text


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
        , clicks (always UsePowerUp)
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



--indexGen : Maze.Index -> Random.Generator Maze.Index
--indexGen maxIndex =
--    let
--        ( x, y ) =
--            ( Random.int 0 maxIndex.x
--            , Random.int 0 maxIndex.y
--            )
--        g =
--            Random.pair x y
--    in
--        Random.map (\( x, y ) -> { x = x, y = y }) g


allIndices : List Maze.Index
allIndices =
    (mazeHeight - 1)
        |> List.range 0
        |> List.concatMap
            (\y ->
                (mazeWidth - 1)
                    |> List.range 0
                    |> List.map (\x -> Maze.Index x y)
            )


updateOrbList : Int -> Random.Seed -> List Maze.Index -> List Maze.Index -> ( List Maze.Index, Random.Seed )
updateOrbList len seed blacklist list =
    if len > List.length list then
        let
            availableIndices =
                List.filter (not << flip member (blacklist ++ list)) allIndices
        in
            case Random.step (Random.List.choose availableIndices) seed of
                ( ( Just newOrb, _ ), newSeed ) ->
                    updateOrbList len newSeed blacklist <| newOrb :: list

                ( ( Nothing, _ ), newSeed ) ->
                    -- No more availableIndices
                    ( list, newSeed )
    else
        ( list, seed )


drawOrbs : Float -> Float -> Time -> Color -> Color -> Pos -> Form
drawOrbs radius maxRadius t f b ( x, y ) =
    let
        sqrDist =
            x * x + y * y

        sqrMaxRadius =
            maxRadius * maxRadius

        pos =
            if sqrDist > sqrMaxRadius then
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
            radius * (0.2 + (abs <| sin (t * 0.7)) * max 0 (1.5 - sqrDist / sqrMaxRadius))

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
