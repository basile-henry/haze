module Main exposing (..)

import AnimationFrame exposing (..)
import Char exposing (toCode)
import Collage exposing (..)
import Color exposing (..)
import Ease
import Element exposing (toHtml, color)
import FontAwesome
import Html exposing (..)
import Html.Attributes exposing (style, attribute, class, type_)
import Html.Events
import Keyboard exposing (presses)
import List exposing (member)
import Maze exposing (defaultViewSettings)
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
    = Welcome
    | Moving { nextCell : Maze.Index, alpha : Float }
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
    , lastPowerUp : PowerUp
    , storedPowerUp : Maybe PowerUp
    , currentPowerUp : Maybe PowerUp
    , mazeForm : Form
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
    , state = Welcome
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
    , radius = 1000
    , lastPowerUp = GetPoints
    , storedPowerUp = Nothing
    , currentPowerUp = Nothing
    , mazeForm = group []
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
    | Restart
    | NoOp
    | DismissWelcome


updateRadius : Model -> Float
updateRadius model =
    0.8 * model.radius + 0.2 * model.fov * 15 / model.timeLeft


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
        case msg of
            Restart ->
                { initialModel | state = Choosing 2.0 } ! [ initialCmd ]

            _ ->
                { model | state = GameOver } ! [ Cmd.none ]
    else
        case ( msg, model.state ) of
            ( DismissWelcome, _ ) ->
                { model | state = Choosing 2.0 } ! [ Cmd.none ]

            ( Restart, _ ) ->
                { initialModel | state = Debug.log "restarting" <| Choosing 2.0 } ! [ initialCmd ]

            ( NoOp, _ ) ->
                model ! [ Cmd.none ]

            ( _, GameOver ) ->
                model ! [ Cmd.none ]

            ( TogglePause, Pause savedState ) ->
                { model | state = savedState } ! [ Cmd.none ]

            ( TogglePause, Welcome ) ->
                model ! [ Cmd.none ]

            ( TogglePause, _ ) ->
                { model | state = Pause model.state } ! [ Cmd.none ]

            ( _, Pause _ ) ->
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
                        let
                            newCell =
                                Maze.getModuloIndex model.maze.grid mov.nextCell
                        in
                            { model
                                | state = CheckOrbs
                                , cell = newCell
                                , points = model.points + 1
                                , timeLeft = updateTimeLeft model dt
                                , radius = updateRadius model
                                , currentPowerUp = newCurrentPowerUp
                                , mazeForm = Maze.drawMaze model.maze Nothing ( mazeWidth, mazeHeight ) newCell model.fov model.radius
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
                        , mazeForm = Maze.drawMaze model.maze (Just model.newMaze) ( mazeWidth, mazeHeight ) model.cell model.fov model.radius
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
                        , mazeForm = Maze.drawMaze model.maze Nothing ( mazeWidth, mazeHeight ) model.cell model.fov (model.radius + 30)
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
                        , mazeForm = Maze.drawMaze model.newMaze Nothing ( mazeWidth, mazeHeight ) model.cell model.fov model.radius
                    }
                        ! [ newMaze ]
                else
                    { model
                        | state = NewMaze <| alpha - (inSeconds dt)
                        , radius = updateRadius model
                    }
                        ! [ Cmd.none ]

            ( Step _, Welcome ) ->
                { model | radius = updateRadius model } ! [ Cmd.none ]

            ( GenMaze m, _ ) ->
                { model | newMaze = m } ! [ Cmd.none ]

            ( GenMazeStart m, _ ) ->
                { model
                    | maze = m
                    , mazeForm = Maze.drawMaze m Nothing ( mazeWidth, mazeHeight ) model.cell model.fov (model.fov / 10)
                }
                    ! [ newMaze ]

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
                        Random.step
                            ([ BreakWall, SpeedUp 3, GetPoints ]
                                |> List.filter ((/=) model.lastPowerUp)
                                |> Random.List.choose
                            )
                            seed
                in
                    { model | storedPowerUp = powerUp, lastPowerUp = Maybe.withDefault GetPoints powerUp } ! [ Cmd.none ]

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

        wOffset =
            max 10 <| (w - min w h) / 2

        ( mx, my ) =
            model.mousePos

        ( x, y ) =
            ( mx - w / 2
            , h / 2 - my
            )

        settings_ =
            { defaultViewSettings
                | radius = model.radius
                , cellFrom = model.cell
                , alpha = 0
            }

        movingSettings mov =
            { settings_
                | cellTo = Just mov.nextCell
                , alpha = Ease.inOutQuad mov.alpha
            }

        settings =
            case model.state of
                Moving mov ->
                    movingSettings mov

                Pause (Moving mov) ->
                    movingSettings mov

                _ ->
                    settings_

        -- adjusting for scaling
        func color colorT orb =
            orb
                |> Maze.getRelativeCellPos ( mazeWidth, mazeHeight ) settings.radius pos
                |> drawOrbs model.radius model.fov model.timeLeft color colorT

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
                , ( "font-family", "helvetica, arial, sans-serif" )
                ]
            ]
            ([ div
                [ style
                    [ ( "top", "0" )
                    , ( "left", "0" )
                    , ( "z-index", "1" )
                    , ( "position", "absolute" )
                    ]
                , Touch.onSingleTouch Touch.TouchStart Touch.preventAndStop (Touch << .touch)
                , Touch.onSingleTouch Touch.TouchMove Touch.preventAndStop (Touch << .touch)
                ]
                [ toHtml <|
                    color backgroundColor <|
                        collage (floor w) (floor h) <|
                            [ Maze.draw
                                settings
                                model.mazeForm
                            , player
                                |> filled
                                    (interpolateColor True shuffleColor wallColor <| Ease.inOutSine model.speedBonus)
                                |> scale (model.radius / 2)
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
                                            dt =
                                                1 - alpha / 0.7
                                        in
                                            [ "+25"
                                                |> fromString
                                                |> typefaced
                                                |> Text.height 30
                                                |> Text.color charcoal
                                                |> Collage.text
                                                |> Collage.move ( model.fov * dt, model.fov * dt ^ 4 )
                                            ]

                                    _ ->
                                        []
                ]
             , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "z-index", "2" )
                    , ( "top", "10px" )
                    , ( "right", toString wOffset ++ "px" )
                    , ( "font-size", "50px" )
                    , ( "color", "rgb(85,87,83)" )
                    ]
                ]
                [ Html.text <| toString model.points
                ]
             , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "z-index", "2" )
                    , ( "bottom", "10px" )
                    , ( "right", toString wOffset ++ "px" )
                    ]
                , Html.Events.onClick UsePowerUp
                ]
                (case model.storedPowerUp of
                    Nothing ->
                        []

                    Just powerUp ->
                        [ powerUpHtml powerUp ]
                )
             , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "10px" )
                    , ( "left", toString wOffset ++ "px" )
                    , ( "z-index", "2" )
                    ]
                , Html.Events.onClick TogglePause
                ]
                [ case model.state of
                    Pause _ ->
                        FontAwesome.play_circle wallColor 50

                    _ ->
                        FontAwesome.pause_circle shuffleColor 50
                ]
             , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "10px" )
                    , ( "left", toString (60 + wOffset) ++ "px" )
                    , ( "z-index", "2" )
                    ]
                , Html.Events.onClick Restart
                ]
                [ FontAwesome.refresh timeColor 50
                ]
             ]
                ++ case model.state of
                    Welcome ->
                        [ div
                            [ style
                                [ ( "position", "absolute" )
                                , ( "top", "70px" )
                                , ( "width", toString w ++ "px" )
                                , ( "z-index", "3" )
                                ]
                            ]
                            [ div
                                [ style
                                    [ ( "position", "relative" )
                                    , ( "margin", "0px auto" )
                                    , ( "width", "30%" )
                                    , ( "min-width", "400px" )
                                    , ( "padding", "10px" )
                                    , ( "border-radius", "10px" )
                                    , ( "background-color", "rgba(50,50,50,0.6)" )
                                    , ( "color", "white" )
                                    ]
                                , Html.Events.onClick DismissWelcome
                                ]
                                [ h1 [ style [ ( "text-align", "center" ), ( "width", "100%" ) ] ] [ Html.text "Haze" ]
                                , hr [ style [ ( "width", "200px" ), ( "margin", "0px auto 20px auto" ), ( "color", "white" ) ] ] []
                                , h2 [] [ Html.text "How to play:" ]
                                , ul
                                    [ style [ ( "margin", "0px 0px 1em 0px" ) ] ]
                                    [ li [] [ Html.text "Choose direction with your mouse/touch" ]
                                    , li [] [ Html.text "Increase your speed by chaining valid moves" ]
                                    , li
                                        []
                                        [ Html.text "Collect the orbs:"
                                        , ul
                                            [ type_ "none" ]
                                            (let
                                                orbElement c t =
                                                    li
                                                        [ style [ ( "text-align", "left" ) ] ]
                                                        [ div [ style [ ( "display", "inline" ) ] ] [ FontAwesome.circle c 20 ]
                                                        , div [ style [ ( "display", "inline" ), ( "height", "20px" ), ( "margin", "auto 10px" ) ] ] [ Html.text t ]
                                                        ]
                                             in
                                                [ orbElement timeColor "More time"
                                                , orbElement shuffleColor "Shuffle the maze"
                                                , orbElement powerUpColor "Power ups"
                                                ]
                                            )
                                        ]
                                    , li
                                        []
                                        [ Html.text "Click anywhere or tap on the icon to use the power ups:"
                                        , ul
                                            [ type_ "none" ]
                                            (let
                                                orbElement f t =
                                                    li
                                                        [ style [ ( "text-align", "left" ) ] ]
                                                        [ div [ style [ ( "display", "inline" ) ] ] [ f powerUpColor 20 ]
                                                        , div [ style [ ( "display", "inline" ), ( "height", "20px" ), ( "margin", "auto 10px" ) ] ] [ Html.text t ]
                                                        ]
                                             in
                                                [ orbElement FontAwesome.magic "Go through a wall"
                                                , orbElement FontAwesome.angle_double_up "Speed boost"
                                                , orbElement FontAwesome.trophy "Bonus 50 points"
                                                ]
                                            )
                                        ]
                                    , li [] [ Html.text "Get as many points as possible!" ]
                                    ]
                                , Html.h2 [ style [ ( "text-align", "center" ), ( "width", "100%" ) ] ] [ Html.text "Click/Tap to Start!" ]
                                ]
                            ]
                        ]

                    _ ->
                        []
            )


typefaced : Text -> Text
typefaced =
    typeface [ "helvetica", "arial", "sans-serif" ]


powerUpHtml : PowerUp -> Html Msg
powerUpHtml powerUp =
    case powerUp of
        BreakWall ->
            FontAwesome.magic powerUpColor 50

        SpeedUp _ ->
            FontAwesome.angle_double_up powerUpColor 50

        GetPoints ->
            FontAwesome.trophy powerUpColor 50


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
        , presses
            (\key ->
                if key == toCode ' ' then
                    TogglePause
                else if key == toCode 'r' || key == toCode 'R' then
                    Restart
                else
                    NoOp
            )
        , clicks (always UsePowerUp)
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
            radius * (0.1 + (abs <| sin (t * 0.7)) * max 0 (0.75 - sqrDist / sqrMaxRadius))

        g =
            radial ( 0, 0 ) (0.05 * radius) ( 0, 0 ) r [ ( 0, f ), ( 0.9, b ) ]
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
