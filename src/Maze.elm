module Maze exposing (..)

import Collage exposing (..)
import Color exposing (Color, red, blue)
import Debug
import List exposing (..)
import List.Extra exposing ((!!), zip3, zip)
import Maybe exposing (andThen)
import Random exposing (Generator, Seed, step)
import Transform exposing (translation, multiply)
import Tuple exposing (..)
import Utils exposing (..)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


type alias Index =
    { x : Int
    , y : Int
    }


type Wall
    = Wall
    | Open


type alias Cell =
    { visited : Bool
    , walls : List Wall
    }


type alias Model =
    { grid : List (List Cell)
    , current : Index
    , stack : List Index
    }


type alias ViewSettings =
    { radius : Float
    , cellFrom : Index
    , cellTo : Maybe Index
    , alpha : Float
    , scale : Float
    , dims : ( Int, Int )
    , transition : Maybe Model
    }



-------------------------------------------------------------------------------
-- Initial values
-------------------------------------------------------------------------------


defaultViewSettings : ViewSettings
defaultViewSettings =
    { radius = 15
    , cellFrom = { x = 0, y = 0 }
    , cellTo = Nothing
    , alpha = 0
    , scale = 2
    , dims = ( 15, 15 )
    , transition = Nothing
    }


init : Int -> Int -> Model
init w h =
    let
        initCell =
            { visited = False
            , walls = repeat 6 Wall
            }
    in
        { grid =
            modifyCell (\c -> { c | visited = True }) { x = 0, y = 0 } <|
                repeat h <|
                    repeat w <|
                        initCell
        , current = { x = 0, y = 0 }
        , stack = []
        }



-------------------------------------------------------------------------------
-- Drawing the maze
-------------------------------------------------------------------------------
-- This only draw half of the hexagon outline to avoid overlapping
-- dotted lines between cells


drawCellOutline : Float -> Form
drawCellOutline radius =
    let
        lineStyle =
            solid outlineColor
    in
        traced
            { lineStyle | width = 0.5 }
            (path <| map (getPos radius) [ 0, 1, 2, 3 ])



-- Draw only walls given a cell and prerendered walls


drawCellWalls : List Form -> Cell -> List Form
drawCellWalls forms cell =
    map second <|
        filter ((==) Wall << first) <|
            zip cell.walls forms


drawTransitionWalls : List ( Form, Form, Form ) -> Cell -> Cell -> List Form
drawTransitionWalls forms a b =
    map
        (\( aw, bw, ( w, a, r ) ) ->
            if aw == bw then
                w
            else if aw == Wall then
                r
            else
                a
        )
    <|
        filter (\( aw, bw, _ ) -> aw == Wall || bw == Wall) <|
            zip3 a.walls b.walls forms



-- Draw all the walls for a given radius


drawWalls : Color -> Float -> List Form
drawWalls color radius =
    map (drawSegment color radius) (range 0 5)



-- Draw a segment of an hexagon given a radius and the segment index


drawSegment : Color -> Float -> Int -> Form
drawSegment color radius segIndex =
    let
        lineStyle =
            solid color
    in
        traced
            { lineStyle | width = 1 }
            (segment
                (getPos radius segIndex)
                (getPos radius <| (segIndex + 1) % 6)
            )



-- Transform to apply to the whole hex grid to center and scale it properly


getTransform : ViewSettings -> Transform.Transform
getTransform settings =
    let
        a =
            getCellPos (settings.radius) settings.cellFrom

        b =
            settings.cellTo
                |> Maybe.map (getCellPos settings.radius)
                |> Maybe.withDefault a

        ( dx, dy ) =
            interpolatePos a b settings.alpha
    in
        multiply
            (translation (-dx) (-dy))
            (Transform.scale settings.scale)


sqrt3 : Float
sqrt3 =
    sqrt 3


drawCell : Index -> Form -> List ( Form, Form, Form ) -> Model -> Maybe Model -> List Form
drawCell index hexOutline hexWalls model transition =
    case ( getCell index model.grid, transition |> andThen (\m -> getCell index m.grid) ) of
        ( Nothing, _ ) ->
            []

        ( Just c, Nothing ) ->
            (::) hexOutline <|
                drawCellWalls (map (\( w, _, _ ) -> w) hexWalls) c

        ( Just a, Just b ) ->
            (::) hexOutline <|
                drawTransitionWalls hexWalls a b



-- Main draw function


draw : ViewSettings -> Model -> Form
draw settings model =
    let
        radius =
            settings.radius / settings.scale

        hexOutline =
            drawCellOutline radius

        hexWalls =
            zip3 (drawWalls wallColor radius)
                (drawWalls shuffleColor radius)
                (drawWalls timeColor radius)

        ( width, height ) =
            settings.dims

        center =
            settings.cellFrom

        ( startX, startY ) =
            ( center.x - width // 2
            , center.y - height // 2
            )

        ( endX, endY ) =
            ( center.x + width // 2
            , center.y + height // 2
            )

        highlightedCell =
            case settings.cellTo of
                Nothing ->
                    group []

                Just c ->
                    ngon 6 radius
                        |> filled hexBackgroundColor
                        |> move (getCellPos radius c)
    in
        groupTransform
            (getTransform settings)
            << (::) highlightedCell
        <|
            map
                (\y ->
                    groupTransform
                        (translation
                            0
                            (sqrt3 * radius * toFloat y)
                        )
                    <|
                        map
                            (\x ->
                                groupTransform
                                    (translation
                                        (1.5 * radius * toFloat x)
                                        (if isEven x then
                                            0
                                         else
                                            sqrt3 / 2 * radius
                                        )
                                    )
                                <|
                                    drawCell
                                        { x = x, y = y }
                                        hexOutline
                                        hexWalls
                                        model
                                        settings.transition
                            )
                            (range startX endX)
                )
                (range startY endY)



-------------------------------------------------------------------------------
-- Hex grid utils
-------------------------------------------------------------------------------
-- Get the position of a cell in the grid


getCellPos : Float -> Index -> Pos
getCellPos radius index =
    let
        newX =
            1.5 * radius * toFloat index.x

        newY =
            sqrt3 * radius * toFloat index.y
    in
        if isEven index.x then
            ( newX, newY )
        else
            ( newX, newY + sqrt3 / 2 * radius )


getRelativeCellPos : ( Int, Int ) -> Float -> Pos -> Index -> Pos
getRelativeCellPos ( w_, h_ ) radius ( x, y ) index =
    let
        ( w, h ) =
            ( 1.5 * radius * toFloat w_, sqrt3 * radius * toFloat h_ )

        ( cx, cy ) =
            getCellPos radius index

        sqrtDist ( a, b ) ( c, d ) =
            let
                x =
                    a - c

                y =
                    b - d
            in
                x * x + y * y

        candidates =
            [ ( x, y )
            , ( x + w, y )
            , ( x - w, y )
            , ( x + w, y + h )
            , ( x + w, y - h )
            , ( x - w, y + h )
            , ( x - w, y - h )
            , ( x, y + h )
            , ( x, y - h )
            ]

        ( x_, y_ ) =
            candidates
                |> sortBy (sqrtDist ( cx, cy ))
                |> head
                |> Maybe.withDefault ( x, y )
    in
        ( cx - x_, cy - y_ )



-- Get the position of the corner of an hexagon given its radius and corner index


getPos : Float -> Int -> Pos
getPos radius index =
    let
        angle =
            pi / 3 * toFloat index
    in
        ( radius * cos angle, radius * sin angle )


getModuloIndex : List (List Cell) -> Index -> Index
getModuloIndex grid i =
    case grid of
        [] ->
            Index 0 0

        -- technically wrong but will be caught later on
        [] :: _ ->
            Index 0 0

        -- technically wrong but will be caught later on
        row :: _ ->
            Index (i.x % length row) (i.y % length grid)



-- Get a cell from the grid if it exists


getCell : Index -> List (List Cell) -> Maybe Cell
getCell i grid =
    let
        { x, y } =
            getModuloIndex grid i
    in
        (grid !! y) |> andThen (\r -> r !! x)



-- Modify a cell in the grid at a given index


modifyCell : (Cell -> Cell) -> Index -> List (List Cell) -> List (List Cell)
modifyCell f i grid =
    let
        { x, y } =
            getModuloIndex grid i

        maybeGrid =
            Maybe.map2
                (\c r ->
                    take y grid
                        ++ [ take x r ++ [ f c ] ++ drop (x + 1) r ]
                        ++ drop (y + 1) grid
                )
                (getCell i grid)
                (grid !! y)
    in
        case maybeGrid of
            Nothing ->
                grid

            Just g ->
                g



-- Get the neighbour of the given index using the shared segment index


getNeighbour : Index -> Int -> Index
getNeighbour index i =
    case ( i, isEven index.x ) of
        ( 0, True ) ->
            { index | x = index.x + 1 }

        ( 0, False ) ->
            { x = index.x + 1, y = index.y + 1 }

        ( 1, _ ) ->
            { index | y = index.y + 1 }

        ( 2, True ) ->
            { index | x = index.x - 1 }

        ( 2, False ) ->
            { x = index.x - 1, y = index.y + 1 }

        ( 3, True ) ->
            { x = index.x - 1, y = index.y - 1 }

        ( 3, False ) ->
            { index | x = index.x - 1 }

        ( 4, _ ) ->
            { index | y = index.y - 1 }

        ( _, True ) ->
            { x = index.x + 1, y = index.y - 1 }

        ( _, False ) ->
            { index | x = index.x + 1 }



-- Get all the neighbouring cell indices (even the ones outside the grid)


getNeighbours : Index -> List ( Int, Index )
getNeighbours index =
    map
        (\i -> ( i, getNeighbour index i ))
        (range 0 5)



-- All the possible directions accessible from a given index on the maze


validDirections : Model -> Index -> List Int
validDirections model index =
    filterMap
        (\i ->
            (getCell index model.grid)
                |> andThen (\c -> c.walls !! i)
                |> andThen
                    (\w ->
                        if w == Open then
                            Just i
                        else
                            Nothing
                    )
        )
        (range 0 5)



-------------------------------------------------------------------------------
-- Generate the maze
-------------------------------------------------------------------------------
-- Take into account weither the neighbours are in the grid and
-- if they have been visited before


availableNeighbours : Model -> List ( Int, Index )
availableNeighbours model =
    filterMap
        (\( i, index ) ->
            (getCell index model.grid)
                |> andThen
                    (\c ->
                        if c.visited then
                            Nothing
                        else
                            Just ( i, index )
                    )
        )
        (getNeighbours model.current)



-- With a random see pick the next cell to visit if possible


pickNext : Seed -> Model -> ( Maybe ( Int, Index ), Seed )
pickNext seed model =
    let
        ns =
            availableNeighbours model

        ( i, newSeed ) =
            step (Random.int 0 (length ns - 1)) seed
    in
        if isEmpty ns then
            ( Nothing, seed )
        else
            ( ns !! i, newSeed )



-- Remove the wall indicated by the given hex segment index


removeWall : Int -> Cell -> Cell
removeWall x cell =
    { cell
        | walls =
            take x cell.walls
                ++ [ Open ]
                ++ drop (x + 1) cell.walls
    }



-- Given a random seed, generate a maze using a simple recursive backtracker algorithm


generate : Seed -> Model -> Model
generate seed model =
    let
        ( n, newSeed ) =
            pickNext seed model

        anyLeft =
            any (any .visited) model.grid
    in
        case ( anyLeft, n, head model.stack ) of
            ( False, _, _ ) ->
                model

            ( _, Nothing, Nothing ) ->
                model

            ( _, Nothing, Just i ) ->
                generate newSeed <|
                    { model
                        | current = i
                        , stack =
                            model.stack
                                |> tail
                                |> Maybe.withDefault []
                    }

            ( _, Just ( x, i ), _ ) ->
                generate newSeed <|
                    { grid =
                        model.grid
                            |> (modifyCell (removeWall x) model.current)
                            |> (modifyCell (removeWall ((x + 3) % 6)) i)
                            |> (modifyCell (\c -> { c | visited = True }) i)
                    , current = i
                    , stack = model.current :: model.stack
                    }
