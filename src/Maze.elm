module Maze exposing (..)

import Array exposing (Array, get)
import Array.Extra exposing (update)
import Collage exposing (..)
import Color exposing (Color, blue, red)
import Debug
import List exposing (..)
import List.Extra exposing ((!!), zip, zip3)
import Maybe exposing (andThen)
import Random exposing (Seed, step)
import Random.List
import Transform exposing (multiply, translation)
import Tuple exposing (..)
import Utils exposing (..)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


type alias Grid =
    Array (Array Cell)


type alias Dim =
    ( Int, Int )


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
    { grid : Grid
    , current : Index
    , stack : List Index
    , dim : Dim
    }


type alias ViewSettings =
    { radius : Float
    , cell : Index
    , dir : Maybe Int
    , alpha : Float
    }



-------------------------------------------------------------------------------
-- Initial values
-------------------------------------------------------------------------------


defaultViewSettings : ViewSettings
defaultViewSettings =
    { radius = 1000
    , cell = { x = 0, y = 0 }
    , dir = Nothing
    , alpha = 0
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
            modifyCell (\c -> { c | visited = True }) { x = 0, y = 0 } ( w, h ) <|
                Array.repeat h <|
                    Array.repeat w <|
                        initCell
        , current = { x = 0, y = 0 }
        , stack = []
        , dim = ( w, h )
        }


renderRadius : Float
renderRadius =
    500.0



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
            { lineStyle | width = 5 }
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
            { lineStyle | width = 20 }
            (segment
                (getPos radius segIndex)
                (getPos radius <| (segIndex + 1) % 6)
            )



-- Transform to apply to the whole hex grid to center and scale it properly


getTransform : ViewSettings -> Transform.Transform
getTransform settings =
    let
        a =
            getCellPos settings.radius settings.cell

        b =
            settings.dir
                |> Maybe.map (getCellPos settings.radius << getNeighbour settings.cell)
                |> Maybe.withDefault a

        ( dx, dy ) =
            interpolatePos a b settings.alpha
    in
        multiply
            (translation (-dx) (-dy))
            (Transform.scale (settings.radius / renderRadius))


sqrt3 : Float
sqrt3 =
    sqrt 3


drawCell : Index -> Form -> List ( Form, Form, Form ) -> Model -> Maybe Model -> List Form
drawCell index hexOutline hexWalls model transition =
    case ( getCell index model.dim model.grid, transition |> andThen (\m -> getCell index m.dim m.grid) ) of
        ( Nothing, _ ) ->
            []

        ( Just c, Nothing ) ->
            hexOutline :: drawCellWalls (map (\( w, _, _ ) -> w) hexWalls) c

        ( Just a, Just b ) ->
            hexOutline :: drawTransitionWalls hexWalls a b



-- Main draw function


draw : ViewSettings -> Form -> Form
draw settings mazeForm =
    let
        highlightedCell =
            case settings.dir of
                Nothing ->
                    group []

                Just d ->
                    ngon 6 renderRadius
                        |> filled hexBackgroundColor
                        |> move (getCellPos renderRadius <| getNeighbour settings.cell d)
    in
        groupTransform
            (getTransform settings)
            [ highlightedCell, mazeForm ]


drawMaze : Model -> Maybe Model -> ( Int, Int ) -> Index -> Float -> Float -> Form
drawMaze model transition ( mazeWidth, mazeHeight ) curentCell fov radius =
    let
        n =
            ceiling (fov / radius)

        ( fromX, toX, fromY, toY ) =
            ( curentCell.x - n
            , curentCell.x + n
            , curentCell.y - n
            , curentCell.y + n
            )

        hexOutline =
            drawCellOutline renderRadius

        hexWalls =
            zip3
                (drawWalls wallColor renderRadius)
                (drawWalls shuffleColor renderRadius)
                (drawWalls timeColor renderRadius)

        maze =
            map
                (\y ->
                    groupTransform
                        (translation
                            0
                            (sqrt3 * renderRadius * toFloat y)
                        )
                    <|
                        map
                            (\x ->
                                groupTransform
                                    (translation
                                        (1.5 * renderRadius * toFloat x)
                                        (if isEven x then
                                            0
                                         else
                                            sqrt3 / 2 * renderRadius
                                        )
                                    )
                                <|
                                    drawCell
                                        { x = x % mazeWidth, y = y % mazeHeight }
                                        hexOutline
                                        hexWalls
                                        model
                                        transition
                            )
                            (range fromX toX)
                )
                (range fromY toY)
    in
        group maze



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


getRelativeCellPos : Dim -> Float -> Pos -> Index -> Pos
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


getModuloIndex : Dim -> Index -> Index
getModuloIndex ( w, h ) i =
    Index (i.x % w) (i.y % h)



-- Get a cell from the grid if it exists


getCell : Index -> Dim -> Grid -> Maybe Cell
getCell i dim grid =
    let
        { x, y } =
            getModuloIndex dim i
    in
        (get y grid) |> andThen (\r -> get x r)



-- Modify a cell in the grid at a given index


modifyCell : (Cell -> Cell) -> Index -> Dim -> Grid -> Grid
modifyCell f i dim grid =
    let
        { x, y } =
            getModuloIndex dim i
    in
        update y (update x f) grid



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
            (getCell index model.dim model.grid)
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
            (getCell index model.dim model.grid)
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
    model
        |> availableNeighbours
        |> Random.List.choose
        |> flip step seed
        |> Tuple.mapFirst Tuple.first



-- Remove the wall indicated by the given hex segment index


removeWall : Int -> Cell -> Cell
removeWall x cell =
    { cell
        | walls =
            take x cell.walls
                ++ [ Open ]
                ++ drop (x + 1) cell.walls
    }


arrayAny : (a -> Bool) -> Array a -> Bool
arrayAny f =
    Array.foldl ((||) << f) False



-- Given a random seed, generate a maze using a simple recursive backtracker algorithm


generate : Seed -> Model -> Model
generate seed model =
    let
        ( n, newSeed ) =
            pickNext seed model

        anyLeft =
            arrayAny (arrayAny .visited) model.grid
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
                    { model
                        | grid =
                            model.grid
                                |> (modifyCell (removeWall x) model.current model.dim)
                                |> (modifyCell (removeWall ((x + 3) % 6)) i model.dim)
                                |> (modifyCell (\c -> { c | visited = True }) i model.dim)
                        , current = i
                        , stack = model.current :: model.stack
                    }
