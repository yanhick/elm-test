module Main exposing (..)

import Html exposing (..)
import Array exposing (..)
import Html.Attributes exposing (style)
import Keyboard exposing (..)
import Char exposing (fromCode)
import Html.Events exposing (..)
import Random exposing (generate, int, Generator, map, list)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Board =
    List (List Cell)


type alias Model =
    { start : Position
    , end : Position
    , board : Maybe Board
    , chars : List (List Char)
    , width : Int
    , height : Int
    , position : Position
    , key : Maybe Char
    }


type alias Position =
    { x : Int, y : Int }


init : ( Model, Cmd Msg )
init =
    ( Model (Position 0 0) Nothing [] 5 5 (Position 0 0) Nothing, Cmd.none )


type alias Cell =
    { position : Position
    , state : CellState
    }


type CellState
    = Empty
    | Letter Char
    | Selected


type Msg
    = Start
    | NewBoard (List (List Char))
    | Presses Char


lowercaseletter : Generator Char
lowercaseletter =
    Random.map (\n -> fromCode (n + 97)) (int 0 25)


boardChars : Int -> Int -> Generator (List (List Char))
boardChars width height =
    list width (list height lowercaseletter)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { width, height } =
            model
    in
        case msg of
            Start ->
                ( model, Random.generate NewBoard <| boardChars width height )

            NewBoard chars ->
                ( { model
                    | board = Just <| makeBoard chars model.position
                    , chars = chars
                  }
                , Cmd.none
                )

            Presses code ->
                let
                    newPos =
                        updatePosition model.position model.board code
                in
                    ( { model
                        | key = Just code
                        , position = newPos
                        , board = Just <| makeBoard model.chars newPos
                      }
                    , Cmd.none
                    )


updatePosition : Position -> Maybe Board -> Char -> Position
updatePosition p b k =
    case b of
        Just b ->
            let
                matchingChar =
                    List.filter
                        (\{ state } ->
                            case state of
                                Empty ->
                                    False

                                Selected ->
                                    False

                                Letter l ->
                                    k == l
                        )

                matchingChars =
                    matchingChar (List.concat b)

                adjacent p1 p2 =
                    (p1.y == p2.y && p1.x + 1 == p2.x)
                        || (p1.y + 1 == p2.y && p1.x + 1 == p2.x)
                        || (p1.y + 1 == p2.y && p1.x == p2.x)
                        || (p1.y + 1 == p2.y && p1.x - 1 == p2.x)
                        || (p1.y == p2.y && p1.x - 1 == p2.x)
                        || (p1.y - 1 == p2.y && p1.x - 1 == p2.x)
                        || (p1.y - 1 == p2.y && p1.x == p2.x)
                        || (p1.y - 1 == p2.y && p1.x + 1 == p2.x)
            in
                Maybe.withDefault p <|
                    List.foldr
                        (\{ position } c ->
                            case c of
                                Just _ ->
                                    c

                                Nothing ->
                                    if adjacent p position then
                                        Just position
                                    else
                                        Nothing
                        )
                        Nothing
                        matchingChars

        Nothing ->
            p


makeBoard : List (List Char) -> Position -> Board
makeBoard ls p =
    let
        idx =
            Array.fromList >> Array.toIndexedList

        xs =
            idx <| List.map idx ls
    in
        List.map
            (\( x, l ) ->
                (List.map
                    (\( y, l ) ->
                        if Position x y == p then
                            { position =
                                Position x y
                            , state = Selected
                            }
                        else
                            { position =
                                Position x y
                            , state = Letter l
                            }
                    )
                    l
                )
            )
            xs


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> Presses (fromCode code))


view : Model -> Html Msg
view { board, key } =
    case board of
        Just board ->
            div []
                (List.map (row key) board)

        Nothing ->
            case key of
                Just key ->
                    div []
                        [ button
                            [ onClick Start ]
                            [ text "Start" ]
                        , text (String.fromChar key)
                        ]

                Nothing ->
                    div []
                        [ button [ onClick Start ] [ text "Start" ] ]


cellStyle : Cell -> Attribute Msg
cellStyle { state } =
    let
        background =
            case state of
                Empty ->
                    "grey"

                Letter _ ->
                    "grey"

                Selected ->
                    "red"
    in
        style
            [ ( "width", "100px" )
            , ( "height", "100px" )
            , ( "display", "inline-block" )
            , ( "margin", "5px" )
            , ( "background", background )
            , ( "white-space", "pre" )
            ]


row : Maybe Char -> List Cell -> Html Msg
row ch ls =
    let
        cellToString { state } =
            case state of
                Empty ->
                    " "

                Letter l ->
                    String.fromChar l

                Selected ->
                    " "
    in
        div
            []
            (List.map
                (\c ->
                    div
                        [ cellStyle c
                        ]
                        [ text <| cellToString c ]
                )
                ls
            )
