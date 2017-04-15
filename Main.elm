module Main exposing (..)

import Html exposing (..)
import Task
import Array exposing (..)
import Position
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
    { board : Maybe Board
    , chars : List (List Char)
    , width : Int
    , height : Int
    , position : Position.Position
    , key : Maybe Char
    , win : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        Nothing
        []
        5
        5
        (Position.Position 0 0)
        Nothing
        False
    , Cmd.none
    )


type alias Cell =
    { position : Position.Position
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
    | Win


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
                    if newPos /= { x = width - 1, y = height - 1 } then
                        ( { model
                            | key = Just code
                            , position = newPos
                            , board = Just <| makeBoard model.chars newPos
                          }
                        , Cmd.none
                        )
                    else
                        ( model, Task.perform (\_ -> Win) (Task.succeed "You win") )

            Win ->
                ( { model | win = True }, Cmd.none )


updatePosition : Position.Position -> Maybe Board -> Char -> Position.Position
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
            in
                Maybe.withDefault p <|
                    List.foldr
                        (\{ position } c ->
                            case c of
                                Just _ ->
                                    c

                                Nothing ->
                                    if Position.adjacent p position then
                                        Just position
                                    else
                                        Nothing
                        )
                        Nothing
                        matchingChars

        Nothing ->
            p


makeBoard : List (List Char) -> Position.Position -> Board
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
                        if Position.Position x y == p then
                            { position =
                                Position.Position x y
                            , state = Selected
                            }
                        else
                            { position =
                                Position.Position x y
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
view { board, key, win } =
    if win then
        div [] [ text "Win!" ]
    else
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
