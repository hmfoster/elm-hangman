module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Char
import List
import String


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type GameResult
    = Win
    | Lose
    | Incomplete


type alias LetterVisibility =
    ( String, Bool )


type alias Model =
    { lettersAvailable : List String
    , word : List LetterVisibility
    , numGuessesLeft : Int
    , gameResult : GameResult
    , numLettersToWin : Int
    }


letters : List String
letters =
    List.range 97 122
        |> List.map
            (\c ->
                Char.fromCode c |> String.fromChar
            )


initWord : String -> List LetterVisibility
initWord word =
    String.split "" word |> List.map (\l -> ( l, False ))


word : List LetterVisibility
word =
    initWord "book"


init : ( Model, Cmd Msg )
init =
    ( Model letters word 10 Incomplete (List.length word), Cmd.none )



-- as letters are chosen, use filter to remove
-- Update


type Msg
    = Guess String
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess letter ->
            let
                newLetters =
                    List.filter (\l -> l /= letter) model.lettersAvailable

                visibleLetters =
                    List.map
                        (\( l, vis ) ->
                            if l == letter then
                                ( l, True )
                            else
                                ( l, vis )
                        )
                        model.word

                numLettersFound =
                    List.filter
                        (\( l, vis ) -> letter == l)
                        model.word
                        |> List.length

                numLettersToWin =
                    model.numLettersToWin - numLettersFound

                turnsLeft =
                    model.numGuessesLeft - 1

                gameResult =
                    if numLettersToWin == 0 then
                        Win
                    else if turnsLeft == 0 then
                        Lose
                    else
                        Incomplete
            in
                ( Model
                    newLetters
                    visibleLetters
                    turnsLeft
                    gameResult
                    numLettersToWin
                , Cmd.none
                )

        NewGame ->
            init



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ gameProgress model
        , wordView model.word
        , lettersView model.lettersAvailable
        , button [ onClick NewGame ] [ text "New Game!" ]
        ]


gameProgress : Model -> Html Msg
gameProgress model =
    let
        result =
            case model.gameResult of
                Win ->
                    ( "Well done, you win!", "" )

                Lose ->
                    ( "Womp womp womp, you lose", "" )

                Incomplete ->
                    ( "", (toString model.numGuessesLeft) ++ " guesses left!" )

        ( progress, guesses ) =
            result
    in
        div []
            [ p [] [ text progress ]
            , p [] [ text guesses ]
            ]


lettersView : List String -> Html Msg
lettersView letters =
    List.map (\l -> span [ onClick (Guess l) ] [ text (l ++ " ") ]) letters |> p []


wordView : List LetterVisibility -> Html Msg
wordView word =
    List.map letterView word |> p []


letterView : LetterVisibility -> Html Msg
letterView ( letter, visibility ) =
    if visibility then
        text letter
    else
        text " _ "
