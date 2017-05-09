module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Char
import List
import String
import Array exposing (..)
import Random
import Maybe exposing (..)


main : Program Never Model Msg
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
    , word : String
    , splitWord : List LetterVisibility
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


words : Array String
words =
    Array.fromList [ "elephant", "acrobat", "caterpiller", "pillow", "charm" ]


initWord : String -> List LetterVisibility
initWord word =
    String.split "" word |> List.map (\l -> ( l, False ))


wordIndex : Random.Generator
wordIndex =
    Random.generate NewWord (Random.int 0 4)


init : ( Model, Cmd Msg )
init =
    ( Model letters "guess" (initWord "guess") 10 Incomplete (String.length "guess"), Cmd.none )



-- newGame
-- as letters are chosen, use filter to remove
-- Update


type Msg
    = Guess String
    | NewGame
    | NewWord



-- | NextWord (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess letter ->
            case model.gameResult of
                Incomplete ->
                    ( guess letter model, Cmd.none )

                Win ->
                    ( model, Cmd.none )

                Lose ->
                    ( model, Cmd.none )

        NewGame ->
            init

        NewWord ->
            init



--         NextWord (Ok newWord) ->
--             ( Model letters newWord (initWord newWord) 10 Incomplete (String.length newWord), Cmd.none )
--
--         NextWord (Err _) ->
--             ( model, Cmd.none )
--
--
-- getNewWord : Cmd Msg
-- getNewWord =
--     let
--         url =
--             "http://randomword.setgetgo.com/get.php"
--
--         request =
--             Http.getString url
--     in
--         Http.send NextWord request


guess : String -> Model -> Model
guess letter model =
    let
        newLetters =
            filterOutLetter letter model.lettersAvailable

        letterVisibility =
            setLetterVisible letter model.splitWord

        numLettersToWin =
            countLettersLeft letterVisibility

        turnsLeft =
            model.numGuessesLeft - 1

        gameResult =
            if numLettersToWin == 0 then
                Win
            else if turnsLeft <= 0 then
                Lose
            else
                Incomplete
    in
        Model
            newLetters
            "next"
            letterVisibility
            turnsLeft
            gameResult
            numLettersToWin


filterOutLetter : a -> List a -> List a
filterOutLetter letter list =
    List.filter (\l -> l /= letter) list


setLetterVisible : String -> List LetterVisibility -> List LetterVisibility
setLetterVisible letter list =
    List.map
        (\( l, vis ) ->
            if l == letter then
                ( l, True )
            else
                ( l, vis )
        )
        list


countLettersLeft : List LetterVisibility -> Int
countLettersLeft word =
    List.filter (\( l, v ) -> not v) word |> List.length



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ gameProgress model
        , wordView model.splitWord
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
                    ( "Womp womp womp, you lose. The word was \""
                        ++ model.word
                        ++ "\""
                    , ""
                    )

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
