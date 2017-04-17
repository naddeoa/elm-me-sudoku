module Main exposing (..)

import Game exposing (Game)
import Html exposing (program, text, Html)


type alias Model =
    Game


update : Game.GameEvent -> Model -> ( Model, Cmd Game.GameEvent )
update msg model =
    case msg of
        Game.Increment symbol ->
            let
                updatedModel =
                    Game.increment model symbol
            in
                update (Game.evaluate updatedModel) updatedModel

        Game.Win ->
            ( model, Cmd.none )

        Game.Wrong ->
            ( model, Cmd.none )


view : Model -> Html Game.GameEvent
view model =
    Html.div []
        [ Game.render model
        , Game.renderResults model
        ]


main : Program Never Game Game.GameEvent
main =
    Html.program
        { init = ( Game.defaultGame, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
