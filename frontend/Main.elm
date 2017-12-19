module Main exposing (..)

import Html exposing (Html, text, div, span)
import Json.Decode exposing (..)
import Http


-- API interface to Gemma


type alias Task =
    { name : String
    , description : Maybe String
    , remaining : Int
    }


emptyStringFilter s =
    if s == "" then
        Nothing
    else
        Just s


decodeEmptyString : Decoder (Maybe String)
decodeEmptyString =
    map emptyStringFilter string


decodeTask : Decoder Task
decodeTask =
    map3 Task
        (field "name" string)
        (field "description" decodeEmptyString)
        (field "remaining" int)


loadTasks : Cmd Msg
loadTasks =
    let
        request =
            Http.get "http://localhost:4242/tasks" (list decodeTask)
    in
        Http.send NewTasks request



-- Elm architecture implementation


type Msg
    = None
    | LoadTasks
    | NewTasks (Result Http.Error (List Task))


type alias Model =
    { tasks : List Task
    , error : Maybe String
    }


renderTask : Task -> Html Msg
renderTask task =
    div [] [ span [] [ text task.name ] ]


view : Model -> Html Msg
view model =
    div [] (List.map renderTask model.tasks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadTasks ->
            ( model, loadTasks )

        NewTasks (Ok tasks) ->
            ( { tasks = tasks, error = Nothing }, Cmd.none )

        NewTasks (Err err) ->
            ( { model | error = Just (toString err) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { tasks = [], error = Nothing }, loadTasks )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
