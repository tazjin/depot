module Main exposing (..)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (style)
import Json.Decode exposing (..)
import Http


--  Material design imports

import Material
import Material.Card as Card
import Material.Color as Color
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Scheme as Scheme


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
    | Mdl (Material.Msg Msg)


type alias Model =
    { tasks : List Task
    , error : Maybe String
    , mdl : Material.Model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadTasks ->
            ( model, loadTasks )

        NewTasks (Ok tasks) ->
            ( { model | tasks = tasks, error = Nothing }, Cmd.none )

        NewTasks (Err err) ->
            ( { model | error = Just (toString err) }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- View implementation


white =
    Color.text Color.white


taskColor : Task -> Color.Hue
taskColor task =
    if task.remaining > 2 then
        Color.Green
    else if task.remaining < 0 then
        Color.Red
    else
        Color.Yellow


renderTask : Task -> Html Msg
renderTask task =
    Card.view
        [ Color.background (Color.color (taskColor task) Color.S800) ]
        [ Card.title [] [ Card.head [ white ] [ text task.name ] ] ]



--    div [] [ span [] [  ] ]


gemmaView : Model -> Html Msg
gemmaView model =
    grid []
        (List.map (\t -> cell [ size All 3 ] [ renderTask t ])
            model.tasks
        )


view : Model -> Html Msg
view model =
    gemmaView model |> Scheme.top



-- div [ style [ ( "padding", "2rem" ) ] ]
--
--     |> Scheme.top


main : Program Never Model Msg
main =
    let
        model =
            { tasks = []
            , error = Nothing
            , mdl = Material.model
            }
    in
        Html.program
            { init = ( model, Cmd.batch [ loadTasks, Material.init Mdl ] )
            , view = view
            , update = update
            , subscriptions = Material.subscriptions Mdl
            }
