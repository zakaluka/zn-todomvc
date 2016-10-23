port module ZnTodoMVC exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.
This application is broken up into three key parts:
  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Date
import Task
import Date.Extra.Format exposing (utcIsoString)


main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }



-- MODEL
-- The full application state of our To-do application.


{-| Pulls together underlying data AND UI models
-}
type alias Model =
    { appModel : AppModel
    , createTaskModel : CreateTaskModel
    }


{-| Underlying data model
-}
type alias AppModel =
    { entries : List EntryModel
    , uuid : Int
    , visibility : String
    }


{-| Type of date for task
-}
type TaskDateType
    = Creation
    | Completion


{-| Ways to sort tasks for display
-}
type TaskSort
    = TaskCreationSort
    | TaskCompletionSort
    | TaskSubjectSort


{-| Individual tasks
-}
type alias EntryModel =
    { subject : String
    , body : String
    , createdOn : String
    , completedOn : String
    , editing : Bool
    , id : Int
    }


{-| For UI only
-}
type alias CreateTaskModel =
    { subject : String
    , body : String
    , hide : Bool
    }


{-| Initial model for application
-}
appModel : AppModel
appModel =
    { entries = []
    , visibility = "All"
    , uuid = 0
    }


{-| Initial model for Create Task Card
-}
createTaskModel : CreateTaskModel
createTaskModel =
    { subject = ""
    , body = ""
    , hide = True
    }


{-| Initial model for application initialization
-}
emptyModel : Model
emptyModel =
    { appModel = appModel
    , createTaskModel = createTaskModel
    }


{-| Convenience function to create a new task entry
-}
newEntry : String -> String -> Int -> EntryModel
newEntry sub desc id =
    { subject = sub
    , body = desc
    , createdOn = ""
    , completedOn = ""
    , editing = False
    , id = id
    }


{-| Load browser data, if avaialble
-}
init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


{-| Go out and get latest Date/Time and generate Msg to set the date
on the appropriate task.
-}
setTaskDate : Int -> TaskDateType -> Cmd Msg
setTaskDate id dm =
    Task.perform
        (always
            (SetDate
                id
                dm
                (Date.fromString "2015-01-01 00:00:00.000Z"
                    |> Result.map utcIsoString
                    |> Result.withDefault ""
                )
            )
        )
        (utcIsoString >> SetDate id dm)
        Date.now


{-| Messages types:
Add - Add a new task to the list, start Task to set creation date
CTHideDialog - To hide or show the Create Task UI
CTUpdateBody - To track text in Create Task UI
CTUpdateSubject - To track text in Create Task UI
NoOp - Don't take any action at all (return the same model)
Reset - Remove all tasks
SetDate - To update either creation or completion date of task
-}
type Msg
    = Add
    | CTHideDialog
    | CTUpdateBody String
    | CTUpdateSubject String
    | NoOp
    | Reset
    | SetDate Int TaskDateType String
    | SortTasks TaskSort


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            emptyModel ! []

        NoOp ->
            model ! []

        Add ->
            let
                am =
                    model.appModel

                ctm =
                    model.createTaskModel
            in
                let
                    am2 =
                        { am
                            | uuid = am.uuid + 1
                            , entries =
                                am.entries
                                    ++ [ newEntry
                                            ctm.subject
                                            ctm.body
                                            am.uuid
                                       ]
                        }

                    ctm2 =
                        { ctm | subject = "", body = "", hide = True }
                in
                    { model
                        | appModel = am2
                        , createTaskModel = ctm2
                    }
                        ! [ setTaskDate am.uuid Creation ]

        CTHideDialog ->
            let
                ctm =
                    model.createTaskModel

                ctm2 =
                    { ctm
                        | hide = not ctm.hide
                        , subject = ""
                        , body = ""
                    }
            in
                { model | createTaskModel = ctm2 } ! []

        SetDate iden dateType date ->
            let
                am =
                    model.appModel

                upd entry =
                    if entry.id == iden then
                        case dateType of
                            Creation ->
                                { entry
                                    | createdOn = date
                                }

                            Completion ->
                                { entry
                                    | completedOn = date
                                }
                    else
                        entry

                am2 =
                    { am | entries = List.map upd am.entries }
            in
                { model
                    | appModel = am2
                }
                    ! []

        CTUpdateBody bd ->
            let
                ctm =
                    model.createTaskModel

                ctm2 =
                    { ctm | body = bd }
            in
                { model | createTaskModel = ctm2 } ! []

        CTUpdateSubject sb ->
            let
                ctm =
                    model.createTaskModel

                ctm2 =
                    { ctm | subject = sb }
            in
                { model | createTaskModel = ctm2 } ! []

        SortTasks ts ->
            Debug.crash "SortTasks"



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "g--12 no-pad no-margin-vertical" ]
        [ lazy viewHeader model
        , lazy2 div
            [ class "nudge--left g--12 no-pad no-margin-vertical" ]
            ([ createButton ]
                ++ sortButtons
                ++ [ resetButton
                   , createTaskForm model
                   , viewTaskSummaries model
                   ]
            )
        , lazy viewFooter model
        ]


{-| Page header
-}
viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "container" ]
        [ h1 [ class "nudge--left g--12" ] [ text "todos" ] ]


{-| Page footer
-}
viewFooter : Model -> Html Msg
viewFooter model =
    footer [ class "container" ]
        [ div [ class "nudge--left" ]
            [ span [] [ text "Some stuff" ] ]
        ]


{-| Button to invoke task creation
-}
createButton : Html Msg
createButton =
    button
        [ class "btn--raised nudge--right", onClick CTHideDialog ]
        [ text "What do you need to do today?" ]


sortButtons : List (Html Msg)
sortButtons =
    [ button [ class "btn--raised", onClick (SortTasks TaskSubjectSort) ]
        [ text "Subject" ]
    , button [ class "btn--raised", onClick (SortTasks TaskCreationSort) ]
        [ text "Created On" ]
    , button
        [ class "btn--raised nudge--right"
        , onClick (SortTasks TaskCompletionSort)
        ]
        [ text "Completed On" ]
    ]


{-| Reset all tasks
-}
resetButton : Html Msg
resetButton =
    button
        [ class "btn--raised btn--red nudge--right", onClick Reset ]
        [ text "Reset" ]


{-| Form to create new tasks
-}
createTaskForm : Model -> Html Msg
createTaskForm model =
    div
        [ class "card g--3 g-m--4 g-s--6 g-t--12 no-margin fade-in-from-top"
        , hidden model.createTaskModel.hide
        ]
        [ div [ class "g--12 no-margin-vertical" ]
            [ h3 []
                [ text "Create a new task" ]
            , input
                [ placeholder "Subject"
                , autofocus True
                , type' "text"
                , required True
                , value model.createTaskModel.subject
                , onInput CTUpdateSubject
                , class "g--12 no-margin-vertical"
                ]
                []
            , textarea
                [ required True
                , placeholder "Detailed description"
                , required True
                , wrap "hard"
                , value model.createTaskModel.body
                , onInput CTUpdateBody
                , class "g--12 m--0 no-margin-vertical"
                , rows 5
                ]
                []
            , button [ class "btn--raised", onClick Add ]
                [ text "Create task" ]
            , span [] [ text " " ]
            , button [ class "btn--raised", onClick CTHideDialog ]
                [ text "Cancel" ]
            ]
        ]


{-| View all tasks
-}
viewTaskSummaries : Model -> Html Msg
viewTaskSummaries mdl =
    div [ class "g--12 container container--wrap" ]
        (mdl.appModel.entries
            |> List.sortWith
                (\x ->
                    \y ->
                        if x.completedOn == "" && y.completedOn == "" then
                            Basics.compare x.createdOn y.createdOn
                        else
                            Basics.compare x.completedOn y.completedOn
                )
            |> List.map viewTaskSummary
        )


{-| Generates views of an individual task
-}
viewTaskSummary : EntryModel -> Html Msg
viewTaskSummary entry =
    div
        [ if entry.completedOn == "" then
            class "g--3 g-m--4 g-s--6 g-t--12 tile"
          else
            class
                ("g--3 g-m--4 g-s--6 g-t--12 tile "
                    ++ "bg--purple color--white"
                )
        , style [ ( "padding", "5px" ), ( "margin", "5px" ) ]
        ]
        [ h3 [] [ text entry.subject ]
        , p [] [ text entry.createdOn ]
        , p [] [ text entry.completedOn ]
        ]



-- BROWSER STORAGE


{-| To get and set storage into the browser
-}
port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )
