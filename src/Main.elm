module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)


---- MODEL ----

type alias Participant =
    {
        name: String,
        numberOfTickets: Int
    }

type alias Model =
    {
        participants: List (Participant),
        participantsForm: Participant
    }


init : ( Model, Cmd Msg )
init =
    (
    {
        participants = [{name="hei", numberOfTickets=12}],
        participantsForm = {name = "", numberOfTickets = 0}}
    , Cmd.none
    )



---- UPDATE ----

type Msg
    = AddParticipant |
    FormNameChange String |
    FormNumberOfTicketsChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddParticipant ->
            { model | participants = model.participantsForm :: model.participants, participantsForm = {name= "", numberOfTickets = 0} } ! [ Cmd.none ]

        FormNameChange name ->
            let
                oldParticipantsForm = model.participantsForm
                newParticipantsForm =
                    { oldParticipantsForm | name = name }
            in
                {model | participantsForm = newParticipantsForm} ! [ Cmd.none ]

        FormNumberOfTicketsChange numberOfTickets ->
            let
                oldParticipantsForm = model.participantsForm
                newNumberOfTickets = Result.withDefault 0 (String.toInt numberOfTickets)
                newParticipantsForm =
                    { oldParticipantsForm | numberOfTickets = newNumberOfTickets }
            in
                {model | participantsForm = newParticipantsForm} ! [ Cmd.none ]


---- VIEW ----

viewParticipants : Participant -> Html Msg
viewParticipants participant =
    tr []
        [ td [] [text participant.name]
        , td [] [text (toString participant.numberOfTickets) ]
        ]

view : Model -> Html Msg
view model =
    div [classList
            [("container", True)]
        ]
        [ div [classList
                [("row", True)]
        ] [
            div [classList
                   [("col-md-5", True)]
            ] [
                 div [classList
                        [("row", True)]
                 ] [ Html.form [ onSubmit AddParticipant]
                        [ div [classList
                            [("form-group", True), ("col-md-6", True)]
                        ] [ input [ type_ "text", value model.participantsForm.name, placeholder "Name", onInput FormNameChange, classList [("form-control", True)] ] [] ]
                        , div [classList
                                [("form-group", True), ("col-md-4", True)]
                        ] [ input [ type_ "number", value (toString model.participantsForm.numberOfTickets), placeholder "# tickets", onInput FormNumberOfTicketsChange, classList [("form-control", True)] ] []]
                        , div [classList
                                [("col-md-2", True)]
                        ] [ input [ type_ "submit",  value "+", classList [("btn", True), ("btn-primary", True)] ] []]
                      ]
                  ]
                , div [] [ table [ classList [("table", True)] ] [
                                thead [] [
                                    tr [] [
                                        td [] [text "Name"]
                                        , td [] [text "# tickets"]
                                    ]
                                ]
                                , tbody [] (List.map viewParticipants model.participants)
                            ]
                 ]
             ]
        ]
    ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
