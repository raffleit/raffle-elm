module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


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
    div []
        [ div []
              [ input [ type_ "text", value model.participantsForm.name, placeholder "Name", onInput FormNameChange ] []
              , input [ type_ "number", value (toString model.participantsForm.numberOfTickets), placeholder "# tickets", onInput FormNumberOfTicketsChange ] []
              , input [ type_ "submit",  onClick AddParticipant ] []
              ]
        , div [] [ table [] [
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



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
