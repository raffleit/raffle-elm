module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import List exposing (filter)
import Random exposing (int, generate)
import Navigation

---- MODEL ----


type alias Participant =
    {
        id: Int,
        name: String,
        numberOfTickets: Int
    }

type alias ParticipantsForm =
    {
        name: Maybe String,
        numberOfTickets: Maybe String
    }

type Page = Participants | Drawing

getPage : String -> Page
getPage hash =
    case hash of
        "#/Participants" ->
            Participants

        "#/Drawing" ->
            Drawing

        _ ->
            Participants

type alias Model =
    {
        participants: List (Participant),
        participantsForm: ParticipantsForm,
        page : Page
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init = (\_ ->
    ({
        page = Participants,
        participants = [],
        participantsForm = {name = Nothing, numberOfTickets = Nothing}}
    , Cmd.none))



---- UPDATE ----

type Msg
    = AddParticipant |
    AddParticipantWithId Int|
    FormNameChange String |
    FormNumberOfTicketsChange String |
    DeleteParticipant Int |
    UrlChange Navigation.Location

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddParticipant ->
            (model, generate AddParticipantWithId (int 1000000 999999999))

        AddParticipantWithId id ->
            let
                name = Maybe.withDefault "" model.participantsForm.name
                numberOfTickets = (String.toInt (Maybe.withDefault "0" model.participantsForm.numberOfTickets)) |> Result.toMaybe |> Maybe.withDefault 0
            in
                { model | participants = { id = id, name = name, numberOfTickets = numberOfTickets} :: model.participants,
                        participantsForm = {name= Nothing, numberOfTickets = Nothing} } ! [ Cmd.none ]

        FormNameChange name ->
            let
                oldParticipantsForm = model.participantsForm
                newParticipantsForm =
                    { oldParticipantsForm | name = Just name }
            in
                {model | participantsForm = newParticipantsForm} ! [ Cmd.none ]

        FormNumberOfTicketsChange numberOfTickets ->
            let
                oldParticipantsForm = model.participantsForm
                newParticipantsForm =
                    { oldParticipantsForm | numberOfTickets = Just numberOfTickets}
            in
                {model | participantsForm = newParticipantsForm} ! [ Cmd.none ]

        DeleteParticipant id ->
            {model | participants = filter (\a -> a.id /= id) model.participants } ! [ Cmd.none ]

        UrlChange location ->
                    { model | page = (getPage location.hash) } ! [ Cmd.none ]


---- VIEW ----

viewParticipant : Participant -> Html Msg
viewParticipant participant =
    tr []
        [ td [] [text participant.name]
        , td [] [text (toString participant.numberOfTickets) ]
        , td [] [button [classList [("btn", True), ("btn-link", True)], onClick (DeleteParticipant participant.id) ] [ text "Delete"] ]
        ]

listParticipants: List (Participant) -> Html Msg
listParticipants participants =
    div [] [ table [ classList [("table", True)] ] [
                thead [] [
                    tr [] [
                        td [] [text "Name"]
                        , td [] [text "# tickets"]
                        , td [] []
                    ]
                ]
                , tbody [] (List.map viewParticipant participants)
            ]
                     ]

viewParticipantForm: ParticipantsForm -> Html Msg
viewParticipantForm form =
    div [classList [("row", True)]]
        [ Html.form [ onSubmit AddParticipant]
            [ div [classList
                [("form-group", True), ("col-md-6", True)]
            ] [ input [ type_ "text", value (Maybe.withDefault "" form.name) , placeholder "Name", onInput FormNameChange, classList [("form-control", True)] ] [] ]
            , div [classList
                    [("form-group", True), ("col-md-4", True)]
            ] [ input [ type_ "number", value (Maybe.withDefault "" form.numberOfTickets), placeholder "# tickets", onInput FormNumberOfTicketsChange, classList [("form-control", True)] ] []]
            , div [classList
                    [("col-md-2", True)]
            ] [ input [ type_ "submit",  value "+", classList [("btn", True), ("btn-primary", True)] ] []]
          ]
      ]

nav: Model -> Html Msg
nav model =
    ul [classList [("nav", True), ("nav-pills", True)]]
    [
        li [classList [("active", model.page == Participants)]] [
            a [href "#/Participants"] [text "Participants"]
        ]
        , li [classList [("active", model.page == Drawing)]] [
            a [href "#/Drawing"] [text "Drawing"]
        ]
    ]

view : Model -> Html Msg
view model =
    div [classList [("container", True)]]
        [   (nav model)
            , div [classList [("row", True)]]
            [
                content model
            ]
        ]

content : Model -> Html Msg
content model =
    case model.page of
        Participants ->
            div [classList [("col-md-5", True)]
            ] [
                  (viewParticipantForm model.participantsForm)
                , (listParticipants model.participants)
            ]

        Drawing ->
            h1 [] [ text "Drawing!" ]

---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
