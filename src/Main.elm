module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import List exposing (filter)
import Random exposing (int, generate, Seed, initialSeed)
import Navigation

---- MODEL ----


type alias Participant =
    {
        id: Int,
        name: String,
        numberOfTickets: Int
    }

type alias Winner =
    {
        name: String
    }

type alias ParticipantsForm =
    {
        name: Maybe String,
        nameError: Bool,
        numberOfTickets: Maybe String,
        numberOfTicketsError: Bool
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
        winners: List (Winner),
        participantsForm: ParticipantsForm,
        page : Page,
        seed : Seed
    }

defaultForm = {
        name = Nothing,
        nameError = False,
        numberOfTickets = Nothing,
        numberOfTicketsError = False
    }

initialModel = {
        seed = initialSeed 45634,
        page = Participants,
        participants = [],
        winners = [],
        participantsForm = defaultForm
    }

init : Navigation.Location -> ( Model, Cmd Msg )
init = (\_ -> (initialModel, Cmd.none))



---- UPDATE ----

type Msg
    = AddParticipant |
    FormNameChange String |
    FormNumberOfTicketsChange String |
    DeleteParticipant Int |
    UrlChange Navigation.Location |
    Draw |
    Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddParticipant ->
            let
                oldParticipantsForm = model.participantsForm
                nameError = case oldParticipantsForm.name of
                    Just name -> String.isEmpty name
                    Nothing -> True
                numberOfTicketsError = case oldParticipantsForm.numberOfTickets of
                    Just number -> String.isEmpty number
                    Nothing -> True

                (participants, participantsForm, seed) = case (nameError || numberOfTicketsError) of
                    True ->
                        let
                            newParticipants = model.participants
                            newParticipantsForm = { oldParticipantsForm | nameError = nameError,
                                                    numberOfTicketsError = numberOfTicketsError }
                        in
                            (newParticipants, newParticipantsForm, model.seed)
                    False ->
                        let
                            (id, seed) = Random.step (int 1000000 999999999) model.seed
                            name = Maybe.withDefault "" oldParticipantsForm.name
                            numberOfTicketsMaybe = (String.toInt (Maybe.withDefault "0" oldParticipantsForm.numberOfTickets))
                            numberOfTickets = numberOfTicketsMaybe |> Result.toMaybe |> Maybe.withDefault 0
                            newParticipant = { id = id, name = name, numberOfTickets = numberOfTickets}
                            newParticipants =  newParticipant :: model.participants
                        in
                            (newParticipants, defaultForm, seed)

            in
                { model | participants = participants,
                        participantsForm = participantsForm,
                        seed = seed } ! [ Cmd.none ]

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

        Draw ->
            let
                flattenedParticipants = List.concatMap (\p -> List.repeat p.numberOfTickets p) model.participants
                gen = Random.int 0 (List.length flattenedParticipants - 1)
                max = (List.length flattenedParticipants - 1)
                (rnd, seed) = Random.step (int 0 max) model.seed
                winnerParticipant = List.drop rnd flattenedParticipants |> List.head
                winners = case winnerParticipant of
                    Just winner -> {name = winner.name} :: model.winners
                    Nothing -> model.winners
                newParticipants = case winnerParticipant of
                    Just participant ->
                        List.map (\a -> if a.id == participant.id then
                                    {a | numberOfTickets = a.numberOfTickets - 1}
                                    else a ) model.participants
                    Nothing -> model.participants
            in
                { model | winners = winners, participants = newParticipants, seed = seed} ! [ Cmd.none ]
        Reset -> initialModel ! [ Cmd.none ]

---- VIEW ----

viewParticipant : Participant -> Html Msg
viewParticipant participant =
    tr []
        [ td [] [text participant.name]
        , td [] [text (toString participant.numberOfTickets) ]
        , td [] [button [classList [("btn", True), ("btn-link", True)],
                        onClick (DeleteParticipant participant.id) ] [ text "Delete"] ]
        ]

listParticipants: List (Participant) -> Html Msg
listParticipants participants =
    if List.isEmpty participants then
        div [] []
    else
        div [] [
            table [ classList [("table", True)] ] [
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
            ] [ input [ type_ "text",
                        value (Maybe.withDefault "" form.name),
                        placeholder "Name",
                        onInput FormNameChange,
                        classList [("form-control", True), ("errorInput", form.nameError)] ] [] ]
            , div [classList
                    [("form-group", True), ("col-md-4", True)]
            ] [ input [ type_ "number",
                        value (Maybe.withDefault "" form.numberOfTickets),
                        placeholder "# tickets",
                        onInput FormNumberOfTicketsChange,
                        classList [("form-control", True), ("errorInput", form.numberOfTicketsError)] ] []]
            , div [classList
                    [("col-md-2", True)]
            ] [ input [ type_ "submit",  value "+", classList [("btn", True), ("btn-primary", True)] ] []]
          ]
      ]

viewWinner : Winner -> Html Msg
viewWinner winner =
    li [style [("font-size", "3em")]]
        [ text winner.name ]

listWinners: List (Winner) -> Html Msg
listWinners winners =
    if List.isEmpty winners then
        div [] []
    else
        ul [ classList [("list-unstyled", True)] ] (List.map viewWinner winners)

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
            , div [classList [("row", True)], style [("margin-top", "2em")]]
            [
                content model
            ], div [classList [("reset", True)]] [
                a [ href "#/Participants", onClick Reset ] [text "Reset"]
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
            div [classList [("col-md-7", True)]
            ] [
                button [classList [("btn", True), ("btn-success", True), ("btn-lg", True)], onClick Draw]
                    [text "Draw"]

                , (listWinners model.winners)
            ]

---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
