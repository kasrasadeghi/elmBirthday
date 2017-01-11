module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (append, concat)
import Date exposing (Date, Month, month, year, day)
import Task exposing (..)


main =
    Html.program
        { init = ( model, getTime )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { name : String
    , birthday : String
    , current : Date
    }

    
model : Model
model =
    { name = ""
    , birthday = "2000/01/01"
    , current = Date.fromTime 0
    }



-- UPDATE


type Msg
    = Name String
    | Birthday String
    | SyncDate Date

      
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            { model | name = name } ! []

        Birthday birthday ->
            { model | birthday = birthday } ! []

        SyncDate date ->
            { model | current = date } ! []

                
getTime : Cmd Msg
getTime =
    Task.perform SyncDate Date.now



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ("text-align", "center"), ("font-size", "7vw")]]
        [ input [ style [("width", "50%"), ("font-size", "7vw") ], placeholder "Name", onInput Name ] []
        , input [ style [("width", "50%"), ("font-size", "7vw") ], placeholder "YYYY/MM/DD", onInput Birthday ] []
        , div []
            [ renderGreeting model.name
            , br [] []
            , renderResponse (Date.fromString model.birthday) model
            ]
        ]


renderGreeting : String -> Html Msg
renderGreeting name =
    case name of
        "" -> text ""
        _  -> concat [ "Hello ", name, "." ] |> text


renderResponse : Result String Date -> Model -> Html Msg
renderResponse result model =
    case result of
        Err string ->
            text string

        Ok date ->
            div []
                [ text
                    (concat
                        [ "Your birthday is on "
                        , date |> month |> toString
                        , append " " (date |> day |> toString)
                        , append ", " (date |> year |> toString)
                        , "."
                        ]
                    )
                , br [] []
                , renderAge date model
                ]


renderAge : Date -> Model -> Html Msg
renderAge date model =
    let
        datenow = model.current                  
        a = 
            let
                datemonth = date |> month
                monthint = month2int datemonth
                           
                nowmonth = datenow |> month                    
                nowint = month2int nowmonth
                         
                happenedAge = (datenow |> year) - (date |> year)
                notHappenedAge = happenedAge - 1
            in
                if monthint < nowint then
                    happenedAge
                else if monthint > nowint then
                    notHappenedAge
                else
                    let
                        dateday = date |> day
                        nowday = datenow |> day
                    in
                        if dateday < nowday then
                            happenedAge
                        else if nowday < dateday then
                            notHappenedAge
                        else
                            -1
    in
        if (date |> Date.toTime) - (datenow |> Date.toTime) >= 0 then
            text "You haven't been born yet, you goober. I hope your time has Mars Colonies."
        else if a == -1 then
            text "Happy Birthday"
        else
            text <| concat [ "You are ", toString a, " years old!" ]


month2int : Month -> Int
month2int datemonth =
    case datemonth of
        Date.Jan -> 1
        Date.Feb -> 2
        Date.Mar -> 3
        Date.Apr -> 4
        Date.May -> 5
        Date.Jun -> 6
        Date.Jul -> 7
        Date.Aug -> 8
        Date.Sep -> 9
        Date.Oct -> 10
        Date.Nov -> 11
        Date.Dec -> 12
