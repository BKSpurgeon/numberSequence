module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, br, div, h1, iframe, img, li, p, section, text, ul, button)
import Html.Attributes exposing (class, height, href, property, src, width)
import Html.Events exposing (onClick)
import Random
import Random.List exposing (shuffle)
import List exposing (take, drop, range)
import Json.Encode
import Url



{-
   To do:
       (1) Update the favicon.ico requirements (multiple sizes) - see the images in the public folder, and follow the link in the manifest.json file explaining how to handle favicos.
       (2) Resise the  youtube video depending on the size of the window.
       (3) Fix: Styling of the game: make it look sleek and nice, for mobile and web views.
-}
-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentNumberToClick : Int -- i.e. if we need to click on 1, then the currentNumber to click will be 1
    , gameState : GameState
    , numbers : List Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url 1 BeforeStarting []
    , Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber totalNumbers))
    )


-- Initial Values functions

type GameState =
  BeforeStarting
  | Running
  | GameOver


startingNumber : Int
startingNumber = 1

endingNumber : Int
endingNumber = 10

totalNumbers : Int
totalNumbers = 30

-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RandomizeNumbers  (List Int)
    | NumberPressed Int
    | ResetLevel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomizeNumbers numbers ->
            ( { model | numbers = numbers }, Cmd.none )
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )
        NumberPressed number ->
            let
                theGameHasStarted = if (number == startingNumber) && (model.gameState == BeforeStarting) then
                                       True
                                    else
                                       False                    
                youClickedIncorrectly = if number /= model.currentNumberToClick then
                                           True
                                        else
                                           False
            in

            case model.gameState of
                BeforeStarting ->
                    if theGameHasStarted then
                        ( {model | gameState = Running
                                 , currentNumberToClick = (model.currentNumberToClick + 1)}
                        , Cmd.none )
                    else
                        (model, Cmd.none)
                Running ->
                    if youClickedIncorrectly then
                        ({model | gameState = GameOver, currentNumberToClick = (model.currentNumberToClick + 1) }, Cmd.none)
                    else 
                        (model, Cmd.none)
                GameOver ->
                    (model, Cmd.none)
        ResetLevel ->
            ({model | gameState = BeforeStarting, currentNumberToClick = startingNumber}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.url.path of
        "/home" ->
            game model

        "/genesis" ->
            genesisOfTheGame model

        _ ->
            game model


game : Model -> Browser.Document Msg
game model =
    { title = "Number Sequence Game"
    , body =
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ h1 [] [ text "Number Sequence Game" ]                
                , br [] []
                , instructions
                , br [] [] 
                , showButtons model
                , br [] []
                , if model.gameState == GameOver then
                    button [class "button is-info", onClick ResetLevel] [ text "Reset Button"]
                  else 
                    div [] []
                , viewLink "/genesis" "Genesis of this game?"
                ]
            ]
        ]
    }

instructions : Html Msg
instructions = 
    p [] [text ("Instructions: First memorise the numbers, then click from 1 to " ++ String.fromInt(endingNumber))  ]

split : Int -> List a -> List (List a)
split i list =
    case take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (drop i list)


showButtons : Model -> Html Msg
showButtons model = 
    div [] ((split 6 <| model.numbers) |> List.map (\x -> showButtonRow model x))

showButtonRow : Model -> List Int -> Html Msg
showButtonRow model list = 
  div [class "columns is-mobile is-gapless"]
      (List.map (\x -> showButton model x) list)

showButton : Model -> Int -> Html Msg
showButton model numberOnButton =  
  let
              displayTextOnButton = case model.gameState of
                              BeforeStarting ->
                                if numberOnButton <= endingNumber then
                                    String.fromInt numberOnButton
                                else 
                                    "x"
                              Running ->
                                if numberOnButton <= endingNumber then
                                    ""
                                else 
                                    ""
                              GameOver ->
                                 if numberOnButton <= endingNumber then
                                    String.fromInt numberOnButton
                                 else 
                                    "x"

              setButtonClass = case model.gameState of
                                  BeforeStarting ->
                                    if numberOnButton <= endingNumber then
                                        "button is-primary is-large"
                                    else 
                                        "button is-large"
                                  Running ->
                                    if numberOnButton <= endingNumber then
                                        "button is-primary is-large"
                                    else 
                                        "button is-large"
                                  GameOver -> 
                                    if numberOnButton <= endingNumber then
                                        "button is-primary is-large"
                                    else 
                                        "button is-danger is-large"
                                    

  in
  div [class "column"]
      [button [class setButtonClass, onClick (NumberPressed numberOnButton)] [text displayTextOnButton] 
      ]


genesisOfTheGame : Model -> Browser.Document Msg
genesisOfTheGame model =
    { title = "Genesis"
    , body =
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ h1 [] [ text "Number Sequence Game: Genesis" ]
                , p [] [ text "After watching the following video, it dawned on me that chimps can beat humans (albeit with some training)!" ]
                , p [] [ text "Perhaps we can, in fact, be trained to beat chimps? Hence the genesis of this game. You can see the video below:" ]
                , videoframe
                , br [] []
                , br [] []
                , viewLink "/home" "Back to game"
                ]
            ]
        ]
    }


viewLink : String -> String -> Html msg
viewLink path textAnnotation =
    div [] [ a [ href path ] [ text textAnnotation ] ]


videoframe =
    iframe
        [ width 560
        , height 315
        , src "https://www.youtube.com/embed/zsXP8qeFF6A"
        , property "frameborder" (Json.Encode.string "0")
        , property "allowfullscreen" (Json.Encode.string "true")
        ]
        []



{- , property "allow" (Json.Encode.string "accelerometer; gyroscope; picture-in-picture") -}
