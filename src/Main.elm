module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, br, div, h1, iframe, img, li, p, section, text, ul, button)
import Html.Attributes exposing (class, height, href, property, src, width, disabled)
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
    , endingNumber : Int
    , gameState : GameState
    , numbers : List Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url 1 3 BeforeStarting []
    , randomiseNumbers 
    )


-- Initial Values functions

type GameState =
  BeforeStarting
  | Running
  | Lose
  | Win



startingNumber : Int
startingNumber = 1

totalNumbers : Int
totalNumbers = 30

-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RandomizeNumbers  (List Int)
    | NumberPressed Int
    | ResetLevel
    | NextLevel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        proceedToNextLevel = ({model | gameState = BeforeStarting
                                        , currentNumberToClick = 1
                                        , endingNumber = model.endingNumber + 1}, randomiseNumbers)
            
    in        
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
                youClickedIncorrectly = if number /= model.currentNumberToClick && number <= model.endingNumber then
                                           True
                                        else
                                           False
                levelIsFinished = if number == model.currentNumberToClick && number == model.endingNumber then
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
                        ({model | gameState = Lose }, Cmd.none)
                    else if levelIsFinished then
                        ({model | gameState = Win}, Cmd.none)
                    else
                        ({model| currentNumberToClick = (model.currentNumberToClick + 1)}, Cmd.none)
                Win ->
                     proceedToNextLevel
                Lose ->
                    ({model | currentNumberToClick = startingNumber, gameState = BeforeStarting }, Cmd.none)
        ResetLevel ->
            ({model | gameState = BeforeStarting, currentNumberToClick = startingNumber}, Cmd.none)
        NextLevel ->
            proceedToNextLevel


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
                , instructions model
                , br [] [] 
                , showButtons model
                , br [] []
                , gameButtonOptions model
                , viewLink "/genesis" "Genesis of this game?"
                ]
            ]
        ]
    }

gameButtonOptions : Model -> Html Msg
gameButtonOptions model =
    case model.gameState of
        Lose ->
            button [class "button is-info", onClick ResetLevel] [ text "Reset Game"]
        Win ->
            button [class "button is-info", onClick NextLevel] [ text "Go to the next level!"]
        _ ->
            div [] []

instructions : Model -> Html Msg
instructions model = 
    case model.gameState of
        BeforeStarting ->
          p [] [text ("Instructions: Memorise the number positions, then click from 1 to " ++ String.fromInt(model.endingNumber))  ]
        Running ->
          p [] []
        Win ->
          p [class "notification is-success is-light"] [text ("Congrats! Let's progress to level " ++ String.fromInt (model.endingNumber + 1))]
        Lose ->
          p [] [text "Oh no! Wanna try again?" ]


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
                                if numberOnButton <= model.endingNumber then
                                    String.fromInt numberOnButton
                                else 
                                    "x"
                              Running ->
                                if numberOnButton <= model.endingNumber then
                                    ""
                                else 
                                    ""
                              Lose ->
                                 if numberOnButton <= model.endingNumber then
                                    String.fromInt numberOnButton
                                 else 
                                    "x"
                              Win ->
                                if numberOnButton <= model.endingNumber then
                                    String.fromInt numberOnButton
                                else 
                                    "x"

              setButtonClass = case model.gameState of
                                  BeforeStarting ->
                                    if numberOnButton <= model.endingNumber then
                                        "button is-primary is-large"
                                    else 
                                        "button is-large"
                                  Running ->
                                    if numberOnButton <= model.endingNumber then
                                        "button is-primary is-large"
                                    else 
                                        "button is-large"
                                  Lose -> 
                                    if numberOnButton <= model.endingNumber then
                                        "button is-primary is-large"
                                    else 
                                        "button is-danger is-large"
                                  Win ->
                                    if numberOnButton <= model.endingNumber then
                                        "button is-primary is-large"
                                    else 
                                        "button is-large"
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

-- Commands
randomiseNumbers : Cmd Msg
randomiseNumbers =
   Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber totalNumbers))

{- , property "allow" (Json.Encode.string "accelerometer; gyroscope; picture-in-picture") -}
