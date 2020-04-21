module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, disabled, height, href, property, src, width)
import Html.Events exposing (onClick)
import Json.Encode
import List exposing (drop, range, take)
import Random
import Random.List exposing (shuffle)
import Url
import Task
import Browser.Dom exposing (Viewport)



{-
   To do:
       (1) Update the favicon.ico requirements (multiple sizes) - see the images in the public folder, and follow the link in the manifest.json file explaining how to handle favicos.
       (2) The window width is set when the game starts: but what if the window width changes? let's handle that too.
       (2a) Fix: the grid and button sizes
       (3) Fix: Styling of the game: make it look sleek and nice, for mobile and web views.       
       (4) Make the advanced / easy game settings appear at the start of the game.
       (5) Bring up a modal when the level completes.
       (6) Reduce the verbiage at the top of the screen.
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
    , numberClicked : Int
    , endingNumber : Int
    , gameState : GameState
    , numbers : List Int
    , windowWidth : Float
    , gameMode : GameMode
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url 1 0 3 BeforeStarting [] 560 Hard
    , randomiseNumbers
    )



-- Initial Values functions

type GameMode
    = Easy
    | Hard

type GameState
    = BeforeStarting
    | Running
    | Lose
    | Win


startingNumber : Int
startingNumber =
    1


totalNumbers : Int
totalNumbers =
    30



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RandomizeNumbers (List Int)
    | NumberPressed Int
    | ResetLevel
    | NextLevel
    | PreviousLevel
    | ViewPortInfo Viewport
    | SetGameMode GameMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        getFinalLevelNumber =
            case List.minimum [ model.endingNumber + 1, totalNumbers ] of
                Just finalLevel ->
                    finalLevel

                Nothing ->
                    totalNumbers

        proceedToNextLevel =
            ( { model
                | gameState = BeforeStarting
                , currentNumberToClick = 1
                , numberClicked = 0
                , endingNumber = getFinalLevelNumber
              }
            , randomiseNumbers
            )
    in
    case msg of
        RandomizeNumbers numbers ->
            ( { model | numbers = numbers }, getWindowWidth )

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
                theGameHasStarted =
                    if (number == startingNumber) && (model.gameState == BeforeStarting) then
                        True

                    else
                        False

                youClickedIncorrectly =
                    if number /= model.currentNumberToClick && number <= model.endingNumber then
                        True

                    else
                        False

                levelIsFinished =
                    if number == model.currentNumberToClick && number == model.endingNumber then
                        True

                    else
                        False
            in
            case model.gameState of
                BeforeStarting ->
                    if theGameHasStarted then
                        ( { model
                            | gameState = Running
                            , currentNumberToClick = model.currentNumberToClick + 1
                            , numberClicked = number
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Running ->
                    if youClickedIncorrectly then
                        ( { model | gameState = Lose, numberClicked = number }, Cmd.none )

                    else if levelIsFinished then
                        ( { model | gameState = Win, numberClicked = number }, Cmd.none )

                    else
                        ( { model | currentNumberToClick = model.currentNumberToClick + 1, numberClicked = number }, Cmd.none )

                Win ->    
                  case model.gameMode of
                      Easy ->
                        ( { model
                                | gameState = BeforeStarting
                                , currentNumberToClick = 1
                                , numberClicked = 0
                                , endingNumber = getFinalLevelNumber
                              }
                            , Cmd.none
                            )

                      Hard ->
                             ( { model
                                | gameState = BeforeStarting
                                , currentNumberToClick = 1
                                , numberClicked = 0
                                , endingNumber = getFinalLevelNumber
                              }
                            , randomiseNumbers
                            )                     

                Lose ->
                    ( { model | currentNumberToClick = startingNumber, gameState = BeforeStarting, numberClicked = number }, Cmd.none )

        ResetLevel ->
            ( { model | gameState = BeforeStarting, currentNumberToClick = startingNumber, numberClicked = 0 }, Cmd.none )

        NextLevel ->
            case model.gameMode of
                Easy ->
                    ( { model
                            | gameState = BeforeStarting
                            , currentNumberToClick = 1
                            , numberClicked = 0
                            , endingNumber = getFinalLevelNumber
                          }
                        , Cmd.none
                        )

                Hard ->
                         ( { model
                            | gameState = BeforeStarting
                            , currentNumberToClick = 1
                            , numberClicked = 0
                            , endingNumber = getFinalLevelNumber
                          }
                        , randomiseNumbers
                        )

        PreviousLevel ->
            let
                previousLevelNumber =
                    if (model.endingNumber - 1) >= (startingNumber + 1) then
                        model.endingNumber - 1

                    else
                        startingNumber + 1
            in
            ( { model
                | gameState = BeforeStarting
                , currentNumberToClick = 1
                , endingNumber = previousLevelNumber
                , numberClicked = 0
              }
            , randomiseNumbers
            )
        ViewPortInfo viewport->
            ({model | windowWidth = viewport.viewport.width}, Cmd.none)
        SetGameMode gameMode ->
            ({model | gameMode = gameMode }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.url.path of
        "/numberSequence/home" ->
            game model

        "/numberSequence/genesis" ->
            genesisOfTheGame model

        _ ->
            game model


debugger : Model -> Browser.Document Msg
debugger model =
    { title = "URL Interceptor"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/home" "home"
            , viewLink "/profile" "profile"
            , viewLink "/reviews/the-century-of-the-self" "/reviews/the-century-of-the-self"
            , viewLink "/reviews/public-opinion" "/reviews/public-opinion"
            , viewLink "/reviews/shah-of-shahs" "/reviews/shah-of-shahs"
            ]
        ]
    }


game : Model -> Browser.Document Msg
game model =
    { title = "Number Sequence Game"
    , body =
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ h1 [] [ text "Number Sequence Game" ]
                , small [class "is-size-7"] [text "Goal: Can you get to level 20?"]
                , br [] []
                , instructions model
                , br [] []
                , showButtons model
                , br [] []
                , displayGameSettings model
                , br [] []
                , gameButtonOptions model
                , br [] []
                , gameSettingsLinks model
                , br [] []
                , viewLink "/numberSequence/genesis" "Genesis of this game?"
                ]
            ]
        ]
    }

displayGameSettings : Model -> Html Msg
displayGameSettings model =
    let
        displayGameModeString = case model.gameMode of
                                  Easy -> "Easy"
                                  Hard -> "Hard"
            
    in
        
    p [] [text ("Level: " ++ String.fromInt(model.endingNumber) ++ " - Mode: " ++ displayGameModeString  )]


gameSettingsLinks : Model -> Html Msg
gameSettingsLinks model =
    let
      gameModeButtons = [ button [ class "button is-primary is-light", onClick (SetGameMode Easy)] [ text "Set to Easy" ]  
                        , button [ class "button is-primary is-light", onClick (SetGameMode Hard)] [ text "Set to Hard" ] 
                        ]

            
    in
        
    if model.gameState == Lose || model.gameState == Win then
        div [] ([br [][] ] ++ gameModeButtons)
    else
        div []
            ([ button [ class "button is-link is-light", onClick PreviousLevel ] [ text "< Previous Level" ]             
                        , button [ class "button is-link is-light", onClick NextLevel ] [ text " Next Level > " ] 
                        , br [] [] 
                        , br [][]            
                        ] ++ gameModeButtons)


gameButtonOptions : Model -> Html Msg
gameButtonOptions model =
    case model.gameState of
        Lose ->
            button [ class "button is-info", onClick ResetLevel ] [ text "Reset Game" ]

        Win ->
            button [ class "button is-info", onClick NextLevel ] [ text "Go to the next level!" ]

        _ ->
            div [] []


instructions : Model -> Html Msg
instructions model =
    case model.gameState of
        BeforeStarting ->
            p [] [ text ("Instructions: Memorise number positions, then click from 1 to " ++ String.fromInt model.endingNumber) ]

        Running ->
            p [] [ text ("Instructions: Memorise number positions, then click from 1 to " ++ String.fromInt model.endingNumber) ]

        Win ->
            p [ class "notification is-success is-light" ] [ text ("Congrats! Let's progress to level " ++ String.fromInt (model.endingNumber + 1)) ]

        Lose ->
            p [] [ text "Oh no! Wanna try again?" ]


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
    div [ class "columns is-mobile is-gapless" ]
        ( [div [class "column"][]] ++  List.map (\x -> showButton model x) list ++ [div [class "column"][]])


showButton : Model -> Int -> Html Msg
showButton model numberOnButton =
    let
        displayTextOnButton =
            case model.gameState of
                BeforeStarting ->
                    if numberOnButton <= model.endingNumber then
                        String.fromInt numberOnButton

                    else
                        ""

                Running ->
                    if numberOnButton <= model.endingNumber then
                        ""

                    else
                        ""

                Lose ->
                    if numberOnButton <= model.endingNumber then
                        String.fromInt numberOnButton

                    else
                        ""

                Win ->
                    if numberOnButton <= model.endingNumber then
                        String.fromInt numberOnButton

                    else
                        ""

        setButtonClass =
            case model.gameState of
                BeforeStarting ->
                    if numberOnButton == model.numberClicked then
                        "button is-info is-large-desktop"

                    else if numberOnButton <= model.endingNumber then
                        "button is-primary is-large-desktop"

                    else
                        "button is-large-desktop"

                Running ->
                    if numberOnButton == model.numberClicked then
                        "button is-info is-large-desktop"

                    else if numberOnButton <= model.endingNumber then
                        "button is-primary is-large-desktop"

                    else
                        "button is-large-desktop"

                Lose ->
                    if numberOnButton == model.numberClicked then
                        "button is-info is-large-desktop"

                    else if numberOnButton <= model.endingNumber then
                        "button is-primary is-large-desktop"

                    else
                        "button is-danger is-large-desktop"

                Win ->
                    if numberOnButton == model.numberClicked then
                        "button is-info is-large-desktop"

                    else if numberOnButton <= model.endingNumber then
                        "button is-primary is-large-desktop"

                    else
                        "button is-small"
    in
    div [ class "column" ]
        [ button [ class setButtonClass, onClick (NumberPressed numberOnButton) ] [ text displayTextOnButton ]
        ]


genesisOfTheGame : Model -> Browser.Document Msg
genesisOfTheGame model =
    { title = "Genesis"
    , body =
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ h1 [] [ text "Number Sequence Game: Genesis" ]
                , p [ class "has-text-left" ] [ text "After watching the following video, it dawned on me: are chimps smarter than humans at memorising number positions?" ]
                , br [] []
                , p [ class "has-text-left" ] [ text "To redress this travesty, I created this game: perhaps after a couple of hours of intense training, we can outwit this cheeky monkey. Yes sir: nobody's gonna make a monkey out of me! (Do check out the video: it's quite cool!)" ]
                , br [] []
                , videoframe model.windowWidth
                , br [] []
                , br [] []
                , viewLink "/numberSequence/home" "Back to game"
                ]
            ]
        ]
    }


viewLink : String -> String -> Html msg
viewLink path textAnnotation =
    div [] [ a [ href path ] [ text textAnnotation ] ]


videoframe windowWidth =
    let
        calculdatedWidth = if windowWidth < 560 then
                             ((round windowWidth) - 80)
                           else
                             560
    in
    
    iframe
        [ width calculdatedWidth
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


getWindowWidth : Cmd Msg
getWindowWidth =
  Task.perform ViewPortInfo Browser.Dom.getViewport

{- , property "allow" (Json.Encode.string "accelerometer; gyroscope; picture-in-picture") -}
