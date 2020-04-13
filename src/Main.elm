module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, br, div, h1, iframe, img, li, p, section, text, ul)
import Html.Attributes exposing (class, height, href, property, src, width)
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
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url 1 BeforeStarting, Cmd.none)


-- Initial Values functions

type GameState =
  BeforeStarting
  | Running


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                [ h1 [] [ text "Number Sequence Game: Genesis" ]
                , viewLink "/genesis" "Genesis of this game?"
                , br [] []
                , showButtons model.gameState
                ]
            ]
        ]
    }


showButtons : GameState -> Html Msg
showButtons gameState = 
  div [class "columns"]
      (List.map (\x -> showButton x gameState ) (List.range startingNumber totalNumbers))

showButton : Int -> GameState -> Html Msg
showButton numberOnButton gameState =  
  let
              displayTextOnButton = case gameState of
                              BeforeStarting ->
                                if numberOnButton <= endingNumber then
                                    String.fromInt numberOnButton
                                else 
                                    ""
                              _ ->
                                if numberOnButton <= endingNumber then
                                    "x"
                                else 
                                    ""
                  
          in

  div [class "column"]
      [text displayTextOnButton]

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
