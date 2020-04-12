module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, div, h1, img, section, li, a, b, ul, iframe)
import Html.Attributes exposing (src, class, href, width, height, property)
import Url

import Json.Encode

{-
To do:
    (1) Update the favicon.ico requirements (multiple sizes)

-}
---- MODEL ----



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
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url, Cmd.none )



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
      [ section [class "section"]
        [ div [ class "container"]
          [ h1 [] [text "Number Sequence Game: Genesis"]
          , viewLink "/genesis" "Genesis of this game?"
          ]           
        ]        
      ]
  }

genesisOfTheGame : Model -> Browser.Document Msg
genesisOfTheGame model =
  { title = "Genesis"
  , body =
      [ section [ class "section"]
        [ div [ class "container"]
          [ h1 [] [text "Number Sequence Game: Genesis"]
          , viewLink "/home" "Back to game"  
          , videoframe          
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

{-, property "allow" (Json.Encode.string "accelerometer; gyroscope; picture-in-picture")-}
