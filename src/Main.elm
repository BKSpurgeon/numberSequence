module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, div, h1, img, section, li, a, b, ul)
import Html.Attributes exposing (src, class, href)
import Url

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
    if model.url.path == "/home" then
        game model
    else
        explanationOfTheGame model


game : Model -> Browser.Document Msg
game model =
  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      , viewLink "/explanation" "Genesis of this game?"
      ]
  }

explanationOfTheGame : Model -> Browser.Document Msg
explanationOfTheGame model =
  { title = "Genesis"
  , body =
      [ div [ class "container"]
            [ h1 [] [text "Man vs Chimp"]]     
      , viewLink "/home" "Back to game"
      ]
  }


viewLink : String -> String -> Html msg
viewLink path textAnnotation =
  div [] [ a [ href path ] [ text textAnnotation ] ]