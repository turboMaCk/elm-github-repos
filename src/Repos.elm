module Repos where

import List
import Graphics.Element exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Signal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Effects exposing (Effects)
import StartApp

-- Model

type alias Repo =
  { id : Int
  , name : String
  , htmlUrl : String
  -- , description : String
  , sshUrl : String
  -- , language : String
  , stargazersCount: Int
  , avatarUrl: String }

type alias Model =
  { userName : String
  , resultsFor : String
  , repos : List Repo
  , isLoading : Bool
  , alert : String
  , selected : Int
  , sortBy : SortBy }

type SortBy = Name
    | Stars

initialModel : Model
initialModel =
  { userName = "turbomack"
  , resultsFor = "turbomack"
  , repos = []
  , isLoading = True
  , alert = ""
  , selected = -1
  , sortBy = Name }

isSelected : Model -> Repo -> Bool
isSelected model repo =
  if model.selected == repo.id then True else False

sort : SortBy -> List Repo -> List Repo
sort sortBy repos =
  case sortBy of
    Name ->
      repos |> List.sortBy .name
    Stars ->
      repos |> List.sortBy .stargazersCount |> List.reverse

reposDecoder : Json.Decoder (List Repo)
reposDecoder =
  Json.list repoDecoder

repoDecoder : Json.Decoder Repo
repoDecoder =
  Json.object6
        Repo
          ("id" := Json.int)
          ("name" := Json.string)
          ("html_url" := Json.string)
          -- ("description" := Json.string)
          ("ssh_url" := Json.string)
          -- ("language" := Json.string)
          ("stargazers_count" := Json.int)
          (Json.at ["owner", "avatar_url"] Json.string)

getUrl : String -> String
getUrl name =
  "https://api.github.com/users/" ++ name ++ "/repos"

fetchData : String -> Task.Task a (Result Http.Error (List Repo))
fetchData name =
  Http.get reposDecoder (getUrl name)
      |> Task.toResult

httpErrorToString : String -> Http.Error -> String
httpErrorToString name err =
  case err of
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Connection problem"
    Http.UnexpectedPayload _ -> "That's weird. Something is broken!"
    Http.BadResponse status msg ->
      case status of
        404 -> name ++ "not found:("
        _ -> msg

httpResultToAction : String -> Result Http.Error (List Repo) -> Action
httpResultToAction name result =
  case result of
    Ok repos ->
      FetchDone repos
    Err err ->
      Error (httpErrorToString name err)

fetchDataAsEffects : String -> Effects Action
fetchDataAsEffects name =
  fetchData name
    |> Task.map (httpResultToAction name)
    |> Effects.task

-- Init

init : ( Model, Effects Action )
init =
  ( initialModel
  , fetchDataAsEffects initialModel.userName )

-- Actions

type Action = NoOp
    | FetchData String
    | FetchDone (List Repo)
    | Error String
    | NameChanged String
    | SelectRepo Repo
    | ChangeSort SortBy

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )
    FetchData name ->
      ( { model
          | isLoading = True
          , resultsFor = model.userName }
      , fetchDataAsEffects model.userName )
    FetchDone results ->
      ( { model
          | repos = results
          , isLoading = False
          , alert = "" }
      , Effects.none )
    Error msg ->
      ( { model
          | repos = []
          , isLoading = False
          , alert = msg }
      , Effects.none )
    NameChanged name ->
      ( { model
          | userName = name }
      , Effects.none )
    SelectRepo repo ->
      let
        value =
          if repo.id == model.selected then -1 else repo.id
      in
        ( { model
            | selected = value }
        , Effects.none )
    ChangeSort attr ->
      ( { model
          | sortBy = attr }
      , Effects.none )

-- View

onInput : Signal.Address Action -> (String -> Action) -> Attribute
onInput address f =
  Events.on "input" Events.targetValue (\v -> Signal.message address (f v))

oSubmit address value =
  Events.onWithOptions "submit"
    { stopPropagation = True, preventDefault = True }
    Json.value (\_ -> Signal.message address (FetchData value))

headerView : Signal.Address Action -> Model -> Html
headerView address model =
  header
    [ class "header" ]
    [ img
      [ src "assets/octocat.png"
      , class "octo-cat" ] []
    , h1
      [ class "headline" ]
      [ text "Repos" ]
    , Html.form
        [ onSubmit address model.userName
        , class "search-form" ]
        [ span
          [ class "hint" ]
          [ text "github.com/" ]
        , input
            [ value model.userName
            , onInput address NameChanged
            , class "search-field" ] []
        , button
            [ type' "submit"
            , class "submit-btn" ]
            [ text "Go" ] ]]

sortView : Signal.Address Action -> Model -> Html
sortView address model =
  let
    isActive attr =
      model.sortBy == attr
    classNames attr =
      if isActive attr then "active" else "inactive"
  in
    div
      [ class "sort-filter" ]
      [ button
        [ class (classNames Name)
        , Events.onClick address (ChangeSort Name) ]
        [ text "name" ]
      , button
        [ class (classNames Stars)
        , Events.onClick address (ChangeSort Stars) ]
        [ text "stars" ]]

loadingView : Signal.Address Action -> Html
loadingView address =
  div []
  [ text "loading..." ]

repoView : Signal.Address Action -> Model -> Repo -> Html
repoView address model repo =
  let
    classNames =
      if isSelected model repo then "repo selected" else "repo"
    cloneValue =
      "git clone " ++ repo.sshUrl
  in
    li
    [ class classNames ]
    [ div
      [ class "repo-main"
      , Events.onClick address (SelectRepo repo)]
      [ img
        [ src repo.avatarUrl
        , class "avatar" ] []
      , div
        [ class "repo-info" ]
        [ span
          [ class "stars-count" ]
          [ text ("stars: " ++ (toString repo.stargazersCount)) ]
        , h2
          [ class "repo-name" ]
          [ a
          [ href repo.htmlUrl
          , target "_blank" ]
          [ text repo.name ]]]]
    , div
      [ class "repo-details" ]
      [ div
        [ class "clone" ]
        [ label
          [ class "clone-label"]
          [ text "clone:"
          , input
            [ class "clone-input"
            , disabled True
            , value cloneValue ] []]]]]

reposListView : Signal.Address Action -> Model -> Html
reposListView address model =
  ul
  [ class "repos-list" ]
  ( model.repos |> sort model.sortBy |> List.map (repoView address model) )

alertView : Signal.Address Action -> String -> Html
alertView address msg =
  div [ class "error alert" ] [ text msg ]

view : Signal.Address Action -> Model -> Html
view address model =
  let
    content =
      if model.isLoading then loadingView address else reposListView address model
  in
    div []
    [ a
      [ href "https://github.com/turboMaCk/elm-github-repos"
      , class "fork-link" ]
      [ text "Fork me on Github" ]
    , div
      [ class "app-container" ]
      [ headerView address model
      , sortView address model
      , div
        [ class "results-for" ]
        [ text ("Results for `" ++ model.resultsFor ++ "`:")]
      , alertView address model.alert
      , content ]]

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [] }

main : Signal Html
main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
