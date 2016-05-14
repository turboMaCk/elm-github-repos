module Repos exposing (main)

import List
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)

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

headerView : Model -> Html Msg
headerView model =
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

sortView : Model -> Html Msg
sortView model =
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

loadingView : Html Msg
loadingView =
  div []
  [ text "loading..." ]

repoView : Model -> Repo -> Html Msg
repoView model repo =
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

reposListView : Model -> Html Msg
reposListView model =
  ul
  [ class "repos-list" ]
  ( model.repos |> sort model.sortBy |> List.map (repoView model) )

alertView : String -> Html Msg
alertView msg =
  div [ class "error alert" ] [ text msg ]

view : Model -> Html Msg
view model =
  let
    content =
      if model.isLoading then loadingView else reposListView model
  in
    div []
    [ a
      [ href "https://github.com/turboMaCk/elm-github-repos"
      , class "fork-link" ]
      [ text "Fork me on Github" ]
    , div
      [ class "app-container" ]
      [ headerView model
      , sortView model
      , div
        [ class "results-for" ]
        [ text ("Results for `" ++ model.resultsFor ++ "`:")]
      , alertView model.alert
      , content ]]

main : Signal Html
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none }
