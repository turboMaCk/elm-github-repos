import Graphics.Element exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Signal
import Html exposing (..)
import Html.Attributes exposing (..)
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
  , repos : List Repo
  , isLoading : Bool
  , alert : String }

initialModel : Model
initialModel =
  { userName = "tusdrbomack"
  , repos = []
  , isLoading = True
  , alert = "" }

-- Adapter

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
        404 -> name ++ " found:("
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

type Action =
    FetchData (String)
    | FetchDone (List Repo)
    | Error String

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchData name ->
      ( { model | isLoading = True }
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

-- View

headerView : Signal.Address Action -> Model -> Html
headerView address model =
  header []
         [ h1 []
             [ text "Repos" ]
         , Html.form []
                [ span [] [ text "github.com/" ]
                , input [ value model.userName ] []
                , button [ type' "submit" ] [ text "Find" ] ]]

loadingView : Signal.Address Action -> Html
loadingView address =
  div []
      [ text "loading..." ]

repoView : Signal.Address Action -> Repo -> Html
repoView address repo =
  li
    [ ]
    [ img [ src repo.avatarUrl
          , class "avatar" ][]
    , h2 []
         [ text repo.name ]
    , a [ href repo.htmlUrl ]
        [ text repo.htmlUrl ] ]

reposListView : Signal.Address Action -> List Repo -> Html
reposListView address repos =
  ul [] ( List.map (repoView address) repos )

alertView : Signal.Address Action -> String -> Html
alertView address msg =
  div [ class "error alert" ] [ text msg ]

view : Signal.Address Action -> Model -> Html
view address model =
  let
    content =
      if model.isLoading then loadingView address else reposListView address model.repos
  in
    div []
        [ headerView address model
        , div [] [ text ("Results for `" ++ model.userName ++ "`:")]
        , alertView address model.alert
        , content ]

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
