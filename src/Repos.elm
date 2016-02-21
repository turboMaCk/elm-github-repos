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
  , isLoading : Bool }

initialModel : Model
initialModel =
  { userName = "tusdrbomack"
  , repos = []
  , isLoading = True }

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

fetchData : String -> Effects Action
fetchData name =
  Http.get reposDecoder (getUrl name)
        |> Task.toMaybe
        |> Task.map FetchDone
        |> Effects.task

-- Init

-- init : Model -> Effects Action
init =
  ( initialModel
  , fetchData initialModel.userName )

-- Actions

type Action =
    FetchData (String)
    | FetchDone (Maybe (List Repo))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchData name ->
      ( { model | isLoading = True }
      , fetchData name )
    FetchDone result ->
      let
        getRepos =
          case result of
            Just value -> value
            _ -> []
      in
        ( { model
            | repos = getRepos
            , isLoading = False }
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
  let
    size =
      List.length repos
    empty =
      div [] [ text "Not found" ]
    list =
      ul [] ( List.map (repoView address) repos )
  in
    if size == 0 then empty else list

view : Signal.Address Action -> Model -> Html
view address model =
  let
    content =
      if model.isLoading then loadingView address else reposListView address model.repos
  in
    div []
        [ headerView address model
        , div [] [ text ("Results for `" ++ model.userName ++ "`:")]
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
