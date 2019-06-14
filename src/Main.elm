import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main : Program () Model Msg
main =
    Browser.sandbox
        {
            init = init
            , update = update
            , view = view
        }

type alias Model = 
    {
        input : String
        , memos : List String
    }

init : Model
init = 
    {input = "", memos = []}


type Msg
    = Input String | Submit | Delete Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
           { model | input = input } 
    
        Submit ->
            { model | input = "", memos = model.input :: model.memos }

        Delete number ->
            { model | memos =  removeFromList number model.memos }

view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button
                [ disabled (String.isEmpty (String.trim model.input)) ]
                [ text "Submit :)" ]
            ]
        , ul [] (List.indexedMap viewMemo model.memos)
        ]

viewMemo : Int -> String -> Html Msg
viewMemo index memo = 
    li [] [ text memo, button [ onClick (Delete index) ] [text "Delete"] ]

-- https://stackoverflow.com/questions/33099945/how-to-remove-an-item-at-a-given-index-from-array-list-in-elm/33101419
removeFromList i xs =
  (List.take i xs) ++ (List.drop (i+1) xs) 