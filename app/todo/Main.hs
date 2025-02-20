{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Debug.Trace

import Control.Concurrent.Counter (Counter)
import Control.Concurrent.Counter qualified as Counter
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Color
import Iced.Command qualified as Command
import Iced.Widget
import Iced.Widget.Checkbox

data TaskState
    = Idle
    | Editing
    deriving stock (Eq, Ord, Show)

data Task = Task
    { taskId :: Int
    , content :: Text
    , completed :: Bool
    , state :: TaskState
    }
    deriving stock (Eq, Ord, Show)

data AppState = AppState
    { input :: Text
    , tasks :: Vector Task
    , idCounter :: Counter
    }
    deriving stock (Eq)

data AppMessage
    = CreateTask
    | InputChanged String
    | TaskSubmitted
    | TaskToggled Int Bool
    deriving stock (Eq, Ord, Show)

newTask :: Int -> Text -> Task
newTask taskId content =
    Task
        { taskId = taskId
        , content = content
        , completed = False
        , state = Idle
        }

inputWidget :: Text -> Element
inputWidget inputContent =
    textInput
        [width (Fixed 300), onInput InputChanged, onSubmit TaskSubmitted]
        "Note name"
        (Text.unpack inputContent)

taskListing :: Vector Task -> Element
taskListing tasks =
    let isActive state = if state then style Success else style Primary
        rows =
            Vector.toList $
                fmap (\t -> checkbox [onToggle (TaskToggled t.taskId), isActive t.completed] (Text.unpack t.content) t.completed) tasks
     in column [width Fill, alignX Center, spacing 20] rows

view :: AppState -> Element
view appState =
    let title = text [size 100, color (rgb 0.5 0.5 0.5)] "todos"
     in center [] $
            column
                [width Fill, alignX Center, spacing 10]
                [ title
                , inputWidget appState.input
                , taskListing appState.tasks
                ]

update :: AppMessage -> AppState -> IO (AppState, Command AppMessage)
update message appState =
    case message of
        CreateTask -> do
            traceM "Creating Task"
            error "not implemented"
        InputChanged i -> do
            traceM ("Input: " <> i)
            pure (appState{input = Text.pack i}, Command.none)
        TaskSubmitted -> do
            newId <- Counter.add appState.idCounter 1
            let task = newTask newId appState.input
            let newTasks = Vector.cons task appState.tasks
            traceM $ "Task Submitted: " <> show task
            pure (appState{input = Text.empty, tasks = newTasks}, Command.none)
        TaskToggled taskId toggleState -> do
            traceM $ "Task Toggled: " <> show taskId
            let newTasks = fmap (\t -> if t.taskId == taskId then t{completed = toggleState} else t) appState.tasks
            pure (appState{tasks = newTasks}, Command.none)

main :: IO ()
main = do
    idCounter <- Counter.new 0
    let appState = AppState Text.empty Vector.empty idCounter
    Iced.run [] "Todos" appState update view
