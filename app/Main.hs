{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data Todo =
  Todo
    { name :: Text
    , isCompleted :: Bool
    }

setCompleted :: Todo -> Todo
setCompleted td = td {isCompleted = not (isCompleted td)}

data State =
  State
    { todos :: Vector.Vector Todo
    , currentText :: Text
    }

data Event
  = Closed
  | GotTodoInput Text
  | TodoSubmitted
  | TodoToggled Int

initialState' :: State
initialState' = State {todos = mempty, currentText = mempty}

update' :: State -> Event -> Transition State Event
update' state@State {currentText, todos} evt =
  case evt of
    Closed -> Exit
    GotTodoInput t -> Transition state {currentText = t} (pure Nothing)
    TodoSubmitted ->
      let newTodo = Todo {name = currentText, isCompleted = False}
       in Transition
            state {todos = Vector.snoc todos newTodo, currentText = mempty}
            (pure Nothing)
    TodoToggled ix ->
      Transition
        state {todos = vectUpdateAt ix setCompleted todos}
        (pure Nothing)

vectUpdateAt :: Int -> (a -> a) -> Vector.Vector a -> Vector.Vector a
vectUpdateAt idx fn =
  Vector.modify (\v -> MVector.write v idx . fn =<< MVector.read v idx)

view' :: State -> AppView Gtk.Window Event
view' State {currentText, todos} =
  bin
    Gtk.Window
    [#title := "Todo GTK+++", on #deleteEvent (const (True, Closed))]
    (container
       Gtk.Box
       [#orientation := Gtk.OrientationVertical]
       [todoList, newTodoForm])
  where
    todoList :: BoxChild Event
    todoList =
      BoxChild defaultBoxChildProperties {expand = True, fill = True} $
      container
        Gtk.Box
        [#orientation := Gtk.OrientationVertical]
        (Vector.imap todoItem todos)
    -- todoItem :: Int -> Todo -> target Event
    todoItem ix todo =
      bin
        Gtk.CheckButton
        [#active := isCompleted todo, on #toggled (TodoToggled ix)] $
      widget
        Gtk.Label
        [ #label := completedMarkup todo
        , #useMarkup := True
        , #halign := Gtk.AlignStart
        ]
      where
        completedMarkup todo'
          | isCompleted todo' = "<s>" <> name todo' <> "</s>"
          | otherwise = name todo'
    newTodoForm :: BoxChild Event
    newTodoForm =
      widget
        Gtk.Entry
        [ #text := currentText
        , #placeholderText := "What shall we do?"
        , onM #changed (fmap GotTodoInput . Gtk.entryGetText)
        , on #activate TodoSubmitted
        ]

main :: IO ()
main =
  void $
  run
    App
      { view = view'
      , update = update'
      , inputs = []
      , initialState = initialState'
      }
