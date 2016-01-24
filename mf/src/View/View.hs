module View.View where

class View a where
  view :: a -> String