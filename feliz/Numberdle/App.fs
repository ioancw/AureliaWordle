module Numberdle.App

open Elmish
open Elmish.React

Shell.program Rules.game Views.gameView
|> Program.withReactSynchronous "app"
|> Program.run
