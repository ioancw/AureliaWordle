module WhichWitch.App

open Elmish
open Elmish.React

Shell.program Rules.game Views.gameView
|> Program.withReactSynchronous "app"
|> Program.run
