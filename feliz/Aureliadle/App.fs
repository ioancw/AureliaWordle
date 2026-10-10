module Aureliadle.App

open Elmish
open Elmish.React

// The previous version kept the high contrast setting under its own key: carry it over once.
try
    let storage = Browser.WebStorage.localStorage
    let key = Engine.highContrastKey Rules.game

    match storage.getItem key, storage.getItem "aureliaHighContrast" with
    | null, null -> ()
    | null, old -> storage.setItem (key, old)
    | _ -> ()
with _ ->
    ()

Shell.program Rules.game Views.gameView
|> Program.withReactSynchronous "app"
|> Program.run
