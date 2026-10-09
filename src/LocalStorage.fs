/// Reading and writing the browser's local storage, for the Fable (Lit) front end.
module LocalStorage

open Storage

// Storage can be unavailable or full (e.g. some private browsing modes); the game still works without it.
let private tryGetItem key =
    try
        Browser.WebStorage.localStorage.getItem key |> Option.ofObj
    with _ ->
        None

let private trySetItem key value =
    try
        Browser.WebStorage.localStorage.setItem (key, value)
    with _ ->
        ()

let private latestSave () = tryGetItem gameKey |> Option.bind fromJson

/// Today's game, resumed from local storage where possible.
let loadGame () = Game.today (latestSave ())

let saveGame state = trySetItem gameKey (toJson (Game.toSaved state))

/// See Game.refresh.
let refreshGame current = Game.refresh (latestSave ()) current

let loadHighContrast () = tryGetItem highContrastKey = Some "true"

let saveHighContrast (enabled: bool) =
    trySetItem highContrastKey (if enabled then "true" else "false")
