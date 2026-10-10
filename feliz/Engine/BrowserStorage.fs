/// Loading and saving a daily game in the browser's local storage.
module BrowserStorage

open Engine

// Storage can be unavailable or full (e.g. some private browsing modes); games still work without it.
let private tryGet key =
    try
        Browser.WebStorage.localStorage.getItem key |> Option.ofObj
    with _ ->
        None

let private trySet key value =
    try
        Browser.WebStorage.localStorage.setItem (key, value)
    with _ ->
        ()

/// The latest save: in the current format, or else converted from the game's legacy format.
let load game =
    match tryGet (saveKey game) |> Option.bind (fromJson game) with
    | Some saved -> Some saved
    | None ->
        game.Legacy
        |> Option.bind (fun (key, convert) -> tryGet key |> Option.bind (fun json -> convert json (today game)))

let save game saved = trySet (saveKey game) (toJson game saved)

let loadHighContrast game = tryGet (highContrastKey game) = Some "true"

let saveHighContrast game (enabled: bool) =
    trySet (highContrastKey game) (if enabled then "true" else "false")
