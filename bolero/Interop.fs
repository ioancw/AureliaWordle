/// Calls into wwwroot/aureliadle.js for browser access WebAssembly doesn't have directly.
module Aureliadle.Interop

open System
open Microsoft.JSInterop

// In the browser, Blazor WebAssembly can call JavaScript synchronously, which keeps reading
// and writing storage as simple as in the Fable version.
let private inProcess (js: IJSRuntime) = js :?> IJSInProcessRuntime

let private getItem js (key: string) =
    (inProcess js).Invoke<string>("aureliadle.get", key) |> Option.ofObj

let private setItem js (key: string) (value: string) =
    (inProcess js).InvokeVoid("aureliadle.set", key, value)

/// The latest saved game, or None if there isn't one or it can't be read.
let latestSave js =
    getItem js Storage.gameKey |> Option.bind Storage.fromJson

let saveGame js state =
    setItem js Storage.gameKey (Storage.toJson (Game.toSaved state))

let loadHighContrast js = getItem js Storage.highContrastKey = Some "true"

let saveHighContrast js (enabled: bool) =
    setItem js Storage.highContrastKey (if enabled then "true" else "false")

let blurActive js = (inProcess js).InvokeVoid("aureliadle.blurActive")

/// Shares the text, returning "shared", "copied", "cancelled" or "failed".
let share (js: IJSRuntime) (text: string) =
    js.InvokeAsync<string>("aureliadle.share", text).AsTask()

/// Receives browser events from aureliadle.js.
type BrowserEvents(onKey: string -> unit, onStorageChanged: unit -> unit) =
    [<JSInvokable>]
    member _.OnKey(key: string) = onKey key

    [<JSInvokable>]
    member _.OnStorageChanged() = onStorageChanged ()

/// Starts listening for key presses and storage changes; dispose to stop.
let listen (js: IJSRuntime) onKey onStorageChanged =
    let events = DotNetObjectReference.Create(BrowserEvents(onKey, onStorageChanged))
    (inProcess js).InvokeVoid("aureliadle.listen", events)

    { new IDisposable with
        member _.Dispose() =
            (inProcess js).InvokeVoid("aureliadle.unlisten")
            events.Dispose() }
