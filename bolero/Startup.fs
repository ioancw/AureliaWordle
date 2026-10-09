module Aureliadle.Program

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

[<EntryPoint>]
let Main args =
    let builder = WebAssemblyHostBuilder.CreateDefault(args)
    builder.RootComponents.Add<Main.App>("#main")
    builder.Build().RunAsync() |> ignore
    0
