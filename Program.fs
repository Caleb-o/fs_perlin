open SFML.Graphics
open SFML.Window

let width = 1080ul
let height = 720ul
let fpsCap = 24ul

let minOctaves = 1
let maxOctaves = 8
let freqInc = 0.1
let minFrequency = 0.1
let maxFrequency = 20.0

let createMap frequency octaves reset =
    let mapBytes = Array.create (4 * int(width * height)) 0uy

    if reset then
        Perlin.init()

    for pixel_idx in 0..(int (width * height) - 1) do (
        let x = pixel_idx % int (width)
        let y = pixel_idx / int (width)

        let xx = float(x) * frequency
        let yy = float(y) * frequency

        let noise = Perlin.noise2dWithOctaves xx yy octaves;
        let adjusted_noise = int (((noise + 1.0) * 0.5) * 255.0)
        let rgb = byte (System.Math.Clamp(adjusted_noise, 0, 255))

        // RGBA
        Array.set mapBytes ((pixel_idx * 4) + 0) rgb
        Array.set mapBytes ((pixel_idx * 4) + 1) rgb
        Array.set mapBytes ((pixel_idx * 4) + 2) rgb
        Array.set mapBytes ((pixel_idx * 4) + 3) 255uy
    )

    let tex = new Texture(width, height)
    tex.Update(mapBytes)
    tex


let updateWindowTitle (window: RenderWindow) frequency octaves =
    window.SetTitle((sprintf "Perlin Noise Map | Frequency %.1f | Octaves %d" frequency octaves))


[<EntryPoint>]
let main _argv =
    let mutable frequency = 5.0
    let mutable octaves = 4

    let map = new Sprite((createMap frequency 1 true))

    let window = new RenderWindow(new VideoMode(width, height), "N/A")
    window.SetFramerateLimit(fpsCap)
    // :?> - Downcast (Run-time)
    // :> - Upcast (Compile-time)
    window.Closed.AddHandler(fun sender _ -> (sender :?> RenderWindow).Close())
    window.KeyReleased.AddHandler(fun _ args ->
        match args.Code with
        | Keyboard.Key.R ->
            window.SetTitle("Loading map...")
            map.Texture <- (createMap frequency octaves true)
            updateWindowTitle window frequency octaves
            ()
        | Keyboard.Key.F ->
            window.SetTitle("Reloading map...")
            map.Texture <- (createMap frequency octaves false)
            updateWindowTitle window frequency octaves
            ()
        | Keyboard.Key.A ->
            octaves <- System.Math.Clamp((octaves - 1), minOctaves, maxOctaves)
            updateWindowTitle window frequency octaves
        | Keyboard.Key.D ->
            octaves <- System.Math.Clamp((octaves + 1), minOctaves, maxOctaves)
            updateWindowTitle window frequency octaves
        | Keyboard.Key.Escape ->
            window.Close()
        | _ -> ()
    )
    window.KeyPressed.AddHandler(fun _ args ->
        match args.Code with
        | Keyboard.Key.Q ->
            frequency <- System.Math.Clamp((frequency - freqInc), minFrequency, maxFrequency)
            updateWindowTitle window frequency octaves
        | Keyboard.Key.E ->
            frequency <- System.Math.Clamp((frequency + freqInc), minFrequency, maxFrequency)
            updateWindowTitle window frequency octaves
        | _ -> ()
    )

    updateWindowTitle window frequency octaves

    let rec mainLoop() =
        window.DispatchEvents()

        window.Clear()
        window.Draw(map)
        window.Display()

        match window.IsOpen with
        | false -> ()
        | true -> mainLoop()
    
    mainLoop()

    0