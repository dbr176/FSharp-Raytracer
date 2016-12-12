namespace Raytracer

open Raytracer.VectorsNumerics

open Raytracer.VectorsNumerics.Vec3.Operations
   
open Utils
open System.IO

open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open PixelMapSharp

module Main =
    open System
    open System.Windows.Forms
    open System.Drawing.Drawing2D

    type RenderForm() as form =
        inherit Form()

        let w, h = 500, 500
        let pixelMap = new PixelMap(w, h)

        let renderBox = new PictureBox()

        let rec cubePyramid (scene : Scene) mat (start : vec3) (dir : vec3) cnt =
            for j = 0 to cnt - 1 do
                let cube = Mesh.cube ()
                cube |> Mesh.translateInPlace (start + (float32 j * dir))
                scene.AddMesh cube mat
            if cnt > 0 then
                cubePyramid scene mat (start + Vec3.Up + dir * 0.5f) dir (cnt - 1)
        
        let scene = 
            let s = Scene()
            let addLight (l : Light) = s.AddLight l
            let addMesh (m : Mesh) (mat : IMaterial) = s.AddMesh m mat

            let lnoise = 0.001f;

            //cubePyramid s (Material.reflective (Color.silver) 0.1f 0.8f) (Vec3.Zero) (Vec3.Fwd) 30

            let cubes = [|1..6|] |> Array.map (fun _ -> Mesh.cube ())

            let plane = Mesh.planeXZ ()
            let plane2 = Mesh.planeYX ()

            cubes.[0] |> Mesh.scaleInPlace (vec3 (1.0f, 1.0f, 1.0f))
            cubes.[0] |> Mesh.translateInPlace (vec3 (1.0f, -2.0f, 4.0f))

            cubes.[1] |> Mesh.scaleInPlace (vec3 (1.0f, 1.0f, 1.0f))
            cubes.[1] |> Mesh.translateInPlace (vec3 (-1.5f, -2.01f, 4.1f))

            cubes.[2] |> Mesh.scaleInPlace (vec3 (1.0f, 1.0f, 1.0f))
            cubes.[2] |> Mesh.translateInPlace (vec3 (-0.5f, -2.0f, 3.0f))
            cubes.[2] |> Mesh.rotateYInPlace 45.0f

            cubes.[3] |> Mesh.scaleInPlace (vec3 (1.0f, 1.0f, 1.0f))
            cubes.[3] |> Mesh.translateInPlace (vec3 (-0.5f, -1.0f, 3.5f))
            cubes.[3] |> Mesh.rotateYInPlace 20.0f

            cubes.[4] |> Mesh.scaleInPlace (vec3 (1.0f, 1.0f, 1.0f))
            cubes.[4] |> Mesh.translateInPlace (vec3 (-2.5f, -2.01f, 6.1f))
            cubes.[4] |> Mesh.rotateYInPlace 45.0f

            cubes.[5] |> Mesh.scaleInPlace (vec3 (1.0f, 1.0f, 1.0f))
            cubes.[5] |> Mesh.translateInPlace (vec3 (-1.5f, -1.01f, 4.1f))

            plane |> Mesh.translateInPlace (vec3 (-10.0f, -2.0f, -10.0f))
            plane |> Mesh.scaleInPlace (vec3 (100.0f, 1.0f, 100.0f))

            plane2 |> Mesh.translateInPlace (vec3 (-100.0f, 0.0f, 25.0f))
            plane2 |> Mesh.scaleInPlace (vec3 (1000.0f, 1000.0f, 1.0f))
            plane2 |> Mesh.rotateYInPlace 180.0f
            plane2 |> Mesh.rotateZInPlace 45.0f

            let gradient = 
                [
                    Color.black, 0.0f
                    Color.blue, 0.1f
                    Color.green, 0.5f
                    Color.red, 0.9f
                    Color.white, 1.0f
                ]

            addMesh cubes.[0] <| Material.reflective (Color.silver) 0.1f 0.8f
            addMesh cubes.[1] <| Material.reflective (Color.silver) 0.1f 0.8f
            addMesh cubes.[2] <| Material.reflective (Color.steelBlue) 0.1f 0.8f
            addMesh cubes.[3] <| Material.reflective (Color.lightSteelBlue) 0.1f 0.9f
            addMesh cubes.[4] <| Material.reflective (Color.gold) 1.2f 0.2f
            addMesh cubes.[5] <| Material.reflective (Color.gold) 1.2f 0.2f
            addMesh plane <| Material.reflective (Color.orange) 1.0f 1.0f
            addMesh plane2 <| Material.reflective (Color.white) 1.0f 0.2f

            addLight <| Light.ambient (Color.white * 0.1f)
            addLight <| Light.lerpOmni (Color.blue * 0.2f) 12.0f

            let spot = 
                Light.infSoftSpot 
                    (vec3 (1.0f, 10.0f, 4.0f)) 
                    (Vec3.Up) 45.0f
                    (Color.white * 0.8f)
            spot.Position <- vec3 (1.0f, 5.0f, 4.0f)
            addLight (Light.noised spot lnoise)

            let l1 = Light.lerpOmni (Color(vec4 (0.6f, 0.6f, 0.9f, 1.0f)) * 1.5f ) 13.0f
            l1.Position <- vec3(-10.0f, 1.0f, 5.0f)
            addLight (Light.noised l1 lnoise)

            printfn "Triangles: %d" (s.Meshes |> Array.sumBy (fun x -> x.Mesh.Faces.Length))
            printfn "Lights: %d" (s.Light.Length)

            s

        let camera = Camera(vec3.Zero, Vec3.Fwd, Vec3.Right, Vec3.Up,float32 w, float32 h)
        let raytraycer = Raytracer(camera, scene)
        
        let redrawRaytracer _ =
            printfn "%dx%d" pixelMap.Width pixelMap.Height
            let old = System.Runtime.GCSettings.LatencyMode
            System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.SustainedLowLatency
            raytraycer.Begin 2 pixelMap
            renderBox.Image <- pixelMap.GetBitmap()
            renderBox.Invalidate()
            System.Runtime.GCSettings.LatencyMode <- old

        let redraw = redrawRaytracer

        let resize _ = () //redraw ()

        let processKeys (args : KeyEventArgs) =
            redraw ()

        do
            form.Size <- System.Drawing.Size(w,h)
            renderBox.Parent <- form
            renderBox.Dock <- DockStyle.Fill
            renderBox.SizeMode <- PictureBoxSizeMode.Zoom

            form.Resize.Add resize
            form.KeyDown.Add processKeys

            let stopwatch = Stopwatch()
            stopwatch.Start()
            redraw ()
            stopwatch.Stop()
            printfn "%d" (stopwatch.ElapsedMilliseconds)
            printfn "Ready"

    [<EntryPoint>][<STAThread>]
    let main argv = 
        Application.Run (new RenderForm ())
        0 // return an integer exit code
