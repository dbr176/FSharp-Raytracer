namespace Raytracer

open Raytracer.VectorsNumerics

open Raytracer.VectorsNumerics.Vec3.Operations
   
open Utils
open System.IO

open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open PixelMapSharp

type Raytracer(cam : Camera, sc : Scene) =
    
    let shadowRay (p : vec3) =
        let intersected r = sc.Meshes

        let inline f (x : Light) =
            let pos = x.Position
            let colls = Physics.findCollisionScene 
                            (intersected { Origin = p; Direction = Vec3.normalize (pos - p)})
                            { Origin = p; Direction = (pos - p) |> Vec3.normalize }
            x, colls

        sc.Light 
        |> Array.choose (fun x ->
            let x, c = f x
            if 
                (Option.isNone c || not x.CastShadows) 
                && (x.IsVisible <| { Origin = p; Direction = (x.Position - p) |> Vec3.normalize })
            then
                Some (x.Intencity p)
            else None)
        |> Array.toList

    let rec cast depth (r : Ray) =
        let intersected = sc.Meshes
        let cols = Physics.findCollisionScene intersected r
        match cols with
        | None -> Color.black
        | Some (struct (m, c)) ->
            let f = c.Face
            let pnt = c.Point
            let normal = Mesh.faceNormal' m.Mesh f.IX f.IY f.IZ f.N
            let col = Collision(r, pnt, normal)
            let rays = 
                if depth > 0  then
                    (m.Material.Reflect <| col) |> List.map (cast <| depth - 1)
                else []
            let clrs = rays
            let shd  = shadowRay (pnt - normal * 0.0001f) |> List.collect Option.toList
            m.Material.Blend col clrs shd
      
        
    member this.Begin depth (m : PixelMap) =
        let r =
            Parallel.For(0, int cam.Width, fun i ->
                for j = 0 to int cam.Height - 1 do
                    let color = cast depth <| cam.GetRay (float32 i) (float32 j)
                    m.[i,j] <- Pixel(color.AsARGB()))
        while not <| r.IsCompleted do ()

    member this.BeginInterpolated depth (m : PixelMap) =
        let mp = Array2D.zeroCreate m.Width m.Height
        let r =
            Parallel.For(0, int cam.Width, fun i ->
                let tk = i % 2
                for j = 0 to int cam.Height - 1 do
                    if (j + tk) % 2 = 0 then
                        let color = cast depth <| cam.GetRay (float32 i) (float32 j)
                        mp.[i,j] <- color
                )
        while not <| r.IsCompleted do ()

        let getneib i j =
            if i < 0 || j < 0 || i >= m.Width || j >= m.Height then
                None
            else Some mp.[i,j]

        let allNeibs i j = [getneib (i + 1) j; getneib (i - 1) j; getneib i (j + 1); getneib i (j - 1)]

        let r1 =
            Parallel.For(0, int cam.Width, fun i ->
                    let tk = i % 2
                    for j = 0 to int cam.Height - 1 do
                        if (j + tk) % 2 <> 0 then
                            let neibs = allNeibs i j |> List.filter Option.isSome |> List.map Option.get
                            let color = 
                                let l = float32 neibs.Length
                                if neibs.Length > 2 then
                                    let [a;b] = neibs |> List.take 2
                                    if fabs (a.R - b.R) > 0.2f ||
                                       fabs (a.G - b.G) > 0.2f ||
                                       fabs (a.B - b.B) > 0.2f
                                    then
                                        let ray = cam.GetRay (float32 i) (float32 j)
                                        cast 1 ray
                                    else 
                                        (neibs |> List.sum) * (1.0f / l)
                                else 
                                    (neibs |> List.sum) * (1.0f / l)
                            m.[i,j] <- Pixel(color.AsARGB())
                        else 
                            m.[i,j] <- Pixel(mp.[i,j].AsARGB())
                )
        while not <| r1.IsCompleted do ()
    
    member this.BeginInterpolatedSmooth depth smooth (randomize : float32) maxDiff (m : PixelMap) =
        let mp = Array2D.zeroCreate m.Width m.Height
        let r =
            Parallel.For(0, int cam.Width, fun i ->
                let tk = i % 2
                for j = 0 to int cam.Height - 1 do
                    if (j + tk) % 2 = 0 then
                        let color = cast depth <| cam.GetRay (float32 i) (float32 j)
                        mp.[i,j] <- color
                )
        while not <| r.IsCompleted do ()

        let getneib i j =
            if i < 0 || j < 0 || i >= m.Width || j >= m.Height then
                None
            else Some mp.[i,j]

        let allNeibs i j = [getneib (i + 1) j; getneib (i - 1) j; getneib i (j + 1); getneib i (j - 1)]

        Parallel.For(0, int cam.Width, fun i ->
                let tk = i % 2
                for j = 0 to int cam.Height - 1 do
                    if (j + tk) % 2 <> 0 then
                        let neibs = allNeibs i j |> List.filter Option.isSome |> List.map Option.get
                        let l = float32 neibs.Length
                        mp.[i,j] <- (neibs |> List.sum) * (1.0f / l)
                            //mp.[i,j]
            ) |> ignore

        let trySet i j v f =
            match v with
            | None -> ()
            | Some x -> 
                if i < 0 || j < 0 || i >= m.Width || j >= m.Height 
                    then ()
                    else mp.[i,j] <- f v.Value

        Parallel.For(0, int cam.Width, fun i ->
            for j = 0 to int cam.Height - 1 do
                let neibs = allNeibs i j |> List.filter Option.isSome |> List.map Option.get
                if neibs.Length <> 4 then ()
                else
                    let [right; left; up; down] = neibs
                    let dx = Vec4.abs (right.ColorVector - left.ColorVector)
                    let dy = Vec4.abs (up.ColorVector - down.ColorVector)

                    if dx.X > maxDiff || dx.Y > maxDiff || dx.Z > maxDiff ||
                       dy.X > maxDiff || dy.Y > maxDiff || dy.Z > maxDiff
                    then
                        let ray = cam.GetRay (float32 i) (float32 j)
                        let rays = [1..smooth] |> List.map (fun _ -> { ray with Direction = ray.Direction + random3 () * randomize})
                        let clr = ((rays |> List.map (cast depth) |> List.sum) + mp.[i,j]) * (1.0f / float32 (smooth + 1))
                        mp.[i,j] <- clr
                        ) |> ignore

        let r1 = 
            Parallel.For(0, int cam.Width, 
                fun i ->
                    for j = 0 to int cam.Height - 1 do
                        m.[i,j] <- Pixel(mp.[i,j].AsARGB())
                    )


        while not <| r1.IsCompleted do ()
        
