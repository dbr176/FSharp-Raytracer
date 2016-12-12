namespace Raytracer

open Raytracer.VectorsNumerics

open Raytracer.VectorsNumerics.Vec3.Operations
   
open Utils

module Physics = 
    let mutable cutBackMode = true

    let inline findCollision (r : Ray) (m : Mesh) =
        let es = m.Faces
        let inline testFunction (e : IndexFace) =
            if cutBackMode then
                let normal = Mesh.faceNormal' m e.IX e.IY e.IZ e.N
                if Utils.needSlice -normal r.Direction then
                    None
                else 
                    Utils.barTest r &m.Points.[e.IX] &m.Points.[e.IY] &m.Points.[e.IZ] 
                    |> Nullable.map (fun x -> CollisionWithFace(e,x))
            else
                Utils.barTest r &m.Points.[e.IX] &m.Points.[e.IY] &m.Points.[e.IZ] 
                |> Nullable.map (fun x -> CollisionWithFace(e,x))
        let cols = es |> Array.choose testFunction
        if cols |> Array.isEmpty then None
        else
            cols |> Array.minBy (fun c -> Vec3.distance c.Point r.Origin) |> Some

    let inline findCollisionScene (ms : SceneMesh []) (r : Ray) = 
        let cols = ms |> Array.choose (fun x -> findCollision r x.Mesh |> Option.map (fun y -> x, y))
        if Array.isEmpty cols then None
        else
            cols |> Array.minBy (fun (_,c) -> Vec3.distance r.Origin c.Point) |> Some
        
    let inline intersectsAll (ms : SceneMesh []) (r : Ray) : bool = 
        let cols = ms |> Array.choose (fun x -> findCollision r x.Mesh |> Option.map (fun y -> x, y))
        Array.isEmpty cols |> not

    let inline intersects r = findCollision r >> Option.isSome
        
    let inline findCollisionsBoundingBoxes (ms : (Mesh * SceneMesh) array) (r : Ray) = 
        let cols = 
            ms 
            |> Array.choose (fun (bb,m) -> 
                if intersects r bb then
                    findCollision r m.Mesh |> Option.map (fun y -> m, y)
                else None)
        Array.isEmpty cols |> not

    let getBoundingBox (m : Mesh) =
        let minx = m.Points |> Array.minBy (fun x -> x.X)
        let miny = m.Points |> Array.minBy (fun x -> x.Y)
        let minz = m.Points |> Array.minBy (fun x -> x.Z)
        let maxx = m.Points |> Array.maxBy (fun x -> x.X)
        let maxy = m.Points |> Array.maxBy (fun x -> x.Y)
        let maxz = m.Points |> Array.maxBy (fun x -> x.Z)

        let sx = maxx.X - minx.X
        let sy = maxy.Y - miny.Y
        let sz = maxz.Z - minz.Z

        let pos = vec3 (minx.X, miny.Y, minz.Z)

        let c = Mesh.cube ()
        c |> Mesh.scaleInPlace (vec3(sx, sy, sz))
        c |> Mesh.translateInPlace pos
        c
        