namespace Raytracer


open Raytracer.VectorsNumerics

open Raytracer.VectorsNumerics.Vec3.Operations
   
open Utils
open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Mesh =
    let clone (m : Mesh) =
        {
            Points = m.Points.Clone() :?> vec3 array
            Faces = m.Faces.Clone() :?> IndexFace array
        }

    let saveMesh (m : Mesh) path =
        use file = File.CreateText(path)
        file.WriteLine(m.Points.Length)
        for p in m.Points do
            file.WriteLine(sprintf "%f;%f;%f" p.X p.Y p.Z)
        for i in m.Faces do
            file.WriteLine
                (sprintf "%d;%d;%d;%f" i.IX i.IY i.IZ i.N)

    let loadMesh path =
        let lines = File.ReadAllLines path
        let asInt = System.Int32.Parse
        let asFloat = System.Single.Parse

        let inline asPoint (s : string) =
            let spl = s.Split ';' |> Array.map asFloat
            vec3 (spl.[0], spl.[1], spl.[2])

        let inline asFace (s : string) =
            let spl = s.Split ';'
            IndexFace(asInt spl.[0], asInt spl.[1], asInt spl.[2], asFloat spl.[3])

        let pcnt = asInt lines.[0]
        let points = lines.[1..pcnt + 1] |> Array.map asPoint
        let faces = lines.[pcnt + 2..] |> Array.map asFace
        { Points = points; Faces = faces}

    let inline triangle a b c dir = IndexFace(a,b,c,dir)
    let inline quad a b c d dir =
        [triangle d a b dir; triangle b c d dir]

    let inline faceNormal (m : Mesh) i =
        let fi = m.Faces.[i]
        let p1 = m.Points.[fi.IX]
        let p2 = m.Points.[fi.IY]
        let p3 = m.Points.[fi.IZ]
        fi.N * Utils.normal p1 p2 p3

    let inline faceNormal' (m : Mesh) pi1 pi2 pi3 n =
        let p1 = m.Points.[pi1]
        let p2 = m.Points.[pi2]
        let p3 = m.Points.[pi3]
        (n * Utils.normal p1 p2 p3) |> Vec3.normalize

    let inline cube () =
        let v x y z = vec3 (x,y,z)
        let dots = 
            [|
                Vec3.Zero // 0
                Vec3.Up // 1
                Vec3.Up + Vec3.Right // 2
                Vec3.Up + Vec3.Fwd // 3
                Vec3.Up + Vec3.Fwd + Vec3.Right // 4
                Vec3.Right // 5
                Vec3.Right + Vec3.Fwd // 6
                Vec3.Fwd // 7
            |]
        let faces = 
            [
                quad 0 5 2 1 1.0f
                quad 0 1 3 7 1.0f
                quad 0 7 6 5 1.0f
                quad 5 6 4 2 1.0f
                quad 7 3 4 6 1.0f
                quad 4 3 1 2 1.0f
            ] |> List.concat |> List.toArray
        { Points = dots; Faces = faces}

    let inline plane start l r =
        let points =  
            [|
                start; 
                start + l; 
                start + l + r
                start + r
            |]
        let faces = quad 0 1 2 3 1.0f |> List.toArray
        { Points = points; Faces = faces }

    let inline planeXZ () = plane Vec3.Zero Vec3.Right Vec3.Fwd
    let inline planeYZ () = plane Vec3.Zero Vec3.Up Vec3.Fwd
    let inline planeYX () = plane Vec3.Zero Vec3.Up Vec3.Right

    let inline center (m : Mesh) =
        (m.Points |> Array.sum) * (1.0f / float32 m.Points.Length)

    let inline transform f (m : Mesh) =
        { m with Points = m.Points |> Array.map f }

    let inline transformPoints f (l : vec3 list) =
        l |> List.map f


    let inline transformInPlace f (m : Mesh) =
        for i = 0 to m.Points.Length - 1 do
            m.Points.[i] <- f m.Points.[i]

    let inline translateInPlace v = transformInPlace (fun x -> x + v)

    let inline centerTransformInPlace f (m : Mesh) =
        let center = center m
        translateInPlace -center m
        transformInPlace f m
        translateInPlace center m

    let inline scaleInPlace (v : vec3) m = 
        centerTransformInPlace (fun x -> vec3 (x.X * v.X, x.Y * v.Y, x.Z * v.Z)) m
    
    let inline rotateXInPlace a =
        let matrix = Matrix4x4.createRotationX <| degToRad a
        centerTransformInPlace (fun v -> Vec3.transform v matrix)

    let inline rotateYInPlace a =
        let matrix = Matrix4x4.createRotationY <| degToRad a
        centerTransformInPlace (fun v -> Vec3.transform v matrix)

    let inline rotateZInPlace a =
        let matrix = Matrix4x4.createRotationZ <| degToRad a
        centerTransformInPlace (fun v -> Vec3.transform v matrix)

