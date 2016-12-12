namespace Raytracer

type SceneMesh(m : Mesh, material : IMaterial) =
    inherit SceneObject()
    member val Material = material with get,set
    member val Mesh     = m with get
    member this.BakeTransform() =
        this.Mesh |> Mesh.rotateXInPlace this.Rotation.X
        this.Mesh |> Mesh.rotateYInPlace this.Rotation.Y
        this.Mesh |> Mesh.rotateZInPlace this.Rotation.Z
        this.Mesh |> Mesh.scaleInPlace this.Scale
        this.Mesh |> Mesh.translateInPlace this.Position

open System.Collections.Generic
type Scene() =
    let mutable meshes : SceneMesh[] = [||] 
    let mutable lights : Light[] = [||]

    member this.Meshes with get () = meshes
    member this.Light with get () = lights

    member this.AddMesh (m : Mesh) (mat : IMaterial) =
        meshes <- meshes |> Array.append [|SceneMesh(m, mat)|]
    member this.AddLight (l : Light) =
        lights <- lights |> Array.append [|l|]


