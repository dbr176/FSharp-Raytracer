namespace Raytracer

open Raytracer.VectorsNumerics
open Raytracer.VectorsNumerics.Vec3.Operations

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Nullable =
    open System

    let inline map (f : _ -> 'b) (x : Nullable<'a>) : 'b Option =
        if x.HasValue then f x.Value |> Some else None
        

module Utils =
    let inline degToRad (f : float32) =  float32 System.Math.PI / 180.0f * f

    let epsilon = 0.00001f

    let inline fabs x = if x > 0.0f then x else -x

    let rand = System.Random()
    let inline rone () = (rand.NextDouble() - 0.5) * 2.0 |> float32

    /// Returns random normalized vec3.
    let inline random3 () = vec3(rone (), rone(), rone()) |> Vec3.normalize
    /// Returns random normalized vec4.
    let inline random4 () = vec4(rone (), rone(), rone(), rone()) |> Vec4.normalize
    /// Returns true when cos between normal and viewVector is greater than zero
    let inline needSlice normal viewVector = (normal |*| viewVector) >= 0.0f

    let barTest (r : Ray) (v1 : vec3 byref) (v2 : vec3 byref) (v3 : vec3 byref) : vec3 System.Nullable =
        let d = r.Direction
        let e1 = v2 - v1
        let e2 = v3 - v1
        let p = d <*> e2
        let det = e1 |*| p
        
        if fabs det < epsilon then System.Nullable<vec3>()
        else
            let invDet = 1.0f / det
            let t = r.Origin - v1
            let u = (t |*| p) * invDet

            if (u < 0.0f || u > 1.0f) then System.Nullable<vec3>()
            else
                let q = t <*> e1
                let v = d |*| q * invDet
                if v < 0.0f || u + v > 1.0f then System.Nullable<vec3>()
                else
                    let t = e2 |*| q * invDet
                    if t > epsilon then
                        System.Nullable<vec3>(t * r.Direction + r.Origin)
                    else
                        System.Nullable<vec3>()
   
    /// Returns normal of the triangle
    let inline normal p1 p2 p3 = 
        let e1 = p2 - p1
        let e2 = p3 - p1
        (e1 <*> e2) |> Vec3.normalize
     

