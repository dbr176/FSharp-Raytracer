namespace Raytracer.VectorsNumerics

open System.Numerics

/// Represents a vector with four float32 values.
type vec4 = Vector4
/// Represents a vector with three float32 values.
type vec3 = Vector3
/// Represents a vector with two float32 values.
type vec2 = Vector2
/// Represents a 4x4 matrix.
type mat4x4 = Matrix4x4
/// Represents a plane.
type plane = Plane

/// Funcations for work with planes.
module Plane = 
    /// Function checks the point is to the right of the plane.
    let inline isRight (p : plane) (pnt : vec3) =
        let c = p.Normal * p.D
        let k = (pnt - c) |> Vector3.Normalize
        Vector3.Dot(c, k) >= 0.0f
    /// Function checks the point is to the right of the plane.
    let inline isLeft p pnt = not <| isRight p pnt


module Matrix4x4 =
    let createRotationX a = Matrix4x4.CreateRotationX a
    let createRotationY a = Matrix4x4.CreateRotationY a
    let createRotationZ a = Matrix4x4.CreateRotationZ a

module Vec4 =
    let inline lerp v1 v2 t = Vector4.Lerp (v1, v2, t)

    let inline abs v = Vector4.Abs v
    
    let inline normalize c = Vector4.Normalize c

    let Zero = Vector4.Zero

module Vec3 =
    let inline reflect v normal = Vector3.Reflect(v, normal)
    
    let inline normalize v = Vector3.Normalize v
    
    let inline vabs v = Vector3.Abs v
    
    let inline clamp v min max = Vector3.Clamp(v, min, max)
    
    let inline cross v1 v2 = Vector3.Cross(v1, v2)

    let inline distance v1 v2 = Vector3.Distance(v1, v2)

    let inline sqrDistance v1 v2 = Vector3.DistanceSquared(v1, v2)

    let inline dot v1 v2 = Vector3.Dot(v1, v2)

    let inline lerp v1 v2 t = Vector3.Lerp(v1, v2, t)

    let inline magnitude (v : Vector3) = (dot v v) |> sqrt

    let inline transform pos (mat : Matrix4x4) = Vector3.Transform(pos, mat)

    module Operations = 
        let inline (<*>) v1 v2 = cross v1 v2
        let inline (|*|) v1 v2 = dot v1 v2
        let inline (<-->) v1 v2 = distance v1 v2

    let Up = Vector3.UnitY
    let Right = Vector3.UnitX
    let Fwd = Vector3.UnitZ
    let Zero = Vector3.Zero