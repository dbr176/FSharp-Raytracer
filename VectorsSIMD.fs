namespace Raytracer.VectorsSIMD
(*

open NativeInteropEx

type vec4(x : float32, y : float32, z : float32, w : float32) = struct
    member __.arr = 
        let a =
            NativeArray.SIMD.zeroCreate 4
        a.[0] <- x
        a.[1] <- y
        a.[2] <- z
        a.[3] <- w
        a

    member __.X with get () = __.arr.[0]
    member __.Y with get () = __.arr.[1]
    member __.Z with get () = __.arr.[2]
    member __.W with get () = __.arr.[3]

    static member (+) (v1 : vec4, v2 : vec4) =
        vec4(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z, v1.W + v2.W)
    static member (-) (v1 : vec4, v2 : vec4) =
        vec4(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W)
    static member (*) (m : float32, v2 : vec4) =
        vec4(v2.X * m, v2.Y * m, v2.Z * m, v2.W * m)
    static member (*) (v1 : vec4, m : float32) = m * v1

    static member Zero = vec4 (0.0f, 0.0f, 0.0f, 0.0f)
end 

type vec3(iarr : NativeArray<float32>) = struct
    new (x : float32, y : float32, z : float32) =
        let a = NativeArray.SIMD.zeroCreate 3
        a.[0] <- x
        a.[1] <- y
        a.[2] <- z
        vec3(a)

    member __.arr = iarr

    member __.X with get () = iarr.[0]
    member __.Y with get () = iarr.[1]
    member __.Z with get () = iarr.[2]
    member __.W with get () = iarr.[3]

    static member (+) (v1 : vec3, v2 : vec3) =
        vec3(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)

    static member (-) (v1 : vec3, v2 : vec3) =
        vec3(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)

    static member (~-) (v1 : vec3) =
        vec3(-v1.X, -v1.Y, -v1.Z)

    static member (*) (m : float32, v2 : vec3) =
        vec3(v2.X * m, v2.Y * m, v2.Z * m)

    static member (*) (v1 : vec3, m : float32) = m * v1

    static member Zero = vec3 (0.0f, 0.0f, 0.0f)
end 

type vec2(x : float32, y : float32) = struct
    member __.arr = 
        let a =
            NativeArray.SIMD.zeroCreate 2
        a.[0] <- x
        a.[1] <- y
        a

    member __.X with get () = __.arr.[0]
    member __.Y with get () = __.arr.[1]
end 

type mat4x4(
            x11 : float32, x12 : float32, x13 : float32, x14 : float32,
            x21 : float32, x22 : float32, x23 : float32, x24 : float32,
            x31 : float32, x32 : float32, x33 : float32, x34 : float32,
            x41 : float32, x42 : float32, x43 : float32, x44 : float32) 
        = struct
    member private __.arr =
        let a = NativeArray.init 4 (fun _ -> 0.0f)
        a.[0] <- x11
        a.[1] <- x12
        a.[2] <- x13
        a.[3] <- x14

        a.[4] <- x21
        a.[5] <- x22
        a.[6] <- x23
        a.[7] <- x24

        a.[8] <- x31
        a.[9] <- x32
        a.[10] <- x33
        a.[11] <- x34

        a.[12] <- x41
        a.[13] <- x42
        a.[14] <- x43
        a.[15] <- x44
        a

    member __.Item 
        with get (i, j) =
            __.arr.[i % 2 + j]

end

module Matrix4x4 =
    let createRotationX a =
        let sina = sin a
        let cosa = cos a
        mat4x4 (1.0f, 0.0f, 0.0f, 0.0f,
                0.0f, cosa,-sina, 0.0f,
                0.0f, sina, cosa, 0.0f,
                0.0f, 0.0f, 0.0f, 1.0f)

    let createRotationY a = 
        let sina = sin a
        let cosa = cos a
        mat4x4 (cosa, 0.0f, sina, 0.0f,
                0.0f, 1.0f, 0.0f, 0.0f,
                -sina,0.0f, cosa, 0.0f,
                0.0f, 0.0f, 0.0f, 1.0f)

    let createRotationZ a = 
        let sina = sin a
        let cosa = cos a
        mat4x4 (cosa,-sina, 0.0f, 0.0f,
                sina, cosa, 0.0f, 0.0f,
                0.0f, 0.0f, 1.0f, 0.0f,
                0.0f, 0.0f, 0.0f, 1.0f)
    

module Vec4 =
    let inline lerp (v1 : vec4) (v2 : vec4) t = 
        let mt = 1.0f - t
        v1 * mt + v2 * t

    
    let inline normalize (c : vec4) = 
        let invlen = 1.0f / (c.X * c.X + c.Y * c.Y + c.Z * c.Z + c.W * c.W)
        c * invlen

    let Zero = vec4 (0.0f, 0.0f, 0.0f, 0.0f)

module Vec3 =
    let inline dot (v1 : vec3) (v2 : vec3) = NativeArray.SIMD.dot v1.arr v2.arr

    let inline normalize (c : vec3) = 
        let invlen = 1.0f / dot c c
        c * invlen
    
    let inline cross (a : vec3) (b : vec3) = 
        vec3 (
            a.Y * b.Z - a.Z * b.Y,
            a.Z * b.X - a.X * b.Z,
            a.X * b.Y - a.Y * b.X
        )

    let inline sqrDistance (v1 : vec3) (v2 : vec3) = 
        let m = (v2 - v1)
        dot m m

    let inline distance v1 v2 = 
        sqrDistance v1 v2 |> sqrt

    

    let inline reflect d normal = 
        d - (2.0f * (dot normal d)) * normal

    let inline lerp (v1 : vec3) (v2 : vec3) t = 
        let mt = 1.0f - t
        v1 * mt + v2 * t

    let inline magnitude (v : vec3) = (dot v v) |> sqrt

    let inline transform (pos : vec3) (mat : mat4x4) = 
        let inline mulOnCol j =
            pos.X * mat.[0, j] + 
            pos.Y * mat.[1, j] +
            pos.Z * mat.[2, j] +
                    mat.[3, j]
        let x = mulOnCol 0
        let y = mulOnCol 1
        let z = mulOnCol 2
        vec3 (x,y,z)

    module Operations = 
        let inline (<*>) v1 v2 = cross v1 v2
        let inline (|*|) v1 v2 = dot v1 v2
        let inline (<-->) v1 v2 = distance v1 v2

    let Up = vec3 (0.0f, 1.0f, 0.0f)
    let Right = vec3 (1.0f, 0.0f, 0.0f)
    let Fwd = vec3 (0.0f, 0.0f, 1.0f)
    let Zero = vec3 (0.0f, 0.0f, 0.0f)

*)