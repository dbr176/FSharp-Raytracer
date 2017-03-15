namespace Raytracer

open Raytracer.VectorsNumerics

[<Struct>]
type Ray = {
    Origin : vec3
    Direction : vec3
}
and public IndexFace(ix : int, iy : int, iz : int, normalDir : float32) = struct
    member __.IX = ix
    member __.IY = iy
    member __.IZ = iz
    member __.N = normalDir
end 
and 
    [<Struct>]
    public CollisionWithFace(fc : IndexFace, pnt : vec3) = struct
    member __.Face = fc
    member __.Point = pnt
end
and Mesh = {
    Points : vec3 []
    Faces  : IndexFace []
}
and public Color(colorVector : vec4) = struct
        member t.ColorVector = colorVector
        member t.R = t.ColorVector.X
        member t.G = t.ColorVector.Y
        member t.B = t.ColorVector.Z
        member t.A = t.ColorVector.W

        member this.AsARGB() =
            let toByte x = ((max 0.0f x |> min 1.0f) * 255.0f) |> int
            System.Drawing.Color.FromArgb(toByte this.R, toByte this.G, toByte this.B)

        static member (+) (c1 : Color, c2 : Color) =    
            Color(c1.ColorVector + c2.ColorVector)
        static member (*) (c1 : float32, c2 : Color) =    
            Color(c1 * c2.ColorVector)
        static member (*) (c2 : Color, c1 : float32) = c1 * c2 
        static member Zero = Color(Vec4.Zero)
    end
and SceneObject() =
    member val Position = vec3.Zero with get, set
    member val Rotation = vec3.Zero with get, set
    member val Scale    = vec3.Zero with get, set
and Collision(ray : Ray, point : vec3, normal : vec3) =
    member val Point = point with get
    member val Normal = normal with get
    member val Ray = ray with get
and IMaterial =
    abstract member Reflect : Collision -> Ray list
    abstract member Blend : Collision -> Color list -> Color list -> Color
and KDTree =
    | Node of KDTreeNode
    | Leaf of (Mesh * IndexFace ref * IMaterial) array
and KDTreeNode = {
        SepPlane : plane
        Left : KDTree
        Right : KDTree
    }

[<AbstractClass>]
type Light(castShadows : bool) =
    inherit SceneObject ()
    abstract member Intencity : point:vec3 -> Color Option
    member this.CastShadows = castShadows
    abstract member IsVisible : r:Ray -> bool

type Camera (position : vec3, fwd : vec3, right : vec3, up : vec3, w, h) =
    let recenterX x = (x - w / 2.0f) / (2.0f * w)
    let recenterY y = -(y - h / 2.0f) / (2.0f * h)

    member this.Width = w
    member this.Height = h

    member this.Position = position
    member this.Right = right
    member this.Up = up
    member this.Forward = fwd

    member this.GetRay x y =
        let x = (x / float32 w - 0.5f) * 2.0f * right
        let y = -(y / float32 h - 0.5f) * 2.0f * up

        { Origin = position; Direction = ((x + y + fwd) - position) |> Vec3.normalize}




