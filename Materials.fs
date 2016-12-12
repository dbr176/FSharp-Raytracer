namespace Raytracer

open Raytracer.VectorsNumerics

open Raytracer.VectorsNumerics.Vec3.Operations
   
open Utils
open System.IO

module Material =
    let inline gradient (reduce : Color) (colors : (Color * float32) list) =
        let findColor (intens : float32) = 
            colors 
            |> List.pairwise 
            |> List.tryFind (fun ((_,a),(_,b)) -> a <= intens && intens <= b)
            |> Option.map 
                (fun ((c1, a), (c2, b)) -> 
                    let t = (intens - a) / (b - a)
                    Color.lerp c1 c2 t)
        { new IMaterial 
            with
                member x.Reflect _ = []
                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = (clrs |> List.sum) //* (1.0f / float32 clrs.Length)
                    let intens = max (sum.R * reduce.R) (sum.G * reduce.G) |> max (sum.B * reduce.B)
                    let res = findColor intens
                    if res.IsSome then res.Value else Color.black
        }


    let inline constColor (color : Color) =
        { new IMaterial 
            with
                member x.Reflect _ = []
                member x.Blend _ _ _ = color
                       
        }

    let inline norefl (color : Color) =
        { new IMaterial 
            with
                member x.Reflect _ = []
                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = clrs |> List.sumBy (fun c -> c.ColorVector)
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))
                       
        }

    let inline reflective (color : Color) refl lght =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n //
                    [{Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r}]
                member x.Blend _ (clrs : Color list) (shd : Color list) =
                    let clrs = (clrs |> List.map (fun x -> x * refl)) @ (shd |> List.map (fun x -> x * lght))
                    let sum = clrs |> List.sumBy (fun c -> c.ColorVector)
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))
                       
        }

    let inline simple (color : Color) =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n //
                    [{Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r}]
                member x.Blend _ (clrs : Color list) (shd : Color list) =
                    let clrs = clrs @ shd
                    let sum = clrs |> List.sumBy (fun c -> c.ColorVector)
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))
                       
        }

    let inline simpleConst (refl : Color) (baseColor : Color) =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n //
                    [{Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r}]
                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = clrs |> List.sumBy (fun c -> c.ColorVector)
                    baseColor + 
                        Color(
                            vec4 (refl.R * sum.X, refl.G * sum.Y, refl.B * sum.Z, 1.0f))
                       
        }

    let inline sendBack (color : Color) =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    [{Origin = col.Point - col.Normal * 0.0001f; Direction = d}]
                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = clrs |> List.sumBy (fun c -> c.ColorVector)
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))
                       
        }

    let inline simpleRandRefl (color : Color) noise =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n + Utils.random3() * noise
                    [{Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r}]
                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = clrs |> List.sumBy (fun c -> c.ColorVector)
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))
                       
        }

    let inline simpleRandReflN (color : Color) noise count (sm : float32) m =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n + Utils.random3() * noise //
                    [1..count] |> List.map (fun _ -> {Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r})
                member x.Blend _ (clrs : Color list) shd =
                    let clr = (clrs |> List.sum) * sm + (shd |> List.sum) * m
                    Color(vec4 (color.R * clr.R, color.G * clr.G, color.B * clr.B, 1.0f))
                       
        }

    let inline cosRefl (color : Color) =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n //
                    [{Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r}]
                member x.Blend (col) (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let t = sqrt (col.Point.X**2.0f + col.Point.Y**2.0f)
                    let a = (cos t + sin t + 2.0f) / 2.0f
                    let sum = a * (clrs |> List.sumBy (fun c -> c.ColorVector))
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))
                       
        }

    let inline refraction (color : Color) refrf =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction

                    let n = if (col.Normal |*| d) <= 0.0f then -col.Normal else col.Normal
                    let r = Vec3.reflect d n

                    let a = n |*| -d
                    let refr = (refrf * a - sqrt (1.0f - refrf * refrf * a * a)) * n - refrf * -d

                    [//{Origin = col.Point - n * 0.0001f; Direction = normalize r}; 
                     {Origin = col.Point + n * 0.0001f; Direction = Vec3.normalize refr}]

                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = (clrs |> List.sumBy (fun c -> c.ColorVector)) //* 0.5f
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))
                       
        }
    let inline shining (color : Color) (shine : Color) =
        { new IMaterial 

            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n //
                    [{Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r}]

                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = (clrs |> List.sum) + shine
                    Color(vec4 (color.R * sum.R, color.G * sum.G, color.B * sum.B, 1.0f))
                       
        }

    let inline checker () =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n //
                    [{Origin = col.Point - n * 0.0001f; Direction = Vec3.normalize r}]
                member x.Blend col (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = clrs |> List.sumBy (fun c -> c.ColorVector)
                    let color = 
                        let n = col.Point|> Vec3.normalize 
                        if ((n.X * 2.0f |> ceil |> int) % 2 = (n.Y * 2.0f |> ceil |> int) % 2)
                            then Color.black
                            else Color.white
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f))          
        }


    let inline simpleNoise (color : Color) (n : float32) =
        { new IMaterial 
            with
                member x.Reflect (col : Collision) =
                    let d = col.Ray.Direction
                    let n = col.Normal
                    let r = Vec3.reflect d n
                    [{Origin = col.Ray.Origin + n; Direction = Vec3.normalize r}]

                member x.Blend _ (clrs : Color list) shd =
                    let clrs = clrs @ shd
                    let sum = 
                        clrs 
                        |> List.sumBy (fun c -> c.ColorVector)
                    Color(vec4 (color.R * sum.X, color.G * sum.Y, color.B * sum.Z, 1.0f) + random4 () * n)
                       
        }