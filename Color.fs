namespace Raytracer

open Raytracer.VectorsNumerics

open Raytracer.VectorsNumerics.Vec3.Operations
   
open Utils
open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Color =
    let inline fromARGB a r g b =
        Color(
            vec4 (
                     float32 r / 255.0f,
                     float32 g / 255.0f,
                     float32 b / 255.0f,
                     float32 a / 255.0f))
    let inline fromRGB r g b =
        Color(
            vec4 (
                     float32 r / 255.0f,
                     float32 g / 255.0f,
                     float32 b / 255.0f,
                     1.0f))
    
    let inline fromSystemColor (c : System.Drawing.Color) =
        fromARGB c.A c.R c.G c.B

    let inline lerp (c1 : Color) (c2 : Color) i =
        Color(Vec4.lerp c1.ColorVector c2.ColorVector i)

    let black = Color(vec4 (0.0f, 0.0f, 0.0f, 1.0f))
    let white = Color(vec4 (1.0f, 1.0f, 1.0f, 1.0f))
    let red   = Color(vec4 (1.0f, 0.0f, 0.0f, 1.0f))
    let green = Color(vec4 (0.0f, 1.0f, 0.0f, 1.0f))
    let blue  = Color(vec4 (0.0f, 0.0f, 1.0f, 1.0f))
    let yellow = fromRGB 255 255 0
    let orange = fromSystemColor (System.Drawing.Color.Orange)
    let richBlack = fromRGB 0 64 64
    let darkGreen = fromRGB 0 100 0
    let lightGray = fromRGB 211 211 211
    let lightSteelBlue =  fromRGB 176 196 222
    let silver = fromRGB 192 192 192
    let lavender = fromRGB 230 230 250
    let lightBlue = fromRGB 173 216 230
    let maroon = fromRGB 128 0 0
    let powderBlue = fromRGB 176 224 230
    let steelBlue = fromRGB 70 130 180
    let gold = fromRGB 255 215 0

