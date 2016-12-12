namespace Raytracer

open Raytracer.VectorsNumerics

open Raytracer.VectorsNumerics.Vec3.Operations
   
open Utils
open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Light =
    let glow (m : Mesh) dist (color : Color) =
        let c = Mesh.center m
        { new Light(true) with
            override this.Intencity p = 
                let d = c <--> p
                if d > dist then None
                else
                    let c = d / dist
                    Some <| Color(Vec4.lerp Color.black.ColorVector color.ColorVector c)
            override this.IsVisible p =
                Physics.findCollision p m |> Option.isSome
                
        }

    let constOmni color =
        { new Light(true) with
            override this.Intencity p = Some color
            override this.IsVisible _ = true
        }

    let lerpOmni (color : Color) dist =
        { new Light(true) with
            override this.Intencity p = 
                let d = this.Position <--> p
                if d > dist then None
                else
                    let c = 1.0f - d / dist
                    Some <| Color(Vec4.lerp Color.black.ColorVector color.ColorVector c)
            override this.IsVisible _ = true
        }

    let lerpOmniNoise (color : Color) dist noise =
        { new Light(true) with
            override this.Intencity p = 
                let d = this.Position <--> p
                if d > dist then None
                else
                    let c = 1.0f - d / dist
                    Some <|
                            Color(
                                Utils.random4 () * noise +
                                 Vec4.lerp Color.black.ColorVector color.ColorVector c)
            override this.IsVisible _ = true
        }
    
    let noised (l : Light) (noise : float32) =
        let ns = 
            { new Light(l.CastShadows) with
                override this.Intencity p =
                    l.Intencity p |> Option.map (fun x -> x + Color(random4() * noise))
                override this.IsVisible p = l.IsVisible p 
            }
        ns.Position <- l.Position
        ns

    let ambient color =
        { new Light(false) with
            override this.Intencity _ = Some color
            override this.IsVisible _ = true 
        
        }

    let directional vect color =
        { new Light(false) with
            override this.Intencity _ = Some color
            override this.IsVisible r =
                (r.Direction |*| vect) >= 0.0f
        }

    let infSpot dir angl color =
        let rangl = Utils.degToRad angl
        { new Light(true) with
            override this.Intencity _ = Some color
            override this.IsVisible r =
                let nr = Vec3.normalize r.Direction
                let t = (nr |*| dir) |> acos
                -rangl <= t && t <= rangl
        
        }

    let infSoftSpot pos dir angl color =
        let rangl = Utils.degToRad angl
        { new Light(true) with
            override this.Intencity p = 
                let nr = Vec3.normalize (pos - p)
                let t = (nr |*| dir) |> acos
                if fabs t <= angl then
                    let i = 1.0f - (t / rangl)
                    Some (Color.lerp Color.black color i)
                else
                    None

            override this.IsVisible r =
                let nr = Vec3.normalize r.Direction
                let t = (nr |*| dir) |> acos
                -(rangl) <= t && t <= (rangl)
        
        }