module Perlin

open System
open System.Collections.Generic

type Vec2 = {
    x : float
    y : float
}

let dot vecA vecB =
  vecA.x * vecB.x + vecA.y * vecB.y

let mutable seed = 0
let mutable offset = (0, 0)

let makePermutation () =
    let rand = Random(seed)
    let rec make_perm idx (l: List<int>) =
        if idx < l.Capacity / 2 then
            l.Add(rand.Next() % 255)
            make_perm (idx + 1) l;
        else l;

    let l = make_perm 0 (new List<int>(512))
    l.AddRange(l.GetRange(0, (l.Capacity / 2)))
    l.ToArray()


let permutation = makePermutation ()

let init () =
    seed <- DateTime.Now.Millisecond

    let rand = Random(seed)
    offset <- (rand.Next(), rand.Next())


let getConstVec v =
    match (v &&& 3) with
    | 0 -> { x = 1.0; y = 1.0 }
    | 1 -> { x = -1.0; y = 1.0 }
    | 2 -> { x = -1.0; y = -1.0 }
    | _ -> { x = 1.0; y = -1.0 }

let fade t =
    ((6.0 * t - 15.0) * t + 10.0) * t * t * t

let lerp t a b =
    a + t * (b - a)

let noise2d x y =
    let offsetx, offsety = offset
    let xx = (int (floor x) + offsetx) &&& 255
    let yy = (int (floor y) + offsety) &&& 255
    let xf = x - (floor x)
    let yf = y - (floor y)
    let topRight = { x = xf - 1.0; y = yf - 1.0 }
    let topLeft = { x = xf; y = yf - 1.0 }
    let bottomRight = { x = xf - 1.0; y = yf }
    let bottomLeft = { x = xf; y = yf }

    let valueTopRight = Array.item ((Array.item (xx + 1) permutation) + yy + 1) permutation
    let valueTopLeft = Array.item ((Array.item xx permutation) + yy + 1) permutation
    let valueBottomRight = Array.item ((Array.item (xx + 1) permutation) + yy) permutation
    let valueBottomLeft = Array.item ((Array.item xx permutation) + yy) permutation

    let dotTopRight = dot topRight (getConstVec valueTopRight)
    let dotTopLeft = dot topLeft (getConstVec valueTopLeft)
    let dotBottomRight = dot bottomRight (getConstVec valueBottomRight)
    let dotBottomLeft = dot bottomLeft (getConstVec valueBottomLeft)

    let u = fade xf
    let v = fade yf

    lerp u (lerp v dotBottomLeft dotTopLeft) (lerp v dotBottomRight dotTopRight)


let noise2dWithOctaves x y octaves =
    let rec getOctave idx (n: float) a f =
        if idx < octaves then
            getOctave (idx + 1) (n + (a * (noise2d (x * f) (y * f)))) (a * 0.5) (f * 2.0)
        else
            n

    getOctave 0 0.0 1.0 0.005