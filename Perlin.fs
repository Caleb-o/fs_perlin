module Perlin

open System
open System.Collections.Generic

type vec2 = {
    x : float
    y : float
}

let Dot vec_a vec_b =
  vec_a.x * vec_b.x + vec_a.y * vec_b.y

let mutable seed = 0
let mutable offset = (0, 0)

let MakePermutation () =
    let rand = Random(seed)
    let rec make_perm idx (l: List<int>) =
        if idx < l.Capacity / 2 then
            l.Add(rand.Next() % 255)
            make_perm (idx + 1) l;
        else l;

    let l = make_perm 0 (new List<int>(512))
    l.AddRange(l.GetRange(0, (l.Capacity / 2)))
    l.ToArray()


let permutation = MakePermutation ()

let Init () =
    seed <- DateTime.Now.Millisecond

    let rand = Random(seed)
    offset <- (rand.Next(), rand.Next())


let GetConstVec v =
    match (v &&& 3) with
    | 0 -> { x = 1.0; y = 1.0 }
    | 1 -> { x = -1.0; y = 1.0 }
    | 2 -> { x = -1.0; y = -1.0 }
    | _ -> { x = 1.0; y = -1.0 }

let Fade t =
    ((6.0 * t - 15.0) * t + 10.0) * t * t * t

let Lerp t a b =
    a + t * (b - a)

let Noise2d x y =
    let offsetx, offsety = offset
    let xx = (int (floor x) + offsetx) &&& 255
    let yy = (int (floor y) + offsety) &&& 255
    let xf = x - (floor x)
    let yf = y - (floor y)
    let top_right = { x = xf - 1.0; y = yf - 1.0 }
    let top_left = { x = xf; y = yf - 1.0 }
    let bottom_right = { x = xf - 1.0; y = yf }
    let bottom_left = { x = xf; y = yf }

    let value_top_right = Array.item ((Array.item (xx + 1) permutation) + yy + 1) permutation
    let value_top_left = Array.item ((Array.item xx permutation) + yy + 1) permutation
    let value_bottom_right = Array.item ((Array.item (xx + 1) permutation) + yy) permutation
    let value_bottom_left = Array.item ((Array.item xx permutation) + yy) permutation

    let dot_top_right = Dot top_right (GetConstVec value_top_right)
    let dot_top_left = Dot top_left (GetConstVec value_top_left)
    let dot_bottom_right = Dot bottom_right (GetConstVec value_bottom_right)
    let dot_bottom_left = Dot bottom_left (GetConstVec value_bottom_left)

    let u = Fade xf
    let v = Fade yf

    Lerp u (Lerp v dot_bottom_left dot_top_left) (Lerp v dot_bottom_right dot_top_right)


let Noise2dWithOctaves x y octaves =
    let rec GetOctave idx (n: float) a f =
        if idx < octaves then
            GetOctave (idx + 1) (n + (a * (Noise2d (x * f) (y * f)))) (a * 0.5) (f * 2.0)
        else
            n

    GetOctave 0 0.0 1.0 0.005