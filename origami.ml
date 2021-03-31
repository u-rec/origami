(*Autor: Jerzy Denisiewicz
 *Recenzent: Tomasz Ziębowicz *)

(* Punkt na płaszczyźnie *)
type point = float * float

type kartka = point -> int

(* porównywanie floatow *)
let epsilon = 1e-9;;

let (<.) a b = a +. epsilon < b

let (>.) a b = a -. epsilon > b

let (=.) a b = a +. epsilon >= b && a -. epsilon <= b

let (<=.) a b = a =. b || a <. b

let (>=.) a b = a =. b || a >. b


let prostokat (p1x, p1y) (p2x, p2y) = fun (px, py) ->
  if px <=. p2x && py <=. p2y &&
    px >=. p1x && py >=. p1y then 1
  else 0

(*kwadrat odleglosci miedzy punktami a, b*)
let dist2 (ax, ay) (bx, by) =
  (Float.pow (ax -. bx) 2.) +. (Float.pow (ay -. by) 2.)

(*przesuń punkt (x, y) o wektor (vx, vy)*)
let move (x, y) (vx, vy) =
  (x +. vx, y +. vy)

(*jeśli kolko wykracza poza max_float w danym ukladzie wspolrzednych,
  przesuwamy je do środka, gdzie mamy pewnosc, ze nie przekroczy
  (bo r < infinity) *)
let rec kolko (px, py) r = fun (sx, sy) ->
  if px +. r = infinity then
    kolko (0., py) r (sx -. px, sy)
  else if py +. r = infinity then
    kolko (px, 0.) r (sx, sy -. py)
  else if px -. r = neg_infinity then
    kolko (0., py) r (sx +. px, sy)
  else if py -. r = neg_infinity then
    kolko (px, 0.) r (sx, sy +. py)
  else if Float.sqrt (dist2 (px, py) (sx, sy)) <=. r then 1
  else 0

(*wektor od p1 do p2*)
let vec (p1x, p1y) (p2x, p2y) =
  (p2x -. p1x, p2y -. p1y)

(* iloczyn wektorowy *)
let rec cross_prod (v1x, v1y) (v2x, v2y) =
    ((v1x *. v2y) -. (v1y *. v2x))

(* zwraca parę liczb (a, b) prostej y = ax + b przechodzącej
   przez punkty p1 i p2.
   Jeśli ta prosta równoległa do OY, to trochę inna reprezentacja:
   (infinity, b), gdzie prosta wyraza sie wzorem x = b             *)
let line p1 p2 =
  let (x1, y1) = p1 in
  let (vx, vy) = vec p1 p2
  in
  if vx = 0. then (infinity, x1)
  else
    (vy /. vx, y1 -. ((vy /. vx) *. x1))

(*wspolrzedne punktu symetrycznego do (x, y) względej prostej y = ax + b *)
let symmetry (x, y) (a, b) =
  let p =
    if a = infinity then (b, y)
    else if a = 0. then (x, b)
    else
    let xs = (y +. (x /. a) -. b) /. (a +. (1. /. a)) in
    let ys = a *. xs +. b
    in (xs, ys)
  in move p (vec (x, y) p)

let zloz p1 p2 k =
  fun ps ->
    let dir = cross_prod (vec p1 p2) (vec p1 ps)
    in
    if dir <. 0. then 0
    else if dir =. 0. then k ps
    else
      (k ps) + (k (symmetry ps (line p1 p2)))

let skladaj lista k =
    List.fold_left (
      fun kar (p1, p2) -> zloz p1 p2 kar
      ) k lista
