print_string "hello, world!\n";;

let dollar_to_yen (dollar: float): int = int_of_float (dollar *. 111.12);;

let dollar_to_yen_string (dollar: float): string = 
  let yen = dollar_to_yen(dollar) in
    string_of_float(dollar) ^ " dollars are " ^ string_of_int(yen) ^ "yen.\n";;

print_string(dollar_to_yen_string 2.5);;

let x = 2 and y = 1;;

let x = y and y = x;;

let x = (1,2,"aa", dollar_to_yen_string);;

let (a,b,c,d) = x;;

let geo_mean (x,y) = sqrt (x *. y) ;;
print_float(geo_mean(2.1,2.3));;

(* matrix * vector *)
let prodMatVec (mat, vec) =
  let prodRowVec (row, vec) =
    let (r1, r2) = row and (v1, v2) = vec in
    r1 *. v1 +. r2 *. v2 in
  let (row1, row2) = mat in
  (prodRowVec(row1, vec), prodRowVec(row2, vec));;

(* n! *)
let rec fact n = if n = 1 then 1 else n * fact(n - 1);;

let fact n =
  let rec facti (n, res) = if n = 1 then res else facti(n - 1, n * res) in
  facti (n, 1);;

let rec pow_ (x, n) = if n = 1 then x else x * pow_ (x, n- 1);;

let rec powi (x, n, res) = if n = 1 then res * x else powi (x, n - 1, res * x);;

let pow (x, n) =
  let nd = n / 2 and xm = n mod 2 and x2 = x * x in
  let p2 = pow_(x2, nd) in
  if xm = 0 then p2 else p2 * x
;;

let max_ascii str =
  let rec max_ascii_ (str, current) =
    let strlen = String.length(str) and char1 = str.[0] in
    if strlen = 1 then
      current else
        max_ascii_(String.sub str 1 (strlen - 1), max (Char.code char1) current) in
  max_ascii_(str, 0);;

(* curry *)
let concat_curry s1 = fun s2 -> s1 ^ s2;;

let deriv f =
  let dx = 0.1e-10 in
  fun x -> (f(x +. dx) -. f(x)) /. dx;;

let fixpoint f init =
  let threshold = 0.1e-10 in
  let rec loop x =
    let next = f x in
    if abs_float(x -. next) < threshold then x
    else loop next
  in loop init;;

let newton_transform f = fun x -> x -. f(x) /. (deriv f x);;

let newton_method f guess = fixpoint(newton_transform f) guess;;

(* curry pow *)
let rec powc = fun n x -> if n = 1 then x else x * powc (n - 1) x ;;
(* reverse powc *)
let rec powc2 = fun n x -> if n = 1 then x else x * powc x (n - 1);;

let curry f x y = f (x, y);;

let average (x, y) = (x +. y) /. 2.0;;

let curried_avg = curry average;;

(* uncurry *)
let uncurry = fun x y ->
  let (y1, y2) = y in
  x y1 y2;;

let rec repeat f n x = if n > 0 then repeat f (n - 1) (f x) else x;;

let fib n =
  let (fibn, _) = repeat (fun (a1, a2) -> (a1 + a2, a1)) n (1,0)
  in fibn;;

(* charpter 5 list *)
let rec downto0  = function 0 -> [0] | n -> n::downto0 (n-1);;

let rec roman hash =
  function 0 -> "" | num ->
    let rec pick = function (r, a)::rest -> if num / r > 0 then (r, a) else pick rest in
    let (r, a) = pick hash in
    a ^ roman hash (num - r)
;;

let rec concat = function [] -> [] | (x::rest) -> x @ concat rest;;

let rec zip = function [] -> (fun _ -> []) | (x::xs) ->
  (function [] -> [] | (y::ys) -> (x,y)::zip xs ys)
;;

let rec filter p = function [] -> [] | x::xs ->
  let tail = filter p xs in
  if p x then x::tail else tail
;;

let rec belong a = function [] -> false | x::xs ->
  x = a || belong a xs
;;

let rec intersect = function [] -> fun _ -> [] | x::xs ->
  fun yy ->
    let tail = intersect xs yy
    in if belong x yy then x::tail else tail
;;

let rec union a = function [] -> a | y::ys ->
  let tail = union a ys in
  if belong y tail then tail else y::tail
;;

let diff a b =
  let i = intersect a b and u = union a b in
  (* intersect, union*)
  let rec diff_ a = function [] -> [] | b::bs ->
    let tail = diff_ a bs in
    if belong b a then tail else b::tail in
  diff_ i u
;;

let rec quick = function[] -> [] | [x] -> [x] | x :: xs ->  (* x is the pivot *)
  let rec partition left right = function [] -> (quick left) @ (x :: quick right) | y :: ys ->
    if x < y then partition left (y :: right) ys else partition (y :: left) right ys
  in partition [] [] xs;;


let partition piv l =
  match l with [] -> ([], []) | _ ->
    let rec partition_ one two =
      function [] -> (one, two) | a::ls ->
        if piv <= a then partition_ one (a::two) ls else partition_ (a::one) two ls
  in partition_ [] [] l
;;

let rec quicker l sorted = 
  match l with [] -> sorted | [x] -> x::sorted | x::xs ->
  let small, big = partition x xs in
  quicker small (x::(quicker big sorted))
;;

let squares r =
  let max = int_of_float (sqrt (float_of_int r)) in
  let rec squares_ = function -1 -> fun _ -> [] | x ->
    fun y ->
      if x < y then squares_ (x-1) 0
      else
        let a = x*x + y*y in
        if a > r then squares_ (x-1) 0
        else if a = r then (x,y)::squares_ (x-1) 0
        else squares_ x (y+1)
  in squares_ max 0
;;

(* r: result *)
let rec map2 p l r = match l with [] -> r | x::xs -> map2 p xs (r@[p x]) ;;

