print_string "hello, world!\n";;
(*  *)
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

type nat = Zero | OneMoreThan of nat;;

let rec int_of_nat = function Zero -> 0 | OneMoreThan (x) -> 1 + int_of_nat x;;

let rec add m n = match m with Zero -> n | OneMoreThan(x) -> OneMoreThan (add x n);;

let rec mul m n = match m with Zero -> Zero | OneMoreThan(x) -> add n (mul x n);;

let rec monus m = function Zero -> m | OneMoreThan(x) ->
  match m with Zero -> Zero | OneMoreThan(y) -> monus y x;;

let rec minus m = function Zero -> Some m | OneMoreThan(x) ->
  match m with Zero -> None | OneMoreThan(y) -> minus y x;;

let a = (OneMoreThan (OneMoreThan (OneMoreThan (OneMoreThan Zero))));;
let b = (OneMoreThan (OneMoreThan (OneMoreThan  Zero)));;

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec comptree x = function 0 -> Br (x, Lf, Lf) | n -> Br (x, (comptree x (n - 1)), (comptree x (n -1)));;

let rec inord t n = match t with Lf -> n |
  Br(x, left, right) -> inord left (x:: inord right n);;

let rec postord t n = match t with Lf -> n |
  Br(x, left, right) -> postord left (postord right (x::n));;

let rec reflect = function Lf -> Lf | Br(x, left, right) -> Br(x, reflect right, reflect left);;

let t1 = Br(1, Br(2, Br(4, Lf, Lf),Br(5, Lf, Lf)),Br(3, Br(6, Lf, Lf),Br(7, Lf, Lf)));

type arith = Const of int | Add of arith * arith | Mul of arith * arith;;

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;

let rec string_of_arith = function Const x -> string_of_int x 
  | Add(x,y) -> "(" ^ string_of_arith x ^ "+" ^ string_of_arith y ^ ")"
  | Mul(x, y) -> string_of_arith x ^ "*" ^ string_of_arith y
;;

let rec expand = function
  | Mul(x, y) -> (
    let rec mul_adds xx = function Add(yy, yys) ->
      Add(Mul(xx, yy), mul_adds xx yys) | yy -> Mul(xx, yy) in

    let rec expanded_mul ex ey = match ex with Add(exx, exs) ->
        Add(mul_adds exx ey, expanded_mul exs ey)
       | _ -> mul_adds ex ey

    in expanded_mul (expand x) (expand y)
    )
  | Add(x, y) -> Add(expand x, expand y)
  | Const x -> Const x
;;

let print_int_utl x = print_string ((string_of_int x) ^ "\n");;

type 'a seq = Cons of 'a * (unit -> 'a seq);;

let rec from n = Cons(n, fun () -> from(n+1));;

let rec filter_seq f seq = 
  let rec next f (Cons(x,tail)) = if f x then next f (tail()) else Cons(x,tail) in
  let Cons(init, tail) = next f seq in
  Cons(init, fun () -> filter_seq f (tail()))
;;

let rec seq_get n (Cons(x,tail)) =
  if n = 0 then x else seq_get (n-1) (tail())
;;

let rec skip_until n (Cons(x, tail)) = 
  if x > n then Cons(x, tail) else skip_until n (tail())
;;

let shift n seq =
  filter_seq (fun x -> x mod n = 0) seq
;;

(* 0 based index *)
let get_prime index = 
  let rec prime_seq (Cons(x, tail)) = Cons(x, fun () ->
    (* next prime seq (filtered by next x) *)
    prime_seq (
      (* next seq (filtered by x) *)
      shift x (tail())
    )) in
  seq_get index (prime_seq (from 2))
;;

type ('a, 'b) sum = Left of 'a | Right of 'b;;

let hoge_2(a,b) =
  match a with Left sa -> 
    (match b with Left sb -> Left(Left(sa, sb)) | Right sb -> Right(Left (sa,sb)))
    | Right sa ->
    match b with Left sb -> Right(Right (sa, sb)) | Right sb -> Left(Right(sa,sb))
;;

(* double match !!! *)
let hoge_2(a,b) =
  match a, b with
  | Left sa, Left sb -> Left(Left(sa, sb))
  | Left sa, Right sb -> Right(Left(sa, sb))
  | Right sa, Left sb -> Right(Right(sa, sb))
  | Right sa, Right sb -> Left(Right(sa, sb))
;;

let hoge_3(a,b) = function Left sa -> a(sa) | Right sa -> b(sa);;

let hoge_4 a = (
  (fun x -> a(Left(x))),
  (fun x -> a(Right(x)))
);;

let hoge_5 = function Left f -> (fun x -> Left(f x)) | Right f -> (fun x -> Right(f x));;

type pointI = {get : unit -> int ; set: int -> unit ; inc: unit -> unit};;

let pointC x this () =
    {
      get= (fun () -> !x);
      set= (fun value -> x:= value);
      inc= (fun () -> (this()).set ((this()).get () + 1 ));
  }
;;

let new_point x =
  let x = ref x in
  let rec this () = pointC x this () in
  this()
;;

(* let ref x = { contents = x };; *)
(*  *)
(* let (!) x = x.contents;; *)
(* let (:=) x y = x.contents <- y;; *)
(* let incr x = x := !x + 1;; *)

let fact_imp n =
  if n < 0 then raise (Invalid_argument "n < 0!!!");
  let i = ref n and res = ref 1 in
    while (!i <> 0) do
      res := !res * !i;
      i := !i - 1
    done;
    !res
;;

type color = Blue | Red | Green | White;;

type cpointI = {
  cget: unit -> int;
  cset: int -> unit;
  cinc: unit-> unit;
  getcolor: unit-> color;
};;

let cpointC x col this () =
  let super () = pointC x (fun () -> {
      get = ( fun () -> (this ()).cget());
      set = ( fun x -> (this ()).cset(x));
      inc = ( fun () -> (this ()).cinc());
  }) () in
  {
      cget = (fun () -> (super ()).get());
      cset = (fun x -> (super ()).set(x));
      cinc = (fun () -> (super ()).inc(); col := White);
      getcolor = (fun () -> !col);
  }
;;

let new_cpoint x col =
  let x = ref x and col = ref col in
  let rec this () = cpointC x col this () in
  this()
;;

let cp  = new_cpoint 0 Red;;

let rec change = function (_,0) -> [] 
  | ((c::rest) as coins, total) ->
      if c > total then change (rest,total)
      else (try c::change (coins, total - c) 
      with Failure "change" -> change(rest, total) 
      )
  | _ -> raise @@ Failure "change"
;;

let us_coins = [25; 10; 5; 1] and gb_coins = [50; 20; 10; 5; 2; 1];;

let _print_int x = output_string  stdout @@ string_of_int x;;

let cp f1 f2 =
  let ff1 = open_in f1 and ff2 = open_out f2 in
  let lines = ref "" in
  (try
    while true do
      lines := !lines ^ input_line ff1 ^ "\n"
    done;
  with End_of_file -> ());
  output_string ff2 !lines;
  close_out ff2; close_in ff1;
;;
