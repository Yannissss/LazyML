let length = function
| []   -> 0
| _::t -> 1 + length t
;;

let map f = function
| []    -> []
| x::xs -> f x :: map f xs
;;

let fact n = 
  if n = 0
    then 1
    else n * fact (n-1)
;;

{- 
  Ceci est commentaire ! 
  -}
let take n l =
  if n = 0
    then []
    else match l with
      | [] -> []
      | x::t -> x :: take (n-1) t
;;

let repeat x = x :: repeat x;;

let append l1 l2 = match l1 with
| []    -> l2
| x::xs -> x :: (append xs l2)
;;

let cycle l = append l (cycle l);;

let concat = function
| [] -> []
| (x::xs) -> append x (concat xs)
;;

let bins = aux [[]]
and aux l = let l' = concat [map (fun x => 0::x) l, map (fun x => 1::x) l] in append l' (aux l');;

let fix f = let x = f x in f x;;

let f n = n :: f (n + 7);;

let curry f x y = f (x,y);;

f 0