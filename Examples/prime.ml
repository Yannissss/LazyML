let length = function
| [] -> 0
| _::xs -> 1 + length xs
;;

let map f = function
| [] -> []
| x::xs -> f x :: map f xs
;;

let foldr f acc = function
| [] -> acc
| (x::xs) -> f x (foldr f acc xs)
;;

let any p = foldr (fun x acc => (p x) || acc) 0;;

let all p = foldr (fun x acc => (p x) && acc) 1;;

let filter p = function
| [] -> []
| x::xs -> if p x then x :: filter p xs else filter p xs
;;

let zip x y = match (x,y) with
| ([], _) -> []
| (_, []) -> []
| (x::xs, y::ys) -> (x,y) :: zip xs ys
;;

let not n = if n then 0 else 1;;

let natsFrom n = n :: natsFrom (n + 1);;

let nats = natsFrom 0;;

let take n l =
  if n = 0
    then []
    else match l with
      | [] -> []
      | x::t -> x :: take (n-1) t
;;

let enum l u = if l > u then [] else l :: (enum (l+1) u);;

let flip f x y = f y x;;

let factor x y = not $ y % x;;

let isPrime n = not $ any (fun x => n % x = 0) $ enum 2 (n-1);;

let sieve (p::xs) = p :: sieve (filter (fun x => x % p) xs);;

sieve $ natsFrom 2;;