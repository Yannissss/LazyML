let length = function
| [] -> 0
| _::xs -> 1 + length xs
and map f = function
| [] -> []
| x::xs -> f x :: map f xs
and foldr f acc = function
| [] -> acc
| (x::xs) -> f x (foldr f acc xs) in

let any p = foldr (fun x acc => (p x) || acc) 0
and all p = foldr (fun x acc => (p x) && acc) 1
and filter p = function
| [] -> []
| x::xs -> if p x then x :: filter p xs else filter p xs
and zip x y = match (x,y) with
| ([], _) -> []
| (_, []) -> []
| (x::xs, y::ys) -> (x,y) :: zip xs ys
and not n = if n then 0 else 1
and natsFrom n = n :: natsFrom (n + 1)
and nats = natsFrom 0
and take n l =
  if n = 0
    then []
    else match l with
      | [] -> []
      | x::t -> x :: take (n-1) t
      and mod x y = x % y 
and enum l u = if l > u then [] else l :: (enum (l+1) u)
and flip f x y = f y x
and factor x y = not $ y % x
in

let isPrime n = not $ any (fun x => n % x = 0) $ enum 2 (n-1)
and primes = filter isPrime $ natsFrom 2
and even n = not (n % 2)
in

filter isPrime $ natsFrom 2