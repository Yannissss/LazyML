let length = function
| []   -> 0
| _::t -> 1 + length t
and map f = function
| []    -> []
| x::xs -> f x :: map f xs
and fact n = 
  if n = 0
    then 1
    else n * fact (n-1)
and take n l =
  if n = 0
    then []
    else match l with
      | [] -> []
      | x::t -> x :: take (n-1) t
and repeat x = x :: repeat x
and append l1 l2 = match l1 with
| []    -> l2
| x::xs -> x :: (append xs l2)
and cycle l = append l (cycle l) in
length $ take 25 $ cycle [1,2,3]