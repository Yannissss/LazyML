let fibo u v = u :: fibo v (u + v) in
let take n l = 
  if n = 0
    then []
    else match l with
      | [] -> []
      | (x::xs) -> x :: take (n-1) xs
in
take 100 $ fibo 0 1