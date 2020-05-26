let fibo u v = u :: fibo v (u + v);;

let take n l = 
  if n = 0
    then []
    else match l with
      | [] -> []
      | (x::xs) -> x :: take (n-1) xs
;;

take 100 $ fibo 0 1