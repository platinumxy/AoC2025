let (mod) x y = ((x mod y) + y) mod y;;

let int_of_bool x = if x then 1 else 0 ;;

let ( /% ) x y = if x >= 0 then x / y else (x - y + 1) / y;;