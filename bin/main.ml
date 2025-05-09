let rec _last (xs: 'a list): 'a option = 
  match xs with 
  | [] -> None
  | [x] -> Some x 
  | _ :: rest -> _last rest

let rec read_lines_to_list() = 
  try
    let line = read_line() in 
    line :: read_lines_to_list () 
  with End_of_file -> []

let rec _last_two (xs: 'a list): ('a * 'a) option = 
  match xs with 
  | [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: rest -> _last_two rest

let rec _at (n: int) (xs: 'a list): 'a option =
  match (n, xs) with
  | (_, []) -> None 
  | (0, [x]) -> Some x
  | (n, _::rest) -> _at (n-1) rest

let _read_int () = 
  try
    Some (read_line () |> int_of_string) 
  with 
  | Failure _ -> None


let _print_list (lst: 'a list) = List.iter print_endline lst

let rec _length (xs: 'a list): int = 
  match xs with
  | [] -> 0
  | [_] -> 1 
  | _ :: rest -> 1+ _length rest

let rec reverse (xs: 'a list): 'a list = 
  match xs with 
  | [] -> []
  | [x] -> [x]
  | x :: rest -> reverse rest @ [x]

let palindrome (xs: 'a list): bool = 
  reverse xs = xs

type 'a node = 
  | One of 'a 
  | Many of 'a node list


let flatten list = 
  let rec aux acc = function 
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t 
  in
  List.rev(aux [] list)

let compress list = 
  let rec aux acc = function
    | [] -> acc
    | [x] -> x :: acc 
    | x :: y :: xs -> if x = y then (aux acc (y :: xs)) else (aux (x :: acc) (y :: xs))
  in 
  List.rev (aux [] list)

let pack (lst: 'a list): 'a list list = 
  let rec aux acc curr = function 
    | []  -> [[]] 
    | [x] ->  (x :: curr) :: acc
    | x :: (y :: _ as t) -> 
        if x = y then aux acc (y :: curr) t  
        else aux ((x :: curr) :: acc) [] t
  in
  aux [] [] lst

let encode (lst: 'a list): ('a * int) list =  
  let rec aux curr acc = function
    | [] -> []
    | [x] -> (x, curr+1) :: acc
    | x :: (y :: _ as t) -> 
        if x = y then aux (curr + 1) acc t 
        else aux 0 ((x, curr+1) :: acc) t
    in List.rev (aux 0 [] lst)

type 'a rle = 
  | One of 'a 
  | Many of int * 'a

let encode2 (lst: 'a list): 'a rle list = 
  let rec aux curr acc = function
    | [] -> []
    | [x] -> 
        if curr = 0 then One x :: acc 
        else Many (curr+1, x) :: acc      
    | x :: (y :: _ as t) -> 
        if x = y then aux (curr + 1) acc t 
        else if curr = 0 then aux 0 (One x :: acc) t
        else aux 0 (Many (curr+1, x) :: acc) t
    in List.rev (aux 0 [] lst)


   
let () = 
  print_endline "hello" 
