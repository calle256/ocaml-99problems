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

let rec decode_single (vl: 'a rle):  'a list = 
  match vl with
  | One x -> [x]
  | Many (0, x) -> [x]
  | Many (n, x) -> x :: decode_single (Many (n-1, x))

let rec decode (lst: 'a rle list): 'a list =
  match lst with 
  | [] -> []
  | x :: xs -> decode_single x @ decode xs

let rec duplicate (lst: 'a list): 'a list = 
  match lst with
  | [] -> []
  | x :: xs -> x :: (x :: duplicate xs)


let rec replicate_single (v: 'a) (n: int): 'a list = 
  match (v, n) with 
  | (_, 0) -> [] 
  | (x, num) -> x :: (replicate_single x (num-1))

let rec replicate (lst: 'a list) (n: int): 'a list = 
  match lst with 
  | [] -> []
  | x :: xs -> (replicate_single x n) @ (replicate xs n)

let drop (lst: 'a list) (n: int): 'a list = 
  let rec aux num = function 
    | [] -> []
    | x :: xs -> if num = 1 then (aux n xs) else x :: (aux (num-1) xs)
  in aux n lst

let split (lst: 'a list) (num: int): 'a list * 'a list = 
  let rec aux acc n= function 
    | [] -> (List.rev acc, [])
    | x :: xs -> if n = 0 then (List.rev acc, x :: xs) else aux (x :: acc) (n-1) xs
  in aux [] num lst

let rec slice (lst: 'a list) (n1: int) (n2: int): 'a list = 
  match (lst, n1, n2) with 
  | ([], _, _) -> []
  | (x :: xs, 0, t) -> x :: (slice xs 0 (t-1))
  | (_, _, 0) ->  []
  | (_ :: xs, u, t) -> (slice xs (u-1) (t-1))

let rotate (lst: 'a list) (n: int) : 'a list = 
  let (fst, snd) = split lst n in 
    snd @ fst

let rec remove_at num lst = 
  match (num, lst) with 
  | (0, _ :: xs) -> xs 
  | (_, []) -> []
  | (n, x :: xs) -> x :: (remove_at (n-1) xs)

let rec insert_at vl n lst = 
  match (n, lst) with 
  | (0, xs) -> vl :: xs 
  | (_, []) -> []
  | (n, x ::xs) -> x :: insert_at vl (n-1) xs

let rec range fst lst = 
  if fst <= lst then fst :: range (fst+1) lst else []


let rec rand_list n high = 
  Random.init 0; 
  match n with 
  | 0 -> []
  | n -> (Random.int n) :: rand_list (n-1) high
