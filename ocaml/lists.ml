let rec last (ls : 'a list) : 'a option = match ls with
	| []      -> None
	| [x]     -> Some x
	| _ :: xs -> last xs;;
            
let rec last_two (ls : 'a list) : ('a * 'a) option = match ls with
	| [x; y]  -> Some (x,y)
	| _ :: xs -> last_two xs
	| _       -> None;;
            
let rec at (n : int) (ls : 'a list) : 'a option = match ls with
	| []    -> None
	| x::xs -> 
		if n <= 0 
		then None
		else if n == 1 
		then Some x 
		else at (n - 1) xs;;
                       
let rec length (ls : 'a list) : int = match ls with
	| []    -> 0
	| _::xs -> 1 + length xs;;
            
(* tail recursive
let length ls = 
            let rec aux n = function
                        | [] -> n
                        | _ :: t -> aux (n + 1) t
            in aux 0 ls;;
*)

let rec rev (ls : 'a list) : 'a list = match ls with
	| []    -> []
	| x::xs -> (rev xs) @ [x];;
            

(* 
let rev' ls =
            let rec aux acc = function
                        | [] -> acc
                        | h :: t -> aux (h :: acc) t
            in aux [] ls;; 
*)

let rec is_palindrome ls = match ls with
	| [] | [_] -> true
	| x::xs    -> let first = x and last, centre = match rev xs with y::ys -> y, ys in if first == last then is_palindrome centre else false;;
            
type 'a node = One of 'a | Many of 'a node list;;

let x = Many ([One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]);;

let rec flatten (n : 'a node) : 'a list = match n with
	| One x        -> [x]
	| Many []      -> []
	| Many (x::xs) -> let flattened_end = flatten (Many xs) in match x with 
				| One c  -> c::flattened_end
				| Many _ -> let flattened_beg = flatten x in flattened_beg @ flattened_end;;
                             
let compress' = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;                             
let compress (ls : 'a list) : 'a list =
	let rec aux c acc = function
		| []    -> acc
		| x::xs -> if x = c then aux x acc xs else aux x (x::acc) xs
	in match ls with
		| [] -> []
		| x::xs -> x :: (rev (aux x [] xs));;
		
(*
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;
*)
let pack' = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
let pack ls = let rec aux acc = 
	function
		| []    -> acc
		| x::xs -> match acc with
				| []     -> aux [[x]] xs
				| a::ass -> match a with | l::_ -> if l = x then aux ((x::a)::ass) xs else aux ([x]::acc) xs | [] -> [] (* esiste solo perché sennò il compilatore mi rompe il cazzo per nulla *)
	in rev (aux [] ls);;

let encode ls = let rec aux acc = 
	function
		| []    -> acc
		| x::xs -> match x with | l::_ -> aux ((length x, l)::acc) xs | [] -> []
	in rev (aux [] (pack ls));;
            
type 'a rle = One of 'a | Many of int * 'a;;

let encode' ls = let rec aux acc = 
	function
		| []    -> acc
		| x::xs -> match x with | l::_ -> let len = length x in if len == 1 then aux ((One l)::acc) xs else aux ((Many (len, l))::acc) xs | [] -> []
	in rev (aux [] (pack ls));;

let decode_l = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;  
let rec decode ls = match ls with
	| []    -> []
	| x::xs -> match x with
			| One e       -> e::(decode xs)
                        | Many (2, e) -> e::e::(decode xs)
                        | Many (n, e) -> e::(decode ((Many (n - 1, e))::xs));;
                        
let rec encode'' ls = let rec get acc = 
	function
		| []    -> acc
		| x::xs -> match acc with 
				| []     -> get ([One x]) xs
				| a::ass -> match a with
						| One e       -> if x = e then get ((Many (2, e))::ass) xs else get ((One x)::acc) xs
						| Many (n, e) -> if x = e then get ((Many (n + 1, e))::ass) xs else get ((One x)::acc) xs
	in rev (get [] ls);;
	
let list1 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
let rec duplicate = function
	| []    -> []
	| x::xs -> x::x::(duplicate xs);;
            
let rec replicate ls n = let rec repl x = 
	function
		| 0 -> []
		| n -> x::(repl x (n - 1))
	in match ls with
		| [] -> []
		| x::xs -> (repl x n)@(replicate xs n);;
                                    
let drop ls n = let rec dr i = 
	function
		| []    -> []
		| x::xs -> if i == n then dr 1 xs else x :: (dr (i + 1) xs)
	in dr 1 ls;;
                
let split ls n = let rec uno i = 
	function
		| x::xs when i < n  -> x::(uno (i + 1) xs)
		| _                 -> []
	in let rec dos i = 
	function
		| []                -> []
		| x::xs when i >= n -> x::(dos (i + 1) xs)
		| _::xs             -> dos (i + 1) xs
	in (uno 0 ls, dos 0 ls);;
                 
let slice ls i j = let rec tk n = 
	function
		| []    -> []
		| x::xs -> if (n >= i) && (n <= j) then x::(tk (n + 1) xs) else tk (n + 1) xs
	in tk 0 ls;;
            
let rotate ls n = 
	let fold_until f acc n = function
		| []         -> (acc, [])
		| x::xs as l -> if n = 0
				then (acc, l)
				else fold_until f (f acc x) (n - 1) xs
	in match fold_until (fun ???) [] 0 ls with (inizio, fine) -> fine@inizio;;
