(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g:

*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)

infix !>
fun x !> f = f x

fun only_capitals xs =
	List.filter(fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
	List.foldl(fn (x, acc) => if (String.size(x) > String.size(acc)) then (x) else (acc)) "" xs

fun longest_string2 xs =
	List.foldl(fn (x, acc) => if (String.size(x) = String.size(acc)) then (x) else (
		if (String.size(x) > String.size(acc)) then (x) else (acc))) "" xs

fun longest_string_helper f xs =
	List.foldl(fn (x, acc) => if f (String.size(x), String.size(acc)) then (x) else (acc)) "" xs

fun longest_string3 xs =
	let
		val like_longest1 = longest_string_helper(fn(x, acc) => x > acc)
	in
		like_longest1 xs
	end

fun longest_string4 xs =
	let
		val like_longest2 = longest_string_helper(fn(x, acc) => x >= acc)
	in
		like_longest2 xs
	end














