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

	The binding for g is (unit -> int) -> (string -> int) -> pattern -> int
	This means that it takes two functions:
		The first accepts the trivial unit type and returning an int
		The second accepts a string and returns an int
	These are passed to the pattern datatype
	And we can see the case statment has all of the types ultimately result as an int

	f1 accepts unit because it will compare its type to see if its a wildcard
	f2 accepts a string that is passed as a variable from a list
	p is a pattern which matches the imput with its case statments

	we set f1 as fn() => 1 so that it patten matches with Wildcard and adds 1 to the acc in the fold
	we set f2 as fn(x) => 0 so that the strings in the list can be passed and checked for being a Wildcard


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

fun longest_capitalized xs =
	let
		val first_cap = (longest_string1) o (only_capitals)
	in
		first_cap xs
	end

fun rev_string s = ((String.implode) o (List.rev) o (String.explode)) s

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
		| v::vs => case f(v) of
			SOME v => v
			| NONE => first_answer f vs

fun all_answers f xs =
	let
		fun helper(f, xs, acc) =
			case xs of
				[] => SOME acc
				| x::xs => case f(x) of
					NONE => NONE
					| SOME x => helper(f, xs, acc @ x)
	in
		helper(f, xs, [])
	end

fun count_wildcards p = g (fn() => 1) (fn(x) => 0) p

fun count_wild_and_variable_lengths p =








