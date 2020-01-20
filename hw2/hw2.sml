(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
   s1 = s2

(* put your solutions for Part 1 here *)

fun all_except_option(str, []) = NONE
   | all_except_option(str, x::xs) =
      let
         fun search(_, []) = NONE
         | search(head, x::xs) =
            if same_string(str, x)
            then SOME (head @ xs)
            else search(head @ [x], xs)
      in
         search([], x::xs)
      end

fun test1_0 =
   val test1_1=all_except_option("3",["4","9","10"]) = NONE;

fun get_substitutions1([], str) = []
   | get_substitutions1(x::xs, str) =
      case all_except_option(str, x) of
         NONE => get_substitutions1(xs, str)
         | SOME e => e @ get_substitutions1(xs, str)

fun get_substitutions2(strlislis, str) =
   let
      fun aux([], acc) = acc
      | aux(x::xs, acc) =
         case all_except_option(str, x) of
            NONE => aux(xs, acc)
            | SOME e => aux(xs, acc @ e)
   in
      aux(strlislis, [])
   end

fun similar_names(namelis, {first=f, middle=m, last=l}) =
   let
      val names = get_substitutions2(namelis, f)
      fun aux([], acc) = acc
      | aux(x::xs, acc) =
         aux(xs, acc @ [{first=x, last=l, middle=m}])
   in
      aux(names, [{first=f, middle=m, last=l}])
   end

(*
   fun all_except_option(str: string, lis: string list) =
      let
         fun search(_, []) = NONE
         | search(head, x::xs) =
            if same_string(str, x)
            then SOME (head @ xs)
            else search(head @ [x], xs)
      in
         search([], lis)
      end
*)

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)

fun card_color(suit, rank) =
   case suit of
      Clubs => Black
      | Diamonds => Red
      | Hearts => Red
      | Spades => Black

fun card_value(suit, rank) =
   case rank of
      Num x => x
      | Ace => 11
      | _ => 10

(*
exception notFound
fun remove_card(cs, c, e) = raise e
   | remove_card(x::xs, c, e) =
      let
         fun search(_, []) = raise e
         | search(head, x::xs) =
            if head = x
            then head @ xs
            else search(head @ [x], xs)
      in
         search([], cs)
      end
*)





