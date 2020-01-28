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

val test1_0=all_except_option("",["4","9","10"]) = NONE;

fun get_substitutions1([], str) = []
   | get_substitutions1(x::xs, str) =
      case all_except_option(str, x) of
         NONE => get_substitutions1(xs, str)
         | SOME e => e @ get_substitutions1(xs, str)

val test2_0=get_substitutions1([["Dan","Daniella", "Stinky"],["Sam","Samuel","Sammy"],["Danny", "Dan"],["Dan","D"]],
                               "Dan")
				= ["Daniella", "Stinky", "Danny", "D"]

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

val test3_0=get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                               "")
            = [];

fun similar_names(namelis, {first=f, middle=m, last=l}) =
   let
      val names = get_substitutions2(namelis, f)
      fun aux([], acc) = acc
      | aux(x::xs, acc) =
         aux(xs, acc @ [{first=x, last=l, middle=m}])
   in
      aux(names, [{first=f, middle=m, last=l}])
   end

val test4_0=similar_names([
                             ["Brian","Brian Brian Kibler"],
                             ["Elizabeth","Betty"],
                             ["Please Dont Call Me Brian 'Brian Kibler'","Brian","Brian 'Please Dont Call Me Brian 'Brian Kibler' Kibler"]
                         ], {first="Brian", middle="", last="Kibler"}) =
            [{first="Brian",last="Kibler",middle=""},
             {first="Brian Brian Kibler",last="Kibler",middle=""},
				 {first="Please Dont Call Me Brian 'Brian Kibler'",last="Kibler",middle=""},
             {first="Brian 'Please Dont Call Me Brian 'Brian Kibler' Kibler",last="Kibler",middle=""}
             ];

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

val test5_0= card_color(Hearts, Num 2) = Red;

fun card_value(suit, rank) =
   case rank of
      Num x => x
      | Ace => 11
      | _ => 10

val test6_0= card_value(Hearts, Num 2) = 2;

exception notFound
fun remove_card([], c, e) = raise e
   | remove_card(cs, c, e) =
      let
         fun search(_, []) = raise e
         | search(head, x::xs) =
            if c = x
            then head @ xs
            else search(head @ [x], xs)
      in
         search([], cs)
      end

val test7_0 = remove_card([] @ [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 5), (Clubs, Num 9)], (Clubs, Num 9), notFound) = [(Clubs, Ace), (Diamonds,Num 10),(Spades,Num 5)];

fun all_same_color(hand) =
	case hand of
		[] => true
		| _::[] => true
		| (c1, r1)::(c2, r2)::others => (c1 = c2) andalso (all_same_color((c2, r2)::others))

val test8_0 = all_same_color([(Clubs, Ace)]) = true;

fun sum_cards([]) = 0
	| sum_cards(x::xs) =
		let
	      fun aux([], acc) = acc
	      | aux(x::xs, acc) =
	         aux(xs, card_value(x) + acc)
	   in
	      aux(x::xs, 0)
	   end

val test9_0 = sum_cards([(Clubs, Ace), (Spades, Ace), (Diamonds, Ace), (Hearts, Ace), (Clubs, Ace), (Clubs, Ace), (Clubs, Num 3)]) = 69;

