(*
val x = 1
val f = (fn y => y x)
val x = 7
val g = (fn y => x - y)
val ans = f g;



val f = fn x => List.map hd x
val ans = f [[1,2,3],[4,5]];


fun evenOff lis =
   let
      val x = length lis
      fun evenPos lis x =
         ()



fun listify lis =
   case ls of
      [] => []
      | x::xs => x @ map(listify, xs)

val 3btest = [1,9,3]

listify(lis)


fun f x y z =
x (y) + z
val y = 3
fun g z =
let
val x = fn x => x*2
in
f z
end
val h = g (fn a => a*a)
val ans = h 5 2;


fun map (f,xs) =
case xs of
[] => []
| x::xs' => (f x)::(map(f,xs'))

fun f x =
case x of
[] => 0
| ((a,b)::[]) => (a + b)
| ((a,b)::(c,d)::e) => (a + d +f(e))
val ans = f (map (fn x => (1,x),[2,3,4]))

fun addAllOpt lis =
   let
      fun help(lis, acc) =
         case lis of
            [] => NONE
            | SOME e => help(tl lis, acc+e)
            | NONE => if (acc=0) then NONE else help(tl lis, acc)
   in
      help(lis, 0)
   end;

val test = addOpt ([SOME 1, NONE, SOME 3]);




val x = 7;
fun g y = x*y;
fun f z =let
val x = 3
in
g(z) + x
end
val ans =f(2)

*)
fun f p =
   let
      val x = 3
      val y = 4
      val (z,w) = p
   in
      (z (w y)) + x
   end
val x = 1
val y = 2
val ans = f((fn z => x + z), (fn x => x + x + 0));




