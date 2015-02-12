(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(*Problem 1 *)

fun is_upper s = Char.isUpper(String.sub (s, 0))
fun only_capitals ss = List.filter is_upper ss 

(*Problem 2 and alternate solution*)

fun longest_string1 ss = 
    List.foldl (fn (s1,s2) => 
		   if String.size s1 > String.size s2 then s1 else s2) "" ss
	       
val is_longer1 = 
 fn (s1,s2) => if String.size s1 > String.size s2 then s1 else s2

fun longest_string1a ss = List.foldl is_longer1 "" ss

(*Problem 3 and alternate solution*)

fun longest_string2 ss = 
    List.foldl (fn (s1,s2) => 
		   if String.size s1 >= String.size s2 then s1 else s2) "" ss
	       
val is_longer2 = 
 fn (s1,s2) => if String.size s1 >= String.size s2 then s1 else s2

fun longest_string2a ss = List.foldl is_longer2 "" ss

(*Problem 4 *)

fun longest_string_helper f ss =
    List.foldl (fn (s, acc) => 
		   if f(String.size s, String.size acc) then s else acc) "" ss
	       
fun longest_string3(ss) =
    let
        val greater = fn (a, b) => a > b
    in
	longest_string_helper greater ss
    end
	
fun longest_string4(ss) =
    let
        val greater_eq = fn (a, b) => a >= b
    in
        longest_string_helper greater_eq ss
    end

(* Below works, but wrong type sig
fun longest_string_helper1 is_upper = List.foldl is_upper ""
val longest_string3 = longest_string_helper is_longer1
val longest_string4 = longest_string_helper is_longer2 *)

(*Problem 5 *)

val longest_capitalized = longest_string3 o only_capitals

(*Problem 6 *)

val rev_string = implode o rev o explode

(*Problem 7*)

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
     | x::xs => case f x of
		    NONE => first_answer f xs
		  | SOME y => y
				  
(*Problem 8*)

fun all_answers f lst =
    let fun helper (f, lst, acc) =
	    case lst of 
		[] => SOME acc
	      | x::xs => case f(x) of
			     NONE => NONE
			   | SOME y => helper(f, xs, acc @ y)
    in 
	helper(f, lst, [])
    end

(*Problem 9a*)
	
val count_wildcards =
    g (fn x => 1) (fn y => 0) 

(*Problem 9b*)

val count_wild_and_variable_lengths =
    g (fn x => 1) (fn y => String.size y) 

(*Problem 9c*)

fun count_some_var (s,p) =
    g (fn x => 0) (fn y => if y = s then 1 else 0) p

(*Problem 10*)

fun s_get (p, acc) =
    case p of
	Variable x        => x :: acc
      | TupleP ps         => List.foldl (fn (pat, accum) => 
					    s_get(pat,acc) @ accum) [] ps
      | ConstructorP(_,p) => s_get(p,acc)
      | _                 => []


fun duplicated ss = case ss of
			[] => false
		      | x :: xs => (List.exists (fn y => x = y) xs) 
				   orelse (duplicated xs)

fun check_pat p = not (duplicated (s_get (p, [])))

(*Problem 11*)

fun match (v : valu, p : pattern) = 
(* returns (string * valu) list option, i.e. SOME [("x",Const 3)]*)
    case (v,p) of
	(_, Wildcard) => SOME[]
      | (x, Variable s) => SOME[(s,x)]
      | (Unit, UnitP) => SOME[]
      | (Const x, ConstP y) => if x = y then SOME[] else NONE
      | (Tuple vs, TupleP ps) => 
	if List.length(vs) = List.length(ps) 
	then let val pairlst = ListPair.zip(vs, ps)
		 fun pair_checker (pairlst) = 
		     all_answers (fn (a,b) => 
				     if match(a,b) <> NONE 
				     then match(a,b) 
				     else NONE) pairlst
	     in pair_checker (pairlst)
	     end
	else NONE
      | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 
						       then match (v,p) 
						       else NONE
      | _ => NONE 

(*Problem 12*)

fun first_match v ps =
    SOME(first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
