(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1a *)

(*Solution using nested case statements.*)
fun all_except_option (str, lst) =
    case lst of 
	[] => NONE
      | x :: xs => case same_string (str, x) of
		     true => SOME(xs)
		   | false =>  case all_except_option (str, xs) of
				   NONE => NONE
				|  SOME y => SOME(x :: y)

(*		 
(* Solution with accumulator -- fails no_string tests, should return NONE.*)
fun all_except_option (str, lst) =
    if null lst then NONE else
    let fun aux (str, lst, acc) =
	    case lst of
		[] => if null acc then NONE else SOME(rev acc)
	      | x :: xs => if same_string (str, x)
			   then if null xs andalso null acc 
				then SOME [] else aux (str, xs, (acc))
			   else aux (str, xs, (x :: acc))
    in
	aux (str, lst, [])
    end	
*)
		   
(* Problem 1b *)

fun get_substitutions1 (subs, s) =
    case subs of
	[] => []
      | lst1::lsts' => case all_except_option (s, lst1) of
			   NONE => get_substitutions1 (lsts', s)
			 | SOME y => y @ get_substitutions1 (lsts', s)

(* Problem 1c *)

fun get_substitutions2 (subs, s) =
    let fun aux (subs, s, acc) =
	    case subs of
		[] => acc
	      | lst1::lsts' => case all_except_option (s, lst1) of
				   NONE => aux(lsts', s, acc)
				 | SOME y => aux(lsts', s, acc @ y)
    in
	aux(subs, s, [])
    end

(* Problem 1d *)
			       
fun similar_names (subnames, {first = first, middle = middle, last = last}) =	
    let val matches = get_substitutions2 (subnames, first)
    in let fun aux (first,middle,last, matches) =
	       case matches of
		   [] => []
		 | x :: xs' => [{first = x, middle = middle, last = last}]
			       @ aux (first,middle,last, xs')
       in 
	   [{first = first,middle = middle,last = last}] 
	   @ aux(first,middle,last, matches)
       end
    end

(* Problem 2a *)

(*you may assume that Num is always used with values 2, 3, ..., 10 
though it will not really come up*)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color ((suit, rank)) = 
    case suit of
	Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red

(* Problem 2b *)

fun card_value ((suit, rank)) =
    case rank of
	Num x => x
      | Ace => 11
      | _ => 10 

(* Problem 2c *)

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x :: xs => if x = c 
		   then xs
		   else x :: remove_card(xs,c,e)

    (*handle e => [];*)

(* Problem 2d *)

fun all_same_color (cs) =
    case cs of
	[] => true
      | x :: xs => case xs of
		       [] => true
		     | y :: ys => if card_color(x) = card_color(y)
				  then all_same_color(xs)
				  else false

(* Problem 2e *)

fun sum_cards (cs) =
    let fun aux (cs, acc) =
	    case cs of
		[] => acc
	      | x :: xs' => aux(xs', card_value(x)+acc)
    in
	aux(cs, 0)
    end			      
					       
(* Problem 2f *)

fun score (held_cs, goal) =
    let val sum = sum_cards(held_cs)
	fun final_score (pre_score) =
	    if all_same_color(held_cs) 
	    then pre_score div 2
	    else pre_score
    in
	if sum > goal 
	then let val pre_score = (3 * (sum - goal))
	     in
		 final_score (pre_score)
	     end
	else let val pre_score = (goal-sum)
	     in
		 final_score (pre_score)
	     end
    end
	
(* Problem 2g *)
			   
fun officiate (cs, ms, goal) = 
    let fun turn (cs, ms, held_cs, goal) =
	    case ms of
		[] => score(held_cs, goal)
	      | x :: xs => (case x of
				Draw => (case cs of 
					    [] => score(held_cs, goal)
					  | y :: ys => if sum_cards(y :: held_cs) > goal
						       then score((y :: held_cs), goal)
						       else turn(remove_card(cs, y, IllegalMove), xs, y :: held_cs, goal)
					)
			      | Discard c => turn(cs, xs, remove_card(held_cs, c, IllegalMove), goal))
    in
	turn (cs, ms, [], goal)
    end
	


