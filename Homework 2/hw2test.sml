(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)

use "hw2provided.sml";

(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

print("PROBLEM 1a TESTS\n");

val test1_only_string = all_except_option("string", ["string"]) = SOME [];
val test1_empty_lst = all_except_option("string", []) = NONE;
val test1_no_string = all_except_option("string", ["foo"]) = NONE;
val test1_no_string_2 = 
    all_except_option("string", ["foo", "bar"]) = NONE;
val test1_string_at_end = 
    all_except_option("string", ["foo", "bar", "string"]) = SOME ["foo", "bar"];
val test1_string_at_beg = 
    all_except_option("string", ["string", "foo", "bar"]) = SOME ["foo", "bar"];
val test1_string_in_mid = 
    all_except_option("string", ["foo", "string", "bar"]) = SOME ["foo", "bar"];
val test1_2_items_str_1st = 
    all_except_option("string", ["string", "foo"]) = SOME ["foo"];
val test1_2_items_foo_1st = 
    all_except_option("string", ["foo", "string"]) = SOME ["foo"];
val test1_something = 
    all_except_option("string", ["a", "b", "string", "d", "e"]) = SOME ["a", "b", "d", "e"];

print("PROBLEM 1b TESTS\n");

val test2_1stringlst_s1st = get_substitutions1([["foo"],["there"]], "foo") = [];
val test2_1stringlst_s2nd = get_substitutions1([["foo"],["there"]], "there") = [];
val test2_3stringlst_1 = 
    get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"] ;
val test2_2stringlst_1 = 
    get_substitutions1([["string", "str", "stringo"],["foo", "bar"]], "string") = ["str", "stringo"];
val test2_2stringlst_2 = 
    get_substitutions1([["foo", "bar"],["string", "str", "stringo"]], "string") = ["str", "stringo"];
val test2_rpt_string = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"];
val test2_noStringInLst = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"StringThatIsNotInTheList") = [];

print("PROBLEM 1c TESTS\n");

val test3_1stringlst_s1st = 
    get_substitutions2([["foo"],["there"]], "foo") = [];
val test3_1stringlst_s2nd = 
    get_substitutions2([["foo"],["there"]], "there") = [];
val test3_3stringlst_1 = 
    get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"];
val test3_2stringlst_1 = 
    get_substitutions2([["string", "str", "stringo"],["foo", "bar"]], "string") = ["str", "stringo"];
val test3_2stringlst_2 = 
    get_substitutions2([["foo", "bar"],["string", "str", "stringo"]], "string") = ["str", "stringo"];
val test3_rpt_string = 
    get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"];
val test3_noStringInLst = 
    get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"StringThatIsNotInTheList") = [];

print("PROBLEM 1d TESTS\n");

val test4_textbook = 
    similar_names([["Fred","Fredrick"],
		   ["Elizabeth","Betty"],
		   ["Freddie","Fred","F"]],
		  {first="Fred", middle="W", last="Smith"}) = 
    [{first="Fred", last="Smith", middle="W"}, 
     {first="Fredrick", last="Smith", middle="W"},
     {first="Freddie", last="Smith", middle="W"}, 
     {first="F", last="Smith", middle="W"}];

val test4_simple = 
    similar_names([["Freddie","Fred","F"], ["Elizabeth","Betty"]],
		  {first="Fred", middle="W", last="Smith"}) = 
    [{first="Fred", last="Smith", middle="W"},
     {first="Freddie", last="Smith", middle="W"}, 
     {first="F", last="Smith", middle="W"}];

val test4_nosubs =
    similar_names([["Elizabeth","Betty"]],
		  {first="Fred", middle="W", last="Smith"}) = 
    [{first="Fred", last="Smith", middle="W"}];

val test4_duplicates =
    similar_names([["Freddie","Fred","F"], ["Freddie","Fred","F"]],
		  {first="Fred", middle="W", last="Smith"}) = 
    [{first="Fred", last="Smith", middle="W"},
     {first="Freddie", last="Smith", middle="W"}, 
     {first="F", last="Smith", middle="W"},
     {first="Freddie", last="Smith", middle="W"}, 
     {first="F", last="Smith", middle="W"}];

print("PROBLEM 2a TESTS\n");

val test5a = card_color((Clubs, Num 2)) = Black;
val test5b = card_color((Diamonds, Jack)) = Red;
val test5c = card_color((Hearts, Ace)) = Red;
val test5d = card_color((Spades, Queen)) = Black; 
 
print("PROBLEM 2b TESTS\n");
val test6 = card_value((Clubs, Num 2)) = 2;
val test6_ace = card_value((Clubs, Ace)) = 11;
val test6_face = card_value((Clubs, King)) = 10;

print("PROBLEM 2c TESTS\n");
val test7_onlycard = 
    remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];
val test7_2items1 =
    remove_card([(Hearts, Ace), (Clubs, Num 5)], (Hearts, Ace), IllegalMove) 
    = [(Clubs, Num 5)];
val test7_2items2 =
    remove_card([(Clubs, Num 5), (Hearts, Ace)], (Hearts, Ace), IllegalMove) 
    = [(Clubs, Num 5)];
(*val test7_nocards = 
    remove_card([], (Hearts, Ace), IllegalMove)
    = IllegalMove;*)
val test7_repeat = 
    remove_card([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) 
    = [(Hearts, Ace)];
val test7_3items1 = 
    remove_card([(Hearts, Ace), (Clubs, King), (Hearts, Ace)], (Hearts, Ace), IllegalMove) 
    = [(Clubs, King),(Hearts, Ace)];
val test7_3items2 = 
    remove_card([(Clubs, King),(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) 
    = [(Clubs, King),(Hearts, Ace)];
val test7_3items3 = 
    remove_card([(Hearts, Ace), (Hearts, Ace),(Clubs, King)], (Hearts, Ace), IllegalMove) 
    = [(Hearts, Ace),(Clubs, King)];

print("PROBLEM 2d TESTS\n");
val test8_2cards_same = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true;
val test8_2cards_diff = all_same_color([(Clubs, Ace), (Hearts, Ace)]) = false;
val test8_2cards_diff1 = all_same_color([(Hearts, Ace), (Clubs, Ace)]) = false;
val test8_1card = all_same_color([(Hearts, Ace)]) = true;
val test8_nocards = all_same_color([]) = true;
val test8_manycards = 
    all_same_color([(Clubs, Ace), (Clubs, Ace),(Clubs, Ace), (Clubs, Ace),(Clubs, Ace), (Spades, Ace),(Clubs, Ace), (Spades, Ace),(Clubs, Ace), (Hearts, Ace)]) = false;



print("PROBLEM 2e TESTS\n");
val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4;
val test9_nocards = sum_cards([]) = 0;
val test9_1card = sum_cards([(Clubs, King)]) = 10;


print("PROBLEM 2f TESTS\n");
val test10_undergoal = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4;
val test10_samecolor_undergoal = 
    score([(Hearts, Num 2),(Diamonds, Num 4), (Diamonds, King), (Hearts, Queen)], 30) = 2;
val test10_samecolor_overgoal = 
    score([(Hearts, Num 2),(Diamonds, Num 4), (Diamonds, King), (Hearts, Queen)], 20) = 9;
val test10_diffcolor_overgoal = score([(Clubs, Num 2),(Diamonds, Num 4), (Diamonds, King), (Hearts, Queen)], 20) = 18
val test10_diffcolor_undergoal = score([(Clubs, Num 2),(Diamonds, Num 4), (Diamonds, King), (Hearts, Queen)], 30) = 4; 

print("PROBLEM 2g TESTS\n");
val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6;
val test11a = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) 
	      = 6;
(*val officiate_test1 = officiate([(Clubs,Jack),(Spades,Num(8))],[Draw,Discard(Hearts,Jack)], 42) 
		      = IllegalMove;*)
val officiate_test2 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw], 42) 
		      = 3;
val officiate_test3 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw], 50) 
		      = 6;
val officiate_test4 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 10)],[Draw,Draw,Discard (Diamonds,Ace),Draw,Draw], 50) 
		      = 8;
val officiate_test5 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 10)],[Draw,Draw,Discard (Clubs,Ace),Draw,Draw], 50) 
		      = 17;


(*
print("PROBLEM 3a TESTS\n");
val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3;

print("PROBLEM 3b TESTS\n");
val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true);

*)
             
             
