(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["a", "B", "Vv"] = ["B", "Vv"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["A","bc","jk"] = "bc"
val test2a = longest_string1 [] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["A","bc","jk"] = "jk"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"
 
val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5a = longest_capitalized ["A", "BB", "df", "f"] = "BB"

val test6 = rev_string "abc" = "cba";

val test7 = 
    first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8b = all_answers (fn x => if x = x then SOME [x] else NONE) [2,3,4,5,6,7] = SOME[2,3,4,5,6,7]
val test8c = all_answers (fn x => if x > 3 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE 

val test9a = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (Variable "string") = 0
val test9a3 = count_wildcards(TupleP [ConstP 1]) = 0
val test9a4 = count_wildcards(TupleP [ConstP 1, Wildcard, Wildcard]) = 2
val test9a5 = 
    count_wildcards(TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard])]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths (Variable "string") = 6
val test9b3 = count_wild_and_variable_lengths (TupleP [ConstP 1, Variable "string", ConstructorP ("S", TupleP [Wildcard, Wildcard])]) = 8

val test9c = count_some_var ("x", Variable("x")) = 1;
val test9c1 = count_some_var ("x",(TupleP [ConstP 1, Variable "x", ConstructorP ("x", TupleP [Wildcard, Wildcard])])) = 1;

val test_s_get = s_get(TupleP [ConstP 1, Variable "a", ConstructorP ("z", TupleP [Wildcard, Variable "b"])], []) = ["b","a"]
val test_s_get1 = s_get(TupleP [Variable "1", Variable "a", ConstructorP ("z", TupleP [Wildcard, Variable "b"])], []) = ["b","a","1"]
val test_duplicated = duplicated ["a","b","c","d"] = false
val test_duplicated1 = duplicated ["a","b","a","d"] = true
val test_duplicated2 = duplicated ["a","a","c","d"] = true
val test_duplicated3 = duplicated ["a","b","c","c"] = true


val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP[Variable("a"),Variable("a")]) = false
val test10b = check_pat (TupleP[Variable("a"),Variable("b")]) = true
val test10c = check_pat (TupleP [Variable "1", Variable "a", ConstructorP ("z", TupleP [Wildcard, Variable "b"])]) = true
val test10d = check_pat (TupleP [Variable "1", Variable "a", ConstructorP ("1", TupleP [Wildcard, Variable "b"])]) = true
val test10e = check_pat (TupleP [Variable "1", Variable "a", ConstructorP ("z", TupleP [Wildcard, Variable "1"])]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11a = match ((Const 1), (Variable "x")) = SOME [("x", Const 1)];
val test11b = match ((Const 1), (UnitP)) = NONE;
val test11c 
    = match(Constructor("pig", Const 5), ConstructorP ("pig", Variable "myvar"))
      = SOME [("myvar", Const 5)];
val test11d 
    = match(Constructor("pig", Const 5), 
	    ConstructorP ("sheep", Variable "myvar"))
      = NONE;
val test11e
    = match((Tuple[Tuple[Const 1, Const 2]]), TupleP[TupleP[Variable "a", Variable "c"]]) 
      = SOME [("a",Const 1),("c",Const 2)];

(*
val test12 = first_match Unit [UnitP] = SOME [] 
*)
