(*1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.)*)

fun is_older (date1 : (int*int*int), date2 : (int*int*int)) = 
    if #1 date1 = #1 date2
    then if #2 date1 = #2 date2
	 then if #3 date1 = #3 date2
	      then false
	      else #3 date1 < #3 date2
	 else #2 date1 < #2 date2
    else #1 date1 < #1 date2

(*Attempt at recursive alternate to is_older. Doesn't handle = case as above. Sad.

fun tuple_to_list (date : (int*int*int)) =
    [#1 date, #2 date, #3 date]

fun is_older_tuple(xs : int list, ys : int list) =
	if null xs
	then false
	else
	    if hd xs < hd ys
	    then true
	    else is_older_tuple(tl xs, tl ys)

fun is_older2 (date1 : (int*int*int), date2 : (int*int*int)) =
    is_older_tuple(tuple_to_list(date1), tuple_to_list(date2))
*)

(*2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month.*)

fun number_in_month (date_list : (int*int*int) list, month : int) =
    if null date_list
    then 0
    else
	(*(if #2 (hd date_list) = month then 1 else 0) 
	+ (number_in_month (tl date_list, month))*) (*old version*)
	let val tl_ans = number_in_month (tl date_list, month)
	in if (#2 (hd date_list) = month)
	   then 1 + tl_ans
	   else tl_ans
	end

(*3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem.*)

fun number_in_months (date_list : (int*int*int) list, month_list : int list) =
    if null month_list
    then 0
    else 
	number_in_month(date_list, hd month_list) 
	+ number_in_months(date_list, tl month_list)

(*4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given.*)

fun dates_in_month (date_list : (int*int*int) list, month : int) =
    if null date_list
    then []
    else
	let val tl_ans = dates_in_month (tl date_list, month)
	in if #2 (hd date_list) = month 
	   then hd date_list :: tl_ans
	   else tl_ans
	end

(*5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem and SML's list-append operator (@).*)

fun dates_in_months (date_list : (int*int*int) list, month_list : int list) =
    if null month_list
    then []
    else dates_in_month(date_list, hd month_list) 
	 @ dates_in_months(date_list, tl month_list)

(*6. Write a function get_nth that takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st. Do not worry about the case where the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay.*)

fun get_nth (s_list : string list, n : int) = (*uncaught exception on n > s_list.length*)
    if n = 1 orelse n = 0
    then hd s_list
    else get_nth(tl s_list, n-1)

(*7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013 (for example). Use the operator ^ for concatenating strings and the library function Int.toString for converting an int to a string. For producing the month part, do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a comma following the day and use capitalized English month names: January, February, March, April, May, June, July, August, September, October, November, December.*)

fun date_to_string (date : int * int * int) =
    get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 date) ^ " " 
    ^ Int.toString (#3 date) ^ ", " 
    ^ Int.toString (#1 date)

(*8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int. You should return an int n such that the first n elements of the list add to less than sum, but the first n+1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in value; it is okay for an exception to occur if this is not the case.*)

fun number_before_reaching_sum (sum : int, xs : int list) =
    if null xs
    then 0
    else
	(if hd xs >= sum then 0 else 1) 
	+ number_before_reaching_sum(sum - hd xs, tl xs)

(*9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your answer to the previous problem.*)

fun what_month (day : int) =
    if day < 1 orelse day > 365
    then 0
    else number_before_reaching_sum (day, ([31,28,31,30,31,30,31,31,30,31,30,31])) + 1

(*10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.*)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(*11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.*)
fun oldest (date_list : (int * int * int) list) =
    if null date_list
    then NONE
    else 
	let val tl_ans = oldest(tl date_list)
	in if isSome tl_ans andalso is_older(valOf tl_ans, hd date_list)
	   then tl_ans
	   else SOME (hd date_list)
	end
(*
(*12. Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge that are like your solutions to problems 3 and 5 except having a month in the second argument multiple times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.)*)
fun number_in_months_challenge (date_list : (int*int*int) list, month_list : int list) =
    if null month_list
    then number_in_months(date_list, month_list)
    else 
	let val ms = month_list
	in if hd month_list = hd tl month_list 
	   then tl ms :: [] (*what do I do with this list?*)
	   else number_in_months_challenge(date_list, tl ms)
	end
*)


(*13. Challenge Problem: Write a function reasonable_date that takes a date and determines if it describes a real date in the common era. A \real date" has a positive year (year 0 did not exist), a month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.)*)

(*"date" is an SML value of type int*int*int, where the first part is the year, the second part is the month, and
the third part is the day. A "reasonable" date has a positive year, a month between 1 and 12, and a day no
greater than 31 (or less depending on the month).*)

(*
fun is_date (year : int, month : int, day : int) =
    year > 0 andalso month > 0 andalso month < 13 andalso day > 0 andalso day < 32

fun is_day_of_year (day : int) =
    day > 0 andalso day < 365
*)
