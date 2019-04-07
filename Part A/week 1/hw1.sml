(* Task 1, comparison of two dates *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) <> (#1 date2)
    then (#1 date1) < (#1 date2)
    else if (#2 date1) <> (#2 date2)
    then (#2 date1) < (#2 date2)
    else (#3 date1) < (#3 date2)

(* Task 2, count how many dates are in one month *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
    then 1 + number_in_month((tl dates), month)
    else number_in_month((tl dates), month)		     

(* Task 3, count how many dates are in a list of months *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

(* Task 4, return list of dates which are in the month *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
    then (hd dates) :: dates_in_month((tl dates), month)
    else dates_in_month((tl dates), month)

(* Task 5, return list of dates which are in the month list *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

(* Task 6, return the n_th element of a list *)							 
fun get_nth (strings : string list, n : int) =
    if n = 1
    then (hd strings)
    else get_nth((tl strings), n - 1)

(* Task 7, convert an "int" date to format string *)
fun date_to_string (year : int, month : int, day : int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "Augest",
		  "September", "October", "November", "Decemer"];
    in get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end

(* Task 8, countup less or equal to sum *)
fun number_before_reaching_sum (sum : int, numbers : int list) =
    if sum <= (hd numbers)
    then 0
    else 1 + number_before_reaching_sum(sum - (hd numbers), (tl numbers))
	
(* Task 9, take a day of year and calculate what month it belongs to *)	
fun what_month (day : int) =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 1 + number_before_reaching_sum(day, days)
    end

(* Task 10, take two days and return a list of months between the two given days *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* Task 11, return option of the oldest date *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let
	 (* nested funtion for slecting the oldest day *)
	    fun oldest_nonempty(dates : (int*int*int) list) =
		if null (tl dates)
		then hd dates
		else let val tl_ans = oldest_nonempty(tl dates)
		     in
			 if is_older(hd dates, tl_ans)
			 then hd dates
			 else tl_ans
		     end
         in
	 (* construct option *)
	     SOME(oldest_nonempty dates)
         end    
