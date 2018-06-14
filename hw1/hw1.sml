fun is_older (date1: int * int * int, date2: int * int * int) =
    let
	val dl1 = [#1 date1, #2 date1, #3 date1];
	val dl2 = [#1 date2, #2 date2, #3 date2];
	fun compare (Dl1: int list, Dl2: int list) =
	    if null (tl Dl1)
	    then
		hd Dl1 < hd Dl2
	    else
		if hd Dl1 = hd Dl2
		then
		    compare(tl Dl1, tl Dl2)
		else
		    hd Dl1 < hd Dl2
    in
	compare(dl1,dl2)
    end

	
fun number_in_month (dl: (int * int * int) list, month: int) =
    if null dl
    then
	0
    else
	if (#2 (hd dl)) = month
	then
	    1 + number_in_month(tl dl, month)
	else
	    number_in_month(tl dl, month)

fun number_in_months (dl: (int * int * int) list, ml: int list) =
    if null ml
    then
	0
    else
	number_in_month(dl,hd ml) +  number_in_months(dl, tl ml)

fun dates_in_month (dl: (int * int * int) list, month: int) =
    if null dl
    then
	[]
    else
	if (#2 (hd dl)) = month
	then
	    hd dl :: dates_in_month(tl dl, month)
	else
	    dates_in_month(tl dl, month)
			  
fun dates_in_months (dl: (int * int * int) list, ml: int list) =
    let
	fun append(l1: 'a list, l2: 'a list) =
	    if null l1
	    then
		l2
	    else
		hd l1 :: append(tl l1, l2)
    in
	if null ml
	then
	    []
	else
	    append(dates_in_month(dl, hd ml), dates_in_months(dl, tl ml))
    end
	
fun get_nth (sl: string list, n: int) =
    if null sl orelse n < 1
    then
	""
    else
	if n=1
	then
	    hd sl
	else
	    get_nth(tl sl, n-1)

fun date_to_string (date: int*int*int) =
    let
	val months = ["January", "February", "March", "April",
		      "May", "June", "July", "August",
		      "September", "October", "November", "December"];
    in
	get_nth(months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end
	
fun number_before_reaching_sum (sum: int, lst: int list) =
    if sum > hd lst
    then
	1 + number_before_reaching_sum(sum - hd lst, tl lst)
    else
	0

fun what_month (day: int) =
    let
	val month_day = [31,28,31,30,31,30,31,31,30,31,30,31];
    in
	number_before_reaching_sum(day, month_day) + 1
    end

fun month_range (day1: int, day2: int) =
    let
	fun get_range(start:int) =
	    if  start = day2
	    then
		[what_month(day2)]
	    else
		what_month(start) :: get_range(start+1)
    in
	if day1 > day2
	then
	    []
	else
	    get_range(day1)
    end
	
fun oldest (dl: (int * int * int) list) =
    if null dl
    then NONE
    else
	let
	    val tl_ans = oldest(tl dl);
	in
	    if isSome tl_ans andalso is_older(valOf tl_ans,hd dl)
	    then
		tl_ans
	    else
		SOME (hd dl)
	end

fun number_in_months_challenge (dl: (int * int * int) list, ml: int list) =
    let
	fun is_contained (ele: int, lst: int list) =
	    if null lst
	    then
		false
	    else
		hd lst = ele orelse is_contained(ele, tl lst)
	    
	fun move_dup (ml:int list) =
	    if null ml
	    then []
	    else
		let
		    val tl_ans = move_dup(tl ml);
		in
		    if is_contained(hd ml, tl_ans)
		    then
			tl_ans
		    else
			hd ml :: tl_ans
		end
		    
        val ml_no_dup = move_dup(ml);
    in
	
	if null ml_no_dup
	then
	    0
	else
	    number_in_month(dl,hd ml_no_dup) +  number_in_months(dl, tl ml_no_dup)
    end
	
fun dates_in_months_challenge (dl: (int * int * int) list, ml: int list) =
    let
	fun is_contained (ele: int, lst: int list) =
	    if null lst
	    then
		false
	    else
		hd lst = ele orelse is_contained(ele, tl lst)
	    
	fun move_dup (ml:int list) =
	    if null ml
	    then []
	    else
		let
		    val tl_ans = move_dup(tl ml);
		in
		    if is_contained(hd ml, tl_ans)
		    then
			tl_ans
		    else
			hd ml :: tl_ans
		end
		    
        val ml_no_dup = move_dup(ml);
	
	fun append(l1: 'a list, l2: 'a list) =
	    if null l1
	    then
		l2
	    else
		hd l1 :: append(tl l1, l2)
    in
	if null ml_no_dup
	then
	    []
	else
	    append(dates_in_month(dl, hd ml_no_dup), dates_in_months(dl, tl ml_no_dup))
    end

fun reasonable_date (date: int * int * int) =
    let
	fun is_leap(year: int) =
	    year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
	fun check_day(month: int, day: int, month_day_lst: int list) =
	    if month < 1 orelse month > 12
	    then
		false
	    else
		if month = 1
		then
		    day > 0 andalso day <=  hd month_day_lst
		else
		    check_day(month-1, day, tl month_day_lst)
	val m_d_leap = [31,29,31,30,31,30,31,31,30,31,30,31];
	val m_d = [31,28,31,30,31,30,31,31,30,31,30,31];
    in
	if is_leap(#1 date)
	then
	    #1 date > 0 andalso check_day(#2 date, #3 date, m_d_leap)
	else
	    #1 date > 0 andalso check_day(#2 date, #3 date, m_d)
    end
	
