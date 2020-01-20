(*  Assignment #1 *)

type DATE = (int * int * int)
exception InvalidParameter

fun is_older(d1: DATE, d2: DATE) =
  if (#1 d1 = #1 d2)
  then
    if (#2 d1 = #2 d2)
    then
       if (#3 d2 < #3 d2)
       then true
       else false
    else
      if (#2 d1 < #2 d1)
      then true
      else false
  else
    if (#1 d1 < #1 d2)
    then true
    else false

fun number_in_month(dates: DATE list, month: int) =
  if (null dates)
  then 0
  else
    if (#2 (hd dates) = month)
    then (1+number_in_month(tl dates, month))
    else (number_in_month(tl dates, month))

fun number_in_months(dates: DATE list, months: int list) =
  if (null months)
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates,  tl months)

fun dates_in_month(dates: DATE list, month: int) =
  if (null dates)
  then []
  else
    if (#2 (hd dates) = month)
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates: DATE list, months: int list) =
  if (null months)
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(s: string list, n: int) =
  if (n>length s) orelse (n=0)
  then (raise InvalidParameter)
  else
    if (n=1)
    then (hd s)
    else (get_nth(tl s, n-1))

fun date_to_string(d: DATE) =
  let
    val x = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(x, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end

fun number_before_reaching_sum(sum: int, l: int list) =
  let
    fun step(x: int, l: int list, start: int) =
      if (hd l+start>=sum)
      then x
      else step(x+1, tl l, start+hd l)
  in
    step(0, l, 0)
  end

fun what_month(day: int) =
  let
    val x = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, x)+1
  end

fun month_range(day1: int, day2: int) =
  if (day1>day2)
  then []
  else (what_month(day1) :: month_range(day1 + 1, day2))

fun oldest(l: DATE list) =
  if (null l)
  then NONE
  else
    let
      val x=oldest(tl l)
    in
      if (isSome x) andalso (is_older(valOf x, hd l))
      then x
      else SOME (hd l)
    end

fun reasonable_date(date: DATE) =
  let
    val days_normal = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val days_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val months = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]

    fun reasonable_day(day: int) =
      if (day>0 andalso day<=31)
      then true
      else false

    fun reasonable_month(month: int) =
      if (month>0 andalso month<=12)
      then true
      else false

    fun reasonable_year(year: int) =
      if (year>0)
      then true
      else false

    fun is_leap(year: int) =
      if ( (year mod 4=0) andalso (year mod 100<>0) orelse (year mod 400=0) )
      then true
      else false

    fun get_days(days: int list, x: int) =
      if (x=1)
      then (hd days)
      else ( get_days(tl days, x-1) )

    fun reasonable_day_for_month() =
      if (is_leap(#1 date))
      then ( (#3 date)<=get_days(days_leap, #2 date) )
      else ( (#3 date)<=get_days(days_normal, #2 date) )

  in
    reasonable_day(#3 date) andalso reasonable_month(#2 date) andalso reasonable_year(#1 date) andalso reasonable_day_for_month()
  end
