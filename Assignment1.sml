
fun better_max(xs:int list)=
  if null xs
  then NONE
  else
    let fun notempty(xs : int list)=
	  if null (tl xs)
	  then hd xs
	  else
	      let val value=notempty(tl xs)
	      in 
		  if value>(hd xs)
		  then value
		  else
		      hd xs
	      end
    in SOME (notempty(xs))
    end
fun is_older(first:int*int*int ,second:int*int*int)=
  let fun cmp(x:int , y:int)=
      if x<y
      then true
      else false
  in
     if cmp(#1 first,#1 second)
     then true
     else
	  if  (#1 first)=( #1 second)
	  then (
	      if cmp( #2 first,#2 second)
	      then true
	      else 
		  if (#2 first)=(#2 second)	  
		  then (
		      if cmp( #3 first,#3 second)
		      then true
		      else false )
		    else false)
	  else  false
  end


fun number_in_month(month: (int*int*int) list,num:int)=
  let
      fun sum_num(month_list:(int*int*int) list,month_num:int ,value:int)=
	if null month_list
	then value
	else (if ( #2 (hd month_list)=month_num)
	      then sum_num(tl month_list,month_num,value+1)
	      else sum_num(tl month_list,month_num,value))
  in sum_num(month,num,0)
  end
 

fun number_in_months(month:(int*int*int) list,num: int list)=
if null (tl num)
then number_in_month(month,hd num)::[]
else number_in_month(month,hd num):: number_in_months(month,tl num)
   

fun dates_in_month(month: (int*int*int) list,num:int)=
  let fun trick(month: (int*int*int)list ,num:int, result:(int*int*int)list)=
	if null month
	then  result
	else ( if (#2 (hd month)) = num
	       then trick(tl month,num,(hd month):: result)
	       else trick(tl month,num, result) )
  in trick(month,num,[])
  end				      

fun dates_in_months(month: (int*int*int) list,num:int list)=					
if null num
then []
else dates_in_month(month ,hd num) @ dates_in_months(month,tl num)

fun get_nth(elements: string list,n:int)=
  let fun trick(elements:string list,n:int ,pos:int)=
	if null elements
	then "no"
	else (if n=pos
	      then hd elements
	      else trick(tl elements,n,pos+1))
  in trick(elements,n,1)
  end

fun date_to_string(date:int*int*int)=
  let val months=["January","February"," March"," April",
		  "May", "June", "July", "August", "September", "October", "November", "December"]
      fun trick(month:int,months:string list,pos:int)=		     
	if pos= month
	then hd months
	else trick(month,tl months,pos+1)
  in trick(#1 date,months,1)^"-"^ Int.toString(#2 date) ^"-"^ Int.toString(#3 date)
  end

fun number_before_reaching_sum(number:int list,sum:int)=
  let fun trick(number:int list,sum:int,num:int)=
	if sum>0
	then trick(tl number,sum-(hd number),num+1)
	else num-1
  in trick(number,sum,0)
  end

fun what_month(num:int)=
  let val month=[31,28,31,30,31,30,31,31,30,31,30,31]
      fun trick(num:int,month:int list,what:int)=
	if num<(hd month)
	then what
	else trick(num-(hd month),tl month,what+1)
  in trick(num,month,1)
end	  


fun month_range(day1:int,day2:int)=
  if day1>day2
  then[]
  else what_month(day1)::month_range(day1+1,day2)


fun oldest(dates:(int*int*int) list)=
  if null dates
  then NONE
  else (if null (tl dates)
	then SOME (hd dates)
	else SOME (let fun find(dates:(int*int*int)list,temp:(int*int*int))=
			 if null dates
			 then temp
			 else (if is_older(temp,hd dates)
			       then find(tl dates,temp)
			       else find(tl dates,hd dates)
			      )
	    
		   in find (tl dates,hd dates)
		   end))
		      
		       
