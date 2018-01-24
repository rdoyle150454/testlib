       program MAIN
c new version dated May 6       
       implicit none
       real a,b,s
       
       read *, a,b

       Print *, 'You entered ', a,' and ', b
       
       s = a + b
       

	   Call Sub1()
	   
	   print *,  'The sum of ', a,' and ' , b
       print *, ' is ' , s
	   

       stop
       end program MAIN
