      program SPEED
c
c    program to time 3 different methods of calculating
c    the dot product of long vectors
c
      integer nrep,i
      parameter (nrep=5000000)
      real   b,c,cfac
      common b,c,cfac
      interface saxpy
         function saxpy (a,x,y)
            real x,y,a,saxpy
c
c    Declare x,y,a as input only to saxpy
c
            intent (in) x,y,a
         end function saxpy
c
      end interface
c
c    The following is a Statement function
c<a name="sf"><font color="FF0000">
      slaxpy(x,y,z)= x*y+z
c</font></a>
c
      cfac=.25
      b=1.
      c=.1
c
c
c    Use the Fortran 90 Intrinsic Subroutine to determine
c    the current time in clock ticks (icount2), the clock
c    rate in clicks per second, and the largest possible
c    count before the clock resets.
c
c    CAUTION:  On this and probably most Unix work stations
c    this clock is measuring real time, not time your program
c    spends running.  If you are sharing the machine with others
c    you will count the time they have the CPU also.  USE the
c    unix "users" command to check for a situation when you are
c    the only user on the machine before running the program.
c    To filter out system activity run the program many times
c    and select results with the lowest total times.
c
      call system_clock(icount1,irate,icmax)
c
      print *, 'clock rate = ',irate, ' ticks per second'
      call system_clock(icount1,irate,icmax)
c
c     The "1000" loop just makes sure that I do lots of work
c     to get good statistics.  Note that coding is set so
c     results of each pass through the loop are a little
c     different.  Without the "+.00001*dotpro" optimization
c     features on many compilers will only execute the loop
c     once.
c
c     Do a simple multiply and add many times
c     I am intentially suppressing any vectorization and forcing
c     a result that won't let the optimizer throw away unused
c     calculations
c
      do 1000 i = 1,nrep
         c = cfac*c +b
 1000    continue
      call system_clock(icount2,irate,icmax)
      time = real(icount2-icount1)/real(irate)
      print *, 'Time for local evaluation = ', time,
     $ ' seconds'
c
c    Repeat the same work using a standard function
c
      call system_clock(icount1,irate,icmax)
      do 1001 i = 1,nrep
         c = saxpy (cfac, c,b)
 1001    continue
      call system_clock(icount2,irate,icmax)
      time = real(icount2-icount1)/real(irate)
      print *, 'Time for function evaluation = ', time,
     $ ' seconds'
c
c    Repeat the same work using a Subroutine
c
      call system_clock(icount1,irate,icmax)
      do 1002 i = 1,nrep
         call ssaxpy(cfac,c,b,c)
 1002    continue
      call system_clock(icount2,irate,icmax)
      time = real(icount2-icount1)/real(irate)
      print *, 'Time for Subroutine = ', time,
     $ ' seconds'
c
c    Repeat the same work using a standard function
c
      call system_clock(icount1,irate,icmax)
      do 1003 i = 1,nrep
         c = slaxpy(cfac,c,b)
 1003    continue
      call system_clock(icount2,irate,icmax)
      time = real(icount2-icount1)/real(irate)
      print *, 'Time for Statement Function = ', time,
     $ ' seconds'
c
c    Repeat the same work using an internal function
c
      call system_clock(icount1,irate,icmax)
      do 1004 i = 1,nrep
         c = siaxpy(cfac,c,b)
 1004    continue
      call system_clock(icount2,irate,icmax)
      time = real(icount2-icount1)/real(irate)
      print *, 'Time for Internal Function = ', time,
     $ ' seconds'
c
c    Repeat the same work using a Subroutine with
c    passing through COMMON
c
      call system_clock(icount1,irate,icmax)
      do 1005 i = 1,nrep
         call scsaxpy
 1005    continue
      call system_clock(icount2,irate,icmax)
      time = real(icount2-icount1)/real(irate)
      print *, 'Time for Subroutine with COMMON = ', time,
     $ ' seconds'
c
c    Do a baseline with just the DO loop and increment
c
      call system_clock(icount1,irate,icmax)
      do 1006 i = 1,nrep
 1006    continue
      call system_clock(icount2,irate,icmax)
      time = real(icount2-icount1)/real(irate)
      print *, 'Time for bare Loop = ', time,
     $ ' seconds'
      print *, c,i
      stop
c<a name="1"><font color="FF0000">
      contains
c</font></a>
         function siaxpy(a1,x1,y1)
         real a1,x1,y1,saxpy
         siaxpy =  a1*x1 + y1
         return
         end function
      end

      function saxpy(a,x,y)
c
c   Multply all contents of  "x" by the scalar "a"
c   then add the result to  "y"
c
      implicit none
      real a,x,y,saxpy
      intent (in) a,x,y
      saxpy =  a*x + y
      return
      end
      subroutine ssaxpy(a,x,y,z)
c
c   Multply all contents of  "x" by the scalar "a"
c   then add the result to  "y"
c
      implicit none
      real a,x,y,z
      z     =  a*x + y
      return
      end
      subroutine scsaxpy
c
c   Multply all contents of  "c" by the scalar "cfac"
c   then add the result to  "b"
c
      implicit none
      real   b,c,cfac
      common b,c,cfac
      c     =  cfac*c + b
      return
      end

