      program aorder

      implicit none
      integer a(5,5), b(25), i, j
      equivalence (a(1,1),b(1))
c
c     Put some values into "a", and because it
c     occupies the same space also into "b"
c
      do i=1,5
         do j=1,5
            a(i,j)= 100*i+j
         enddo
      enddo
c
      wrte(*,*) "Hundred's digit is the first index of the array"
      write(*,*) 'One''s digit is the second index'
      write(*,2000) ((a(i,j),j=1,5),i=1,5)
      write(*,2001) b
 2000 format ( /, 'a(i,j) = ',/ (5i5))
 2001 format (/, 'b = ',/,(15i5))
      stop
      end

