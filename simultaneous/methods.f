c     Solve a system of linear equations using Gauss eliminiation method
c
c     Params:
c     - datafile: the file containing the linear system of equations
c     - dimen: the number of linear equations to solve simultaneously
      function gausselimination(datafile, dimen)
       character*31 datafile
       integer dimen
       
       real a, x, y, s
       dimension a(dimen, dimen), x(dimen), y(dimen)
       
       open(unit=4, file=datafile, status='unknown')
11     format(' ', f11.5)
       
       do i = 1, dimen
        read(4, *) (a(i, j), j = 1, dimen), y(i)
c        write(*, *) (a(i, j), j = 1, dimen), y(i)
       end do
       close(unit=4)
       
       do i = 1, dimen
        do j = i+1, dimen
         ratio = a(j, i) / a(i, i)
         do k = 1, dimen
          a(j, k) = a(j, k) - a(i, k) * ratio
         end do
         y(j) = y(j) - y(i) * ratio
        end do
       end do
       
       x(dimen) = y(dimen) / a(dimen, dimen)
       do i = dimen-1, 1, -1
        s = 0.0
        do j = i+1, dimen
         s = s + x(j)*a(i, j)
        end do
        x(i) = (y(i) - s)/a(i, i)
       end do
       
       do i = 1, dimen
        write(*, 11) x(i)
       end do
       
       gausselimination = 0.0
       return
      end

c     Solve a system of linear equations using Gauss-Seidel method
c
c     Params:
c     - datafile: the file containing the linear system of equations
c     - dimen: the number of linear equations to solve simultaneously
      function gaussseidel(datafile, dimen)
       character*31 datafile
       integer dimen
       
       real a, x, y, s
       dimension a(dimen, dimen), x(dimen), prevx(dimen), y(dimen)
       logical first
       
       open(unit=5, file=datafile, status='unknown')
21     format(' ', f11.5)
       
       do i = 1, dimen
        read(5, *) (a(i, j), j = 1, dimen), prevx(i), y(i)
c        write(*, *) (a(i, j), j = 1, dimen), prevx(i), y(i)
        x(i) = prevx(i)
        first = .TRUE.
       end do
       close(unit=5)

       do iter = 1, 5
        do i = 1, dimen
         lhssum = 0.0
         do j = 1, dimen
          if (i .eq. j) then
           cycle
          end if
          lhssum = lhssum + (a(i,j)*x(j))
         end do
         x(i) = (y(i) - lhssum) / a(i, i)
        end do
       end do       
       
       do i = 1, dimen
        write(*, 21) x(i)
       end do
       
       gaussseidel = 0.0
       return
      end
