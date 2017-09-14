c     Integrate the given function in the given limits and steps
c
c     Params:
c     - f: the function to integrate
c     - a: the lower limit of integration
c     - b: the upper limit of integration
c     - n: the number of steps to divide the range into
      function simpson(f, a, b, n)
       real f, a, b, simpson
       integer n
       
       real h, total

c       write(*, *) a, b, n
       
       h = (b - a)/n
       
       total = 0.0
       do i = 1, n-1
        if (mod(i, 2) .eq. 0) then
         total = total + 2.0*f(a + i*h)
        else
         total = total + 4.0*f(a + i*h)
        end if
       end do
       
       total = total + f(a) + f(b)
       simpson = total*h/3.0
       
       return
      end

c     Integrate the given function in the given limits by Gauss' method
c
c     Params:
c     - f: the function to integrate
c     - a: the lower limit of integration
c     - b: the upper limit of integration
c     - datafile: the name of the file containing points and weights
c     - rows: the number of points and corresponding weights in the file
      function gauss(f, a, b, datafile, rows)
       real f, a, b, gauss
       character*31 datafile
       integer rows
       
       real w, x, m, c, total
       
c       write(*, *) datafile, rows
       
       m = (b - a)/2.0
       c = (b + a)/2.0

       open(unit=4, file=datafile, status='unknown')
1      format(' ', f19.16, ' ', f19.16)
       
       total = 0.0
       do i = 1, rows
        read(4, 1) w, x
c        write(*, 1) w, x
        total = total + w * f(m*x + c)
       end do
       close(unit=4)
       
       gauss = m * total
       return
      end
