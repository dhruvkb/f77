c     Find the value of the function at given x based on tabular data
c
c     Params:
c     - datafile: the name of the file containging tabular data
c     - rows: the number of rows in that datafile
c     - x: the value where the function is to be approximated
      function lagrange(datafile, rows, x)
       real lagrange, x
       character*31 datafile
       integer rows

       real y, xi, yi, pi, dpi, li, term
       dimension xi(rows), yi(rows)
       
       lagrange = 0.0

       write(*, *) "Solving by Lagrange interpolation"
c       write(*, *) datafile, rows, x

       open(unit=4, file=datafile, status='unknown')
1      format(' ', f11.5, ' ', f11.5)

       pi = 1.0
       do i = 1, rows
        read(4, 1) xi(i), yi(i)
c        write(*, 1) xi(i), yi(i)

        pi = pi * (x - xi(i))
       end do
       close(unit=4) 

       y = 0.0
       do i = 1, rows
        dpi = 1.0
        do j = 1, rows
         if (i .eq. j) then
          cycle
         end if

         dpi = dpi * (xi(i) - xi(j))
        end do

        li = pi / ((x - xi(i)) * dpi)
        term = li * yi(i)
        y = y + term
       end do
        
       lagrange = y
       return
      end

c     Find the value of the function at given x based on tabular data
c
c     Params:
c     - datafile: the name of the file containging tabular data
c     - rows: the number of rows in that datafile
c     - x: the value where the function is to be approximated
      function hermite(datafile, rows, x)
       real hermite, x
       character*31 datafile
       integer rows

       real y, xi, yi, dyi, pi, dpi, li, dli, termone, termtwo
       dimension xi(rows), yi(rows), dyi(rows)

       hermite = 0.0

       write(*, *) "Solving by Hermite interpolation"
c       write(*, *) datafile, rows, x

       open(unit=4, file=datafile, status='unknown')
2      format(' ', f11.5, ' ', f11.5, ' ', f11.5)

       pi = 1.0
       do i = 1, rows
        read(4, 2) xi(i), yi(i), dyi(i)
c        write(*, 2) xi(i), yi(i), dyi(i)

        pi = pi * (x - xi(i))
       end do
       close(unit=4)
       
       y = 0.0
       do i = 1, rows
        dpi = 1.0
        dli = 0.0
        do j = 1, rows
         if (i .eq. j) then
          cycle
         end if
         
         dpi = dpi * (xi(i) - xi(j))
         dli = dli + 1/(xi(i)-xi(j))
        end do
        
        li = pi / ((x - xi(i)) * dpi)
        termone = (1 - 2*(x - xi(i))*dli) * (li**2) * yi(i)
        termtwo = (x - xi(i)) * (li**2) * dyi(i)
        
        y = y + termone + termtwo
       end do
       
       hermite = y
       return
      end

c     Find the value of the function at given x based on tabular data
c
c     Params:
c     - datafile: the name of the file containging tabular data
c     - rows: the number of rows in that datafile
c     - yop: the operation to be performed on every tabulated value of y
c     - imop: the inverse operation to perform on determined m
c     - icop: the inverse operation to perform on determined c
      function leastsquare(datafile, rows, yop, imop, icop)
       real yop, imop, icop
       character*31 datafile, leastsquare
       integer rows
       
       real xi, yi, sxi, syi, sxiyi, sxixi, m, c
       dimension xi(rows), yi(rows)
       
       write(*, *) "Solving by least-square interpolation"
c       write(*, *) datafile, rows, x
       
       open(unit=4, file=datafile, status='unknown')
31     format(' ', f11.5, ' ', f11.5)

       sxi = 0.0
       syi = 0.0
       sxiyi = 0.0
       sxixi = 0.0
       do i = 1, rows
        read(4, 31) xi(i), yi(i)
        yi(i) = yop(yi(i))
c        write(*, 31) xi(i), yi(i)

        sxi = sxi + xi(i)
        syi = syi + yi(i)
        sxiyi = sxiyi + (xi(i) * yi(i))
        sxixi = sxixi + (xi(i) ** 2)
       end do
       close(unit=4)
       
       m = (rows*sxiyi - sxi*syi) / (rows*sxixi - sxi*sxi)
       m = imop(m)
       c = (sxi*sxiyi - sxixi*syi) / (sxi*sxi - rows*sxixi)
       c = icop(c)

32     format(' ', 'm = ', f11.5, ' & c = ', f11.5)       
       write(leastsquare, 32) m, c
       return
      end
