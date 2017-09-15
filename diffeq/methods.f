c     Return the y(xn) based on df, x0 and y0 using Euler's method
c
c     Params:
c     - df: dy/dx expressed as a function of x and y
c     - x0: the initial value of x
c     - y0: the value of y at the initial value of x
c     - xn: the value of x where the function is to be determined
c     - n: the number of steps in which to break the process
c     - margin: the acceptable margin of error
      function euler(df, x0, y0, xn, n, margin)
       real euler, df, x0, y0, xn, margin
       integer n
       
       real h, y, nx, ny, prevny, diff, ndiff
       logical first
       
c       write(*, *) x0, y0, xn, n, margin

1      format(' ', f11.5, ' ', f11.5, ' ', f11.5)

       h = (xn - x0) / n

       y = y0
       diff = df(x0, y0)
       do i = 1, n
        nx = x0 + i * h
        ny = y + diff * h
        first = .TRUE.
        
        do j = 1, 100
         prevny = ny
         
         ndiff = df(nx, ny)
         ny = y + (diff + ndiff) * h / 2.0

         if (first .eqv. .TRUE.) then
          first = .FALSE.
         else
          if (abs((ny - prevny) / prevny) .lt. margin) then
           exit
          end if
         end if
        end do
        
        write(*, 1) nx, ny, ndiff
        y = ny
        diff = ndiff
       end do
       
       euler = y
       return
      end
