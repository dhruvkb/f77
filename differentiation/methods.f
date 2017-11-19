c     Calculate the backward leaning derivative of a function at a point
c
c     Params:
c     - f: the function to differentiate
c     - x: the point where to find the derivative
c     - h: the small difference for calculating the derivative
      function backwarddf(f, x, h)
       real backwarddf, f, x, h
       
       backwarddf = (f(x) - f(x-h))/h
       return
      end

c     Calculate the central derivative of a function at a point
c
c     Params:
c     - f: the function to differentiate
c     - x: the point where to find the derivative
c     - h: the small difference for calculating the derivative
      function centraldf(f, x, h)
       real centraldf, f, x, h
       
       centraldf = (f(x+h) - f(x-h))/(2*h)
       return
      end

c     Calculate the forward leaning derivative of a function at a point
c
c     Params:
c     - f: the function to differentiate
c     - x: the point where to find the derivative
c     - h: the small difference for calculating the derivative
      function forwarddf(f, x, h)
       real forwarddf, f, x, h
       
       forwarddf = (f(x+h) - f(x))/h
       return
      end

c     Calculate the Richardson derivative of a function at a point
c
c     Params:
c     - f: the function to differentiate
c     - x: the point where to find the derivative
c     - h: the small difference for calculating the derivative
      function richardson(f, x, h)
       real richardson, f, x, h
       external f
       
       richardson = 4.0*centraldf(f, x, h/2) - centraldf(f, x, h)
       richardson = richardson / 3.0
       return
      end

c     Interpolate the given data and then find the derivative at a point
c
c     Params:
c     - indatafile: the file containing tabulated x and y = f(x)
c     - outdatafile: the file containing tabulated x and dy = f'(x)
c     - rows: the number of rows in indatafile (and outdatafile)
c     - h: the small difference for calculating the derivative
      function interpoldiff(indatafile, outdatafile, rows, h)
       real h
       character*31 indatafile, outdatafile
       integer interpoldiff, rows
       
       real x, xph, xmh, y, yxph, yxmh, dy, lagrange
       
       open(unit=7, file=indatafile, status='unknown')
       open(unit=8, file=outdatafile, status='unknown')
1      format(' ', f11.5, ' ', f11.5)
       
       do i = 1, rows
        read(7, 1) x, y
c        write(*, 1) x, y

        xph = x + h
        xmh = x - h
        yxph = lagrange(indatafile, rows, xph)
        yxmh = lagrange(indatafile, rows, xmh)
        dy = (yxph - yxmh) / (2.0 * h)
        
        write(8, 1) x, dy
       end do
       close(unit=8)
       close(unit=7)
       
       interpoldiff = 1
       return
      end
