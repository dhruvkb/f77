c     Call the methods from methods.f as a demonstration
      program main
       real lagrange, hermite, y
       integer rows, linecount
       character*31 filename, leastsquare, s
       external asitis, ln, epow

1      format(' ', f11.5)

       filename = 'lagrange.data'
       rows = linecount(filename)
       y = lagrange(filename, rows, 0.0)
       write(*, 1) y

       filename = 'hermite.data'
       rows = linecount(filename)
       y = hermite(filename, rows, 2.7)
       write(*, 1) y

       filename = 'cw170817-2.data'
       rows = linecount(filename)
       s = leastsquare(filename, rows, ln, asitis, epow)
       write(*, *) s

       stop
      end
      
c     Returns the value of the operand as it is  
c
c     Params:
c     - y: the operand    
      function asitis(y)
       real asitis, y
       asitis = y
       return
      end

c     Returns the natural logarithm of the operand
c
c     Params:
c     - y: the operand
      function ln(y)
       real ln, y, log
       ln = log(y)
       return
      end         

c     Returns the value of Euler's number raised to the operand
c
c     Params:
c     - y: the operand
      function epow(y)
       real epow, y, exp
       epow = exp(y)
       return
      end
