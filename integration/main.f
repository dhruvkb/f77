c     Call the methods from methods.f as a demonstration
      program main
       real simpson, gauss
       real integral, cw240817, cw310817_1, cw310817_2
       integer rows, linecount
       character*31 filename
       external cw240817, cw310817_1, cw310817_2
       
       pi = 4.0*atan(1.0)

1      format(' ', f11.5)

       write(*, *) 'Solving by Simpson method'       
       integral = simpson(cw240817, 0.0, 4.0, 300)
       write(*, 1) integral
       
       write(*, *) 'Solving by Gauss quadrature'
       filename = 'gauss8.data'
       rows = linecount(filename)
       integral = gauss(cw310817_1, 1.0, 4.0, filename, rows)
       write(*, 1) integral
       
       write(*, *) 'Solving by Gauss quadrature'
       filename = 'gauss16.data'
       rows = linecount(filename)
       integral = gauss(cw310817_2, 0.0, pi/4.0, filename, rows)
       write(*, 1) integral
       
       stop
      end

c     Returns x*e^x + x^2
c
c     Params:
c     - x: the operand
      function cw240817(x)
       real cw240817, x
       cw240817 = x*exp(x) + x**2
       return
      end

c     Returns x**2 + 2*x + 1
c
c     Params:
c     - x: the operand
      function cw310817_1(x)
       real cw310817_1, x
       cw310817_1 = x**2 + 2*x + 1
       return
      end

c     Returns sin^2(x) + x*sin(x)
c
c     Params:
c     - x: the operand
      function cw310817_2(x)
       real cw310817_2, x
       cw310817_2 = sin(x)**2 + x*sin(x)
       return
      end 
