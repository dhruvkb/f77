c     Call the methods from methods.f as a demonstration
      program main
       real simpson, gauss
       real integral, polynomial
       integer rows, linecount
       character*31 filename
       external polynomial
       
       pi = 4.0*atan(1.0)

1      format(' ', f11.5)

       write(*, *) 'Solving by Simpson method'       
       integral = simpson(polynomial, 1.0, 4.0, 300)
       write(*, 1) integral
       
       write(*, *) 'Solving by Gauss quadrature'
       filename = 'gauss8.data'
       rows = linecount(filename)
       integral = gauss(polynomial, 1.0, 4.0, filename, rows)
       write(*, 1) integral
       
       write(*, *) 'Solving by Gauss quadrature'
       filename = 'gauss16.data'
       rows = linecount(filename)
       integral = gauss(polynomial, 1.0, 4.0, filename, rows)
       write(*, 1) integral
       
       stop
      end

c     Returns x**2 + 2*x + 1
c
c     Params:
c     - x: the operand
      function polynomial(x)
       real polynomial, x
       polynomial = x**2 + 2*x + 1
       return
      end
