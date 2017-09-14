c     Uses functions from
c     - methods.f
c       - math.f


c     Calls the various methods of finding the root(s) of f(x)=0
      program main
       external f, phi
       real soln, bisection, iteration, newtonraphson
       
1      format(' ', f10.5)

       write(*, *) 'Solving by bisection method'       
       soln = bisection(f, 0.0, 1.0, 100, 1e-7)
       write(*, 1) soln

       write(*, *) 'Solving by iteration method'
       soln = iteration(phi, 0.75, 100, 1e-7)
       write(*, 1) soln

       write(*, *) 'Solving by Newton-Raphson method'
       soln = newtonraphson(f, 0.5, 100, 1e-7)
       write(*, 1) soln

       stop
      end

c     The function whose root(s) are to be found
      function f(x)
       real f, x
       f = x**3 + x**2 - 1
       return
      end

c     Write the function f(x)=0 as x=phi(x)
c     This notation is used in iteration method
      function phi(x)
       real phi, x
       phi = 1/((x + 1)**0.5)
       return
      end
