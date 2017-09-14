c     Call the methods from methods.f as a demonstration
      program main
       real backwarddf, centraldf, forwarddf
       real derivative, xsquare
       integer rows, linecount, interpoldiff, result
       character*31 infn, outfn
       external xsquare
       
1      format(' ', f11.5)

       write(*, *) "Solving by backward derivative"
       derivative = backwarddf(xsquare, 1.0, 1.0e-6)
       write(*, 1) derivative

       write(*, *) "Solving by central derivative"
       derivative = centraldf(xsquare, 1.0, 1.0e-6)
       write(*, 1) derivative

       write(*, *) "Solving by forward derivative"
       derivative = forwarddf(xsquare, 1.0, 1.0e-6)
       write(*, 1) derivative

       write(*, *) "Solving by Richardson method"
       derivative = forwarddf(xsquare, 1.0, 1.0e-6)
       write(*, 1) derivative
       
       infn = 'interpoldiff.data'
       outfn = 'derivatives.data'
       rows = linecount(infn)
       result = interpoldiff(infn, outfn, rows, 1.0e-3)
       
       stop
      end

c     Returns the square of the operand
c
c     Params:
c     - x: the operand
      function xsquare(x)
       real xsquare, x
       xsquare = x ** 2.0
       return
      end
