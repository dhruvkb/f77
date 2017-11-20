c     Calls the methods from methods.f as a demonstration
      program main
       real gausselimination, gaussseidel
       integer rows, linecount
       character*31 filename, infilename, outfilename

       write(*, *) 'Solving by Gauss elimination method'
       filename = 'cw280917.data'
       rows = linecount(filename)
       x = gausselimination(filename, rows)

       write(*, *) 'Solving by Gauss-Seidel method'
       filename = 'cw261017.data'
       rows = linecount(filename)
       x = gaussseidel(filename, rows)

       write(*, *) 'Solving by matrix inversion method'
       infilename = 'matrixinv.data'
       outfilename = 'inverse.data'
       rows = linecount(infilename)
       x = matrixinv(infilename, outfilename, rows)
       stop
      end
