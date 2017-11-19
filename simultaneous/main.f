c     Calls the methods from methods.f as a demonstration
      program main
       real gausselimination, gaussseidel
       integer rows, linecount
       character*31 filename

       write(*, *) 'Solving by Gauss elimination method'
       filename = 'cw281017.data'
       rows = linecount(filename)
       x = gausselimination(filename, rows)

       write(*, *) 'Solving by Gauss-Seidel method'
       filename = 'cw261017.data'
       rows = linecount(filename)
       x = gaussseidel(filename, rows)

       stop
      end
