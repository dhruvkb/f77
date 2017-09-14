c     Apply the bisection method to solve the given equation
c     
c     Params:
c     - f: the function to find roots for
c     - lowerlimit: the root must be greater than lowerlimit
c     - upperlimit: the root must be lesser than lowerlimit
c     - maxiter: the maximum number of iterations to try
c     - margin: the least significant difference between two iterations
      function bisection(f, lowerlimit, upperlimit, maxiter, margin)
       real bisection, lowerlimit, upperlimit, margin
       integer maxiter
       real prevmid, mid, fmid, ll, fll, ul, ful
       logical first, found

       write(*, *) "Solving by bisection method"
c       write(*, *) ll, ul, maxiter, margin

       ll = lowerlimit
       ul = upperlimit
       fll = f(ll)
       ful = f(ul)
       first = .TRUE.
       found = .FALSE.

       do i = 1, maxiter
        prevmid = mid

        mid = (ll + ul) / 2.0
        fmid = f(mid)
        
        if (first .eqv. .TRUE.) then
         first = .FALSE.
        else
         if (abs((mid - prevmid) / prevmid) .lt. margin) then
          found = .TRUE.
          exit
         end if
        end if

        if ((fmid*ful) .lt. 0) then
         ll = mid
         fll = fmid
        else
         ul = mid
         ful = fmid
        end if
       end do

       if (found .eqv. .TRUE.) then
        bisection = mid
       else
        write(*, *) "Could not find a solution"
        bisection = -1.0
       end if

       return
      end

c     Apply the iteration method to solve the given equation
c
c     Params:
c     - phi: the function to find roots for, expressed as x=phi(x)
c     - start: the initial guess value of the root
c     - maxiter: the maximum number of iterations to try
c     - margin: the least significant difference between two iterations
      function iteration(phi, start, maxiter, margin)
       real iteration, start, margin
       integer maxiter
       real prevx, x
       logical first, found

       write(*, *) "Solving by iteration method"
c       write(*, *) start, maxiter, margin

       x = start
       first = .TRUE.
       found = .FALSE.

       do i = 1, maxiter
        prevx = x
        x = phi(x)

        if (first .eqv. .TRUE.) then
         first = .FALSE.
        else
         if (abs((x - prevx) / prevx) .lt. margin) then
          found = .TRUE.
          exit
         end if
        end if
       end do
   
       if (found .eqv. .TRUE.) then
        iteration = x       
       else
        write(*, *) "Could not find a solution"
        iteration = -1.0
       end if

       return
      end

c     Apply the Newton-Raphson method to solve the given equation
c
c     Params:
c     - f: the function to find roots for
c     - start: the initial guess value of the root
c     - maxiter: the maximum number of iterations to try
c     - margin: the least significant difference between two iterations
      function newtonraphson(f, start, maxiter, margin)
       real newtonraphson, start, margin
       integer maxiter
       external f
       real prevx, x, fx, dfx
       logical first, found
       
       write(*, *) "Solving by Newton-Raphson method"
c       write(*, *) start, maxiter, margin

       x = start
       first = .TRUE.

       do i = 1, maxiter
        prevx = x
        fx = f(x)
        dfx = centraldf(f, x, 1.0e-6)
        x = x - fx/dfx
        
        if (first .eqv. .TRUE.) then
         first = .FALSE.
        else
         if (abs((x - prevx) / prevx) .lt. margin) then
          found = .TRUE.
          exit
         end if
        end if
       end do

       if (found .eqv. .TRUE.) then
        newtonraphson = x
       else
        write(*, *) "Could not find a solution"
        newtonraphson = -1.0
       end if

       return
      end
