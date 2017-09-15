c     Calls the methods from methods.f as a demonstration
      program main
       real euler, xplusy, x, y
       external xplusy
       
       write(*, *) 'Solving by Euler method'
       y = euler(xplusy, 0.0, 1.0, 0.1, 2, 1.0e-8)
c       write(*, *) y
       
       stop
      end
      
c     Returns the sum of the operands
c
c     Params:
c     - x: the operand #1
c     - y: the operand #2
      function xplusy(x, y)
       real xplusy, x, y
       
       xplusy = x + y
       return
      end
