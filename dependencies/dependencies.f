c     Find the number of lines in the specified file
c
c     Params:
c     - filename: the name of the file whose lines are to be counted
      function linecount(filename)
       integer linecount
       character*31 filename

       character*63 command, fname

1      format(' ', 'wc -l ', a, ' > lines.data')
       write(command, 1) filename

c       write(*, *) command

       i = system(command)
       open(unit=4, file='lines.data', status='unknown')
       read(4, *) linecount, fname
       close(unit=4, status='delete')

       return
      end
