C  Return the beginning position of the string
      integer function begpos (string)
      implicit none
      integer endpos
      character*(*) string
      begpos = 1
      endpos = len(string)
      do while
     & ((ichar(string(begpos:begpos)).EQ.32).AND.(begpos.LT.endpos))
         begpos = begpos + 1
      end do
      if (begpos.EQ.endpos) begpos = 0
      return
      end

C  Return the ending position of the string
      integer function endpos (string)
      implicit none
      character*(*) string
      endpos = len(string)
      do while
     & ((ichar(string(endpos:endpos)).EQ.32).AND.(endpos.gt.0))
         endpos = endpos - 1
      end do
      return
      end

C  Trim whitespace from the beginning of the string.
      subroutine trim77 (string)
      implicit none
      character*(*) string
      integer begpos
      string = string(begpos(string):)
      return
      end

C  Split the string at the first delimiter.
C  Input : delmtr, characters delimiting a field
C        : string, delimited string
C  Output : string, string with first field removed
C        : field, first delimited field in string
      subroutine split (field, string, delmtr, greedy)
      implicit none
C  Argument variables
      character*(*) field, string, delmtr
      logical greedy
C  Local variables
      logical skip
      integer endstr, enddel, delpos
C  External functions
      integer endpos
C  Initialization
      skip = greedy
      endstr = endpos(string)
      enddel = max(1,endpos(delmtr))
      delpos = index(string,delmtr(1:enddel))
C  SPACE delimiter at the end of the field.
      if (delpos.GT.endstr) then
         field = string(1:endstr)
         string = char(32)
C  Standard field
      else if (delpos.GT.1) then
         field = string(1:delpos-1)
         string = string(delpos+enddel:max(delpos+enddel,endstr))
C        Consume repeated delimiters
         do while (skip)
            endstr = endpos(string)
            delpos = index(string,delmtr(1:enddel))
            if ((delpos.EQ.1).AND.(delpos.LT.endstr)) then
               string = string(delpos+enddel:max(delpos+enddel,endstr))
            else
               skip = .FALSE.
            end if
         end do
C  Empty field
      else if (delpos.EQ.1) then
         field = char(32)
         string = string(enddel+1:)
         do while (skip)
            endstr = endpos(string)
            delpos = index(string,delmtr(1:enddel))
            if ((delpos.EQ.1).AND.(delpos.LT.endstr)) then
               string = string(delpos+enddel:max(delpos+enddel,endstr))
            else
               skip = .FALSE.
            end if
         end do
C  No delimiter
      else if (delpos.EQ.0) then
         field = string
         string = char(32)
      end if
      return
      end

C  Return true if the field contains an integer.
      logical function is_int (field)
      implicit none
      character*(*) field
      integer istat, scratch
      read (field,'(I11)',iostat=istat) scratch
      is_int = istat.EQ.0
      return
      end

C  Return an integer from the field
      integer function atoi (field)
      implicit none
      character*(*) field
      read (field,'(I11)') atoi
      return
      end

C  Return true if the field contains a real.
      logical function isreal (field)
      implicit none
      character*(*) field
      integer istat
      real scratch
      read (field,'(F16.0)',iostat=istat) scratch
      isreal = istat.EQ.0
      return
      end

C  Return a real from the field
      real function ator (field)
      implicit none
      character*(*) field
      read (field,'(F16.0)') ator
      return
      end

C  Print the ASCII collating sequence table to standard output
      subroutine ascii ()
      implicit none
      integer i, j, code
      code(i,j) = 32 + 8*i + j
      print *
      do i = 0, 11
         print 10, (code(i,j), char(code(i,j)), j = 0, 7)
      end do
      print *
   10 format (8(1x,I3,':',A1))
      end
