C  Return the beginning position of the string
      integer function begpos (string)
      implicit none
      integer endpos
      character*(*) string
      begpos = 1
      endpos = len(string)
   10 if ((ichar(string(begpos:begpos)).EQ.32).AND.(begpos.LT.endpos))
     & then
         begpos = begpos + 1
         goto 10
      else if (begpos.EQ.endpos) then
         begpos = 0
      end if
      return
      end

C  Return the ending position of the string
      integer function endpos (string)
      implicit none
      character*(*) string
      endpos = len(string)
   10 if ((ichar(string(endpos:endpos)).EQ.32).AND.(endpos.gt.0)) then
         endpos = endpos - 1
         go to 10
      end if
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
      integer endstr, enddel, delpos
C  External functions
      integer endpos
C  Initialization
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
         if (greedy) then
   10       endstr = endpos(string)
            delpos = index(string,delmtr(1:enddel))
            if ((delpos.EQ.1).AND.(delpos.LT.endstr)) then
               string = string(delpos+enddel:max(delpos+enddel,endstr))
               go to 10
            end if
         end if
C  Empty field
      else if (delpos.EQ.1) then
         field = char(32)
         string = string(enddel+1:)
         if (greedy) then
   20       endstr = endpos(string)
            delpos = index(string,delmtr(1:enddel))
            if ((delpos.EQ.1).AND.(delpos.LT.endstr)) then
               string = string(delpos+enddel:max(delpos+enddel,endstr))
               go to 20
            end if
         end if
C  No delimiter
      else if (delpos.EQ.0) then
         field = string
         string = char(32)
      end if
      return
      end

C  Print the ASCII collating sequence table to standard output
      subroutine ascii ()
      implicit none
      integer i, j, code
      code(i,j) = 32 + 8*i + j
      print *
      do 10 i = 0, 11
         print 20, (code(i,j), char(code(i,j)), j = 0, 7)
   10 continue
      print *
   20 format (8(1x,I3,':',A1))
      end
