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

C  Return true if character is +/-
      logical function signch (chr)
      implicit none
      character*1 chr
      signch = (chr.EQ.'+').OR.(chr.EQ.'-')
      return
      end

C  FIXME : Rename this function
C  Return true if character is [0,9]
      logical function digit (chr)
      implicit none
      character*1 chr
      digit = (ichar(chr).GT.47).AND.(ichar(chr).LT.58)
      return
      end

C  Return true if every character in field is valid for an integer number
      logical function ifield (field)
      implicit none
      character*(*) field
      integer i1, length, endpos
      logical digit, signch
C     An integer is at least 1 character
      length = endpos(field)
      if (length.LT.1) then
         ifield = .FALSE.
         return
      end if
C     Verify that only valid real characters
      ifield = signch(field(1:1)).OR.digit(field(1:1))
      i1 = 2
   10 if (ifield.AND.(i1.LE.length)) then
         ifield = digit(field(i1:i1))
         i1 = i1 + 1
         go to 10
      end if
      return
      end

C  Return true if character is E,D,e, or d
      logical function expch (chr)
      implicit none
      character*1 chr
      expch = (chr.EQ.'E').OR.(chr.EQ.'e')
     &    .OR.(chr.EQ.'D').OR.(chr.EQ.'d')
      return
      end

C  Return true if character is valid for a real number
      logical function realch (chr)
      implicit none
      character*1 chr
      logical digit, signch, expch
      realch = digit(chr).OR.signch(chr).OR.expch(chr).OR.(chr.EQ.'.')
      return
      end

C  Return true if every character in field is valid for a real number
      logical function rfield (field)
      implicit none
      character*(*) field
      integer i1, length, decpos, exppos, endpos
      logical digit, signch, realch
C     A real is at least 2 characters
      length = endpos(field)
      if (length.LT.2) then
         rfield = .FALSE.
         return
      end if
C     Verify that only valid real characters
      rfield = signch(field(1:1)).OR.
     &         digit(field(1:1)).OR.
     &        (field(1:1).EQ.'.')
      i1 = 2
   10 if (rfield.AND.(i1.LE.length)) then
         rfield = realch(field(i1:i1))
         i1 = i1 + 1
         go to 10
      end if
      if (.NOT.rfield) return
C     Validate the decimal position and optional exponent
      decpos = index(field,'.')
      exppos = max(index(field,'E'),index(field,'e'),
     &             index(field,'D'),index(field,'d'))
      if ((decpos.EQ.0).AND.(exppos.EQ.0)) then
         rfield = .FALSE.
         return
      else if ((exppos.GT.0).AND.(decpos.GT.exppos)) then
         rfield = .FALSE.
         return
      end if
      return
      end

C  Return the integer number parsed from string
      integer function atoi (string)
      implicit none
      character*(*) string
      integer sgn, digit
      integer i1, cursor, eol
      integer endpos
      call trim77 (string)
      cursor = 1
      eol = endpos(string)
C     Set the sign
      if (string(cursor:cursor).EQ.'-') then
         sgn = -1
         cursor = cursor + 1
      else if (string(cursor:cursor).EQ.'+') then
         sgn = 1
         cursor = cursor + 1
      else
         sgn = 1
      end if
      atoi = 0
      if (cursor.LT.eol) then
         do 10 i1 = cursor, eol
            digit = ichar(string(i1:i1)) - 48
            atoi = atoi + digit*10**(eol - i1)
   10    continue
         cursor = eol
      else if (cursor.EQ.eol) then
         atoi = ichar(string(cursor:cursor)) - 48
      else
         atoi = (sgn - sgn)/(sgn - sgn)
      end if
      return
      end

C  Return the real number parsed from string
      real function ator (string)
      implicit none
      character*(*) string
      real rsign, icomp, fcomp
      integer digit, esign, ecomp
      integer i1, cursor, eol, decpos, exppos
      integer endpos
      logical rfield
      call trim77 (string)
      if (.NOT.rfield(string)) then
         ator = (rsign - rsign)/(rsign - rsign)
         return
      end if
      cursor = 1
      eol = endpos(string)
C     Set the sign
      if (string(cursor:cursor).EQ.'-') then
         rsign = -1.0
         cursor = cursor + 1
      else if (string(cursor:cursor).EQ.'+') then
         rsign = 1.0
         cursor = cursor + 1
      else
         rsign = 1.0
      end if
C     Parse the integer component
      decpos = index(string,'.')
      exppos = max(index(string,'E'),index(string,'e'),
     &             index(string,'D'),index(string,'d'))
      icomp = 0.0
      if (cursor.LT.decpos) then
         do 10 i1 = cursor, (decpos - 1)
            digit = ichar(string(i1:i1)) - 48
            icomp = icomp + real(digit)*10.0**(decpos - i1 - 1)
   10    continue
         cursor = decpos + 1
      else if (cursor.EQ.decpos) then
         cursor = decpos + 1
      else if (cursor.LT.exppos) then
         do 11 i1 = cursor, (exppos - 1)
            digit = ichar(string(i1:i1)) - 48
            icomp = icomp + real(digit)*10.0**(exppos - i1 - 1)
   11    continue
         cursor = exppos + 1
      else
         ator = (rsign - rsign)/(rsign - rsign)
         return
      end if
C     Parse the fractional component
      fcomp = 0.0
      if (exppos.EQ.0) then
         do 20 i1 = cursor, eol
            digit = ichar(string(i1:i1)) - 48
            fcomp = fcomp + real(digit)*10.0**(cursor - i1 - 1)
   20    continue
         cursor = eol + 1
      else if (cursor.LT.exppos) then
         do 30 i1 = cursor, (exppos - 1)
            digit = ichar(string(i1:i1)) - 48
            fcomp = fcomp + real(digit)*10.0**(cursor - i1 - 1)
   30    continue
         cursor = exppos + 1
      else if (cursor.EQ.exppos) then
         cursor = exppos + 1
      end if
C     Set the exponential sign
      if (string(cursor:cursor).EQ.'-') then
         esign = -1
         cursor = cursor + 1
      else if (string(cursor:cursor).EQ.'+') then
         esign = 1
         cursor = cursor + 1
      else
         esign = 1
      end if
C     Parse the exponential component
      if (cursor.LE.eol) then
         ecomp = 0
         do 40 i1 = cursor, eol
            digit = ichar(string(i1:i1)) - 48
            ecomp = ecomp + digit*10**(eol - i1)
   40    continue
      else
         ecomp = 0
      end if
C     Calculate the real number
      ator = rsign*(icomp + fcomp)*10.0**(esign*ecomp)
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
