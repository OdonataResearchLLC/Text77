## Simple Text Parsing in Fortran 77

This file contains a set of Fortran 77 routines that are useful for parsing
free format text. While Fortran 77 is obsolete, there is still a lot of
legacy code floating around. These routines are useful in cases where the
legacy code cannot be updated to a more recent version of Fortran. If you
are on Windows, the [Open Watcom Fortran compiler][Watcom] is a good choice
for a free compiler.

```
integer function begpos (string)
```

`begpos` returns the beginning position of the string.

```
integer function endpos (string)
```

`endpos` returns the end position of the string. This may be less than
`len(string)` and is the position of the last non-space character.

```
subroutine trim77 (string)
```

`trim77` modifies the string to remove all leading spaces.

```
subroutine split (field, string, delmtr, greedy)
```

`split` splits the string on the first delimiter, `delmtr`. The first field
is returned in `field` and removed from `string`. The delimiter can be
multiple characters. If `greedy` is true, repeated delimiters are skipped.
Otherwise, empty fields are returned.

```
logical function is_int (field)
```

`is_int` returns true if the field is comprised of an integer.

```
integer function atoi (field)
```

`atoi` returns an integer from a character field.

```
logical function isreal (field)
```

`isreal` returns true if the field is comprised of a real number.

```
real function ator (field)
```

`ator` returns a real value from a character field.

```
logical function is_dlb (field)
```

`is_dbl` returns true if the field is comprised of a double precision number.

```
real function atod (field)
```

`atod` returns a double precision value from a character field.

```
integer function txtios ()
```

`txtios` returns the IO status, `IOSTAT`, of the last function. Applicable to
`is_int`, `atoi`, `isreal`, `ator`, `is_dbl`, and `atod`.

```
subroutine ascii ()
```

`ascii` prints the ASCII collating sequence table to standard output.

[Watcom]: <http://www.openwatcom.org/> "Open Watcom"
