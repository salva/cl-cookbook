Strings
=======

The most important thing to know about strings in Common Lisp is
probably that they are arrays and thus also sequences. This implies that
*all* concepts that are applicable to arrays and sequences also apply to
strings. If you can't find a particular string function, make sure
you've also searched for the more general array or sequence functions.
We'll only cover a fraction of what can be done with and to strings
here.

## Accessing Substrings

As a string is a sequence, you can access substrings with the
[`SUBSEQ`](http://www.lispworks.com/documentation/HyperSpec/Body/f_subseq.htm)
function. The index into the string is, as always, zero-based. The
third, optional, argument is the index of the first character which is
not a part of the substring, it is *not* the length of the substring.

    * (defparameter *my-string* (string "Groucho Marx"))
    *MY-STRING*
    * (subseq *my-string* 8)
    "Marx"
    * (subseq *my-string* 0 7)
    "Groucho"
    * (subseq *my-string* 1 5)
    "rouc"

You can also manipulate the substring if you use `SUBSEQ` together with
[`SETF`](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm).

    * (defparameter *my-string* (string "Harpo Marx"))
    *MY-STRING*
    * (subseq *my-string* 0 5)
    "Harpo"
    * (setf (subseq *my-string* 0 5) "Chico")
    "Chico"
    * *my-string*
    "Chico Marx"

But note that the string isn't "stretchable". To cite from the
[HyperSpec](http://www.lispworks.com/documentation/HyperSpec/index.html):
"If the subsequence and the new sequence are not of equal length, the
shorter length determines the number of elements that are replaced." For
example:

    * (defparameter *my-string* (string "Karl Marx"))
    *MY-STRING*
    * (subseq *my-string* 0 4)
    "Karl"
    * (setf (subseq *my-string* 0 4) "Harpo")
    "Harpo"
    * *my-string*
    "Harp Marx"
    * (subseq *my-string* 4)
    " Marx"
    * (setf (subseq *my-string* 4) "o Marx")
    "o Marx"
    * *my-string*
    "Harpo Mar"

## Accessing Individual Characters

You can use the function
[`CHAR`](http://www.lispworks.com/documentation/HyperSpec/Body/f_char_.htm)
to access individual characters of a string. `CHAR` can also be used in
conjunction with `SETF`.

    * (defparameter *my-string* (string "Groucho Marx"))
    *MY-STRING*
    * (char *my-string* 11)
    #\x
    * (char *my-string* 7)
    #\Space
    * (char *my-string* 6)
    #\o
    * (setf (char *my-string* 6) #\y)
    #\y
    * *my-string*
    "Grouchy Marx"

Note that there's also
[`SCHAR`](http://www.lispworks.com/documentation/HyperSpec/Body/f_char_.htm).
If efficiency is important, `SCHAR` can be a bit faster where
appropriate.

Because strings are arrays and thus sequences, you can also use the more
generic functions
[`AREF`](http://www.lispworks.com/documentation/HyperSpec/Body/f_aref.htm)
and
[`ELT`](http://www.lispworks.com/documentation/HyperSpec/Body/f_elt.htm)
(which are more general while `CHAR` might be implemented more
efficiently).

    * (defparameter *my-string* (string "Groucho Marx"))
    *MY-STRING*
    * (aref *my-string* 3)
    #\u
    * (elt *my-string* 8)
    #\M

Each character in a string has an integer code. The range of recognized
codes and Lisp's ability to print them is directed related to your
implementation's character set support, e.g. ISO-8859-1, or Unicode.
Here are some examples in [SBCL](http://www.sbcl.org) of UTF-8 which
encodes characters as 1 to 4 8 bit bytes. The first example shows a
character outside the first 128 chars, or what is considered the normal
Latin character set. The second example shows a multibyte encoding
(beyond the value 255). Notice the Lisp reader can round-trip characters
by name.

    * (stream-external-format *standard-output*)

    :UTF-8
    * (code-char 200)

    #\LATIN_CAPITAL_LETTER_E_WITH_GRAVE
    * (char-code #\LATIN_CAPITAL_LETTER_E_WITH_GRAVE)

    200
    * (code-char 1488)
    #\HEBREW_LETTER_ALEF

    * (char-code #\HEBREW_LETTER_ALEF)
    1488

Check out the
[`UTF-8 Wikipedia article`](http://en.wikipedia.org/wiki/UTF-8) for the
range of supported characters and their encodings.

## Manipulating Parts of a String

There's a slew of (sequence) functions that can be used to manipulate a
string and we'll only provide some examples here. See the [sequences
dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_sequen.htm)
in the HyperSpec for more.

    * (remove #\o "Harpo Marx")
    "Harp Marx"
    * (remove #\a "Harpo Marx")
    "Hrpo Mrx"
    * (remove #\a "Harpo Marx" :start 2)
    "Harpo Mrx"
    * (remove-if #'upper-case-p "Harpo Marx")
    "arpo arx"
    * (substitute #\u #\o "Groucho Marx")
    "Gruuchu Marx"
    * (substitute-if #\_ #'upper-case-p "Groucho Marx")
    "_roucho _arx"
    * (defparameter *my-string* (string "Zeppo Marx"))
    *MY-STRING*
    * (replace *my-string* "Harpo" :end1 5)
    "Harpo Marx"
    * *my-string*
    "Harpo Marx"

Another function that can be frequently used (but not part of the ANSI
standard) is `replace-all`. This function provides an easy functionality
for search/replace operations on a string, by returning a new string in
which all the occurences of the 'part' in string is replaced with
'replacement'".

    * (replace-all "Groucho Marx Groucho" "Groucho" "ReplacementForGroucho")
    "ReplacementForGroucho Marx ReplacementForGroucho"

One of the implementations of `replace-all is as follows: `

    (defun replace-all (string part replacement &key (test #'char=))
    "Returns a new string in which all the occurences of the part 
    is replaced with replacement."
        (with-output-to-string (out)
          (loop with part-length = (length part)
                for old-pos = 0 then (+ pos part-length)
                for pos = (search part string
                                  :start2 old-pos
                                  :test test)
                do (write-string string out
                                 :start old-pos
                                 :end (or pos (length string)))
                when pos do (write-string replacement out)
                while pos))) 

However, bear in mind that the above code is not optimized for long
strings; if you intend to perform such an operation on very long
strings, files, etc. please consider using
[cl-ppcre](http://www.weitz.de/cl-ppcre/) regular expressions and string
processing library which is heavily optimized.

## Concatenating Strings

The name says it all:
[`CONCATENATE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_concat.htm)
is your friend. Note that this a generic sequence function and you have
to provide the result type as the first argument.

    * (concatenate 'string "Karl" " " "Marx")
    "Karl Marx"
    * (concatenate 'list "Karl" " " "Marx")
    (#\K #\a #\r #\l #\Space #\M #\a #\r #\x)

If you have to construct a string out of many parts, all of these calls
to `CONCATENATE` seem wasteful, though. There are at least three other
good ways to construct a string piecemeal, depending on what exactly
your data is. If you build your string one character at a time, make it
an *adjustable* VECTOR (a one-dimensional
[ARRAY](http://www.lispworks.com/documentation/HyperSpec/Body/15_.htm))
of type character with a fill-pointer of zero, then use
[`VECTOR-PUSH-EXTEND`](http://www.lispworks.com/documentation/HyperSpec/Body/f_vec_ps.htm)
on it. That way, you can also give hints to the system if you can
estimate how long the string will be. (See the optional third argument
to `VECTOR-PUSH-EXTEND`.)

    * (defparameter *my-string* (make-array 0
                                            :element-type 'character
                                            :fill-pointer 0
                                            :adjustable t))
    *MY-STRING*
    * *my-string*
    ""
    * (dolist (char '(#\Z #\a #\p #\p #\a))
        (vector-push-extend char *my-string*))
    NIL
    * *my-string*
    "Zappa"

If the string will be constructed out of (the printed representations
of) arbitrary objects, (symbols, numbers, characters, strings, ...), you
can use
[`FORMAT`](http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm)
with an output stream argument of `NIL`. This directs `FORMAT` to return
the indicated output as a string.

    * (format nil "This is a string with a list ~A in it"
              '(1 2 3))
    "This is a string with a list (1 2 3) in it"

We can use the looping constructs of the `FORMAT` mini language to
emulate `CONCATENATE`.

    * (format nil "The Marx brothers are:~{ ~A~}."
              '("Groucho" "Harpo" "Chico" "Zeppo" "Karl"))
    "The Marx brothers are: Groucho Harpo Chico Zeppo Karl."

`FORMAT` can do a lot more processing but it has a relatively arcane
syntax. After this last example, you can find the details in [the CLHS
section about formatted
output](http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm).

    * (format nil "The Marx brothers are:~{ ~A~^,~}."
              '("Groucho" "Harpo" "Chico" "Zeppo" "Karl"))
    "The Marx brothers are: Groucho, Harpo, Chico, Zeppo, Karl."

Another way to create a string out of the printed representation of
various object is using
[`WITH-OUTPUT-TO-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_out_.htm).
The value of this handy macro is a string containing everything that was
output to the string stream within the body to the macro. This means you
also have the full power of
[`FORMAT`](http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm)
at your disposal, should you need it.

    * (with-output-to-string (stream)
        (dolist (char '(#\Z #\a #\p #\p #\a #\, #\Space))
          (princ char stream))
        (format stream "~S - ~S" 1940 1993))
    "Zappa, 1940 - 1993"

## Processing a String One Character at a Time

Use the
[`MAP`](http://www.lispworks.com/documentation/HyperSpec/Body/f_map.htm)
function to process a string one character at a time.

    * (defparameter *my-string* (string "Groucho Marx"))
    *MY-STRING*
    * (map 'string #'(lambda (c) (print c)) *my-string*)
    #\G 
    #\r 
    #\o 
    #\u 
    #\c 
    #\h 
    #\o 
    #\Space 
    #\M 
    #\a 
    #\r 
    #\x 
    "Groucho Marx"

Or do it with
[`LOOP`](http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm).

    * (loop for char across "Zeppo"
            collect char)
    (#\Z #\e #\p #\p #\o)

## Reversing a String by Word or Character

Reversing a string by character is easy using the built-in
[`REVERSE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_revers.htm)
function (or its
[destructive](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#destructive)
counterpart `NREVERSE`).

    *(defparameter *my-string* (string "DSL"))
    *MY-STRING*
    * (reverse *my-string*)
    "LSD"

There's no one-liner in CL to reverse a string by word (like you would
do it in Perl with `split` and `join`). You either have to use function
from an external library like
[`SPLIT-SEQUENCE`](http://ww.telent.net/cliki/SPLIT-SEQUENCE) or you
have to roll your own solution. Here's an attempt:

    * (defun split-by-one-space (string)
        "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
        (loop for i = 0 then (1+ j)
              as j = (position #\Space string :start i)
              collect (subseq string i j)
              while j))
    SPLIT-BY-ONE-SPACE
    * (split-by-one-space "Singing in the rain")
    ("Singing" "in" "the" "rain")
    * (split-by-one-space "Singing in the  rain")
    ("Singing" "in" "the" "" "rain")
    * (split-by-one-space "Cool")
    ("Cool")
    * (split-by-one-space " Cool ")
    ("" "Cool" "")
    * (defun join-string-list (string-list)
        "Concatenates a list of strings
    and puts spaces between the elements."
        (format nil "~{~A~^ ~}" string-list))
    JOIN-STRING-LIST
    * (join-string-list '("We" "want" "better" "examples"))
    "We want better examples"
    * (join-string-list '("Really"))
    "Really"
    * (join-string-list '())
    ""
    * (join-string-list
       (nreverse
        (split-by-one-space
         "Reverse this sentence by word")))
    "word by sentence this Reverse"

## Controlling Case

Common Lisp has a couple of [functions to control the case of a
string](http://www.lispworks.com/documentation/HyperSpec/Body/f_stg_up.htm).

    * (string-upcase "cool")
    "COOL"
    * (string-upcase "Cool")
    "COOL"
    * (string-downcase "COOL")
    "cool"
    * (string-downcase "Cool")
    "cool"
    * (string-capitalize "cool")
    "Cool"
    * (string-capitalize "cool example")
    "Cool Example"

These functions take `:START` and `:END` keyword arguments so you can
optionally only manipulate a part of the string. They also have
destructive counterparts whose names starts with "N".

    * (string-capitalize "cool example" :start 5)
    "cool Example"
    * (string-capitalize "cool example" :end 5)
    "Cool example"
    * (defparameter *my-string* (string "BIG"))
    *MY-STRING*
    * (defparameter *my-downcase-string* (nstring-downcase *my-string*))
    *MY-DOWNCASE-STRING*
    * *my-downcase-string*
    "big"
    * *my-string*
    "big"

Note this potential caveat: According to the HyperSpec, "for
STRING-UPCASE, STRING-DOWNCASE, and STRING-CAPITALIZE, *string* is not
modified. However, if no characters in string require conversion, the
result may be either *string* or a copy of it, at the implementation's
discretion." This implies the last result in the following example is
implementation-dependent - it may either be "BIG" or "BUG". If you want
to be sure, use
[`COPY-SEQ`](http://www.lispworks.com/documentation/HyperSpec/Body/f_cp_seq.htm).

    * (defparameter *my-string* (string "BIG"))
    *MY-STRING*
    * (defparameter *my-upcase-string* (string-upcase *my-string*))
    *MY-UPCASE-STRING*
    * (setf (char *my-string* 1) #\U)
    #\U
    * *my-string*
    "BUG"
    * *my-upcase-string*
    "BIG"

## Trimming Blanks from the Ends of a String

Not only can you trim blanks, but you can get rid of arbitary
characters. The functions [`STRING-TRIM`, `STRING-LEFT-TRIM` and
`STRING-RIGHT-TRIM`](http://www.lispworks.com/documentation/HyperSpec/Body/f_stg_tr.htm)
return a substring of their second argument where all characters that
are in the first argument are removed off the beginning and/or the end.
The first argument can be any *sequence* of characters.

    * (string-trim " " " trim me ")
    "trim me"
    * (string-trim " et" " trim me ")
    "rim m"
    * (string-left-trim " et" " trim me ")
    "rim me "
    * (string-right-trim " et" " trim me ")
    " trim m"
    * (string-right-trim '(#\Space #\e #\t) " trim me ")
    " trim m"
    * (string-right-trim '(#\Space #\e #\t #\m) " trim me ")

Note: The caveat mentioned in the section about [Controlling
Case](#case) also applies here.

## Converting between Symbols and Strings

The function
[`INTERN`](http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm)
will "convert" a string to a symbol. Actually, it will check whether the
symbol denoted by the string (its first argument) is already accessible
in the package (its second, optional, argument which defaults to the
current package) and enter it, if necessary, into this package. It is
beyond the scope of this chapter to explain all the concepts involved
and to address the second return value of this function. See [the CLHS
chapter about
packages](http://www.lispworks.com/documentation/HyperSpec/Body/11_.htm)
for details.

Note that the case of the string is relevant.

    * (in-package "COMMON-LISP-USER")
    #<The COMMON-LISP-USER package, 35/44 internal, 0/9 external>
    * (intern "MY-SYMBOL")
    MY-SYMBOL
    NIL
    * (intern "MY-SYMBOL")
    MY-SYMBOL
    :INTERNAL
    * (export 'MY-SYMBOL)
    T
    * (intern "MY-SYMBOL")
    MY-SYMBOL
    :EXTERNAL
    * (intern "My-Symbol")
    |My-Symbol|
    NIL
    * (intern "MY-SYMBOL" "KEYWORD")
    :MY-SYMBOL
    NIL
    * (intern "MY-SYMBOL" "KEYWORD")
    :MY-SYMBOL
    :EXTERNAL

To do the opposite, convert from a symbol to a string, use
[SYMBOL-NAME](http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_2.htm)
or
[`STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm).

    * (symbol-name 'MY-SYMBOL)
    "MY-SYMBOL"
    * (symbol-name 'my-symbol)
    "MY-SYMBOL"
    * (symbol-name '|my-symbol|)
    "my-symbol"
    * (string 'howdy)
    "HOWDY"

## Converting between Characters and Strings

You can use
[`COERCE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_coerce.htm)
to convert a string of length 1 to a character. You can also use
`COERCE` to convert any sequence of characters into a string. You can
*not* use `COERCE` to convert a character to a string, though - you'll
have to use
[`STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm)
instead.

    * (coerce "a" 'character)
    #\a
    * (coerce (subseq "cool" 2 3) 'character)
    #\o
    * (coerce "cool" 'list)
    (#\c #\o #\o #\l)
    * (coerce '(#\h #\e #\y) 'string)
    "hey"
    * (coerce (nth 2 '(#\h #\e #\y)) 'character)
    #\y
    * (defparameter *my-array* (make-array 5 :initial-element #\x))
    *MY-ARRAY*
    * *my-array*
    #(#\x #\x #\x #\x #\x)
    * (coerce *my-array* 'string)
    "xxxxx"
    * (string 'howdy)
    "HOWDY"
    * (string #\y)
    "y"
    * (coerce #\y 'string)
    #\y can't be converted to type STRING.
       [Condition of type SIMPLE-TYPE-ERROR]

## Finding an Element of a String

Use
[`FIND`](http://www.lispworks.com/documentation/HyperSpec/Body/f_find_.htm),
[`POSITION`](http://www.lispworks.com/documentation/HyperSpec/Body/f_pos_p.htm),
and their `-IF` counterparts to find characters in a string.

    * (find #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
    #\t
    * (find #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
    #\T
    * (find #\z "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
    NIL
    * (find-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
    #\1
    * (find-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :from-end t)
    #\0
    * (position #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
    17
    * (position #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
    0
    * (position-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
    37
    * (position-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :from-end t)
    43

Or use
[`COUNT`](http://www.lispworks.com/documentation/HyperSpec/Body/f_countc.htm)
and friends to count characters in a string.

    * (count #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
    2
    * (count #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
    3
    * (count-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
    6
    * (count-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :start 38)
    5

## Finding a Substring of a String

The function
[`SEARCH`](http://www.lispworks.com/documentation/HyperSpec/Body/f_search.htm)
can find substrings of a string.

    * (search "we" "If we can't be free we can at least be cheap")
    3
    * (search "we" "If we can't be free we can at least be cheap" :from-end t)
    20
    * (search "we" "If we can't be free we can at least be cheap" :start2 4)
    20
    * (search "we" "If we can't be free we can at least be cheap" :end2 5 :from-end t)
    3
    * (search "FREE" "If we can't be free we can at least be cheap")
    NIL
    * (search "FREE" "If we can't be free we can at least be cheap" :test #'char-equal)
    15

## Converting a String to a Number

CL provides the
[`PARSE-INTEGER`](http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm)
to convert a string representation of an integer to the corresponding
numeric value. The second return value is the index into the string
where the parsing stopped.

    * (parse-integer "42")
    42
    2
    * (parse-integer "42" :start 1)
    2
    2
    * (parse-integer "42" :end 1)
    4
    1
    * (parse-integer "42" :radix 8)
    34
    2
    * (parse-integer " 42 ")
    42
    3
    * (parse-integer " 42 is forty-two" :junk-allowed t)
    42
    3
    * (parse-integer " 42 is forty-two")

    Error in function PARSE-INTEGER:
       There's junk in this string: " 42 is forty-two".

`PARSE-INTEGER` doesn't understand radix specifiers like `#X`, nor is
there a built-in function to parse other numeric types. You could use
[`READ-FROM-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_fro.htm)
in this case, but be aware that the full
[reader](http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm)
is in effect if you're using this function.

    * (read-from-string "#X23")
    35
    4
    * (read-from-string "4.5")
    4.5
    3
    * (read-from-string "6/8")
    3/4
    3
    * (read-from-string "#C(6/8 1)")
    #C(3/4 1)
    9
    * (read-from-string "1.2e2")
    120.00001
    5
    * (read-from-string "symbol")
    SYMBOL
    6
    * (defparameter *foo* 42)
    *FOO*
    * (read-from-string "#.(setq *foo* \"gotcha\")")
    "gotcha"
    23
    * *foo*
    "gotcha"

## Converting a Number to a String

The general function
[`WRITE-TO-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm)
or one of its simpler variants `PRIN1-TO-STRING` or `PRINC-TO-STRING`
may be used to convert a number to a string. With `WRITE-TO-STRING`, the
:base keyword argument may be used to change the output base for a
single call. To change the output base globally, set `*print-base*`
which defaults to 10. Remember in Lisp, rational numbers are represented
as quotients of two integers even when converted to strings.

    * (write-to-string 250)
    "250"
    * (write-to-string 250.02)
    "250.02"
    * (write-to-string 250 :base 5)
    "2000"
    * (write-to-string (/ 1 3))
    "1/3"
    * 

## Comparing Strings

The general functions
[`EQUAL`](http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm)
and
[`EQUALP`](http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm)
can be used to test whether two strings are equal. The strings are
compared element-by-element, either in a case-sensitive manner (`EQUAL`)
or not (`EQUALP`). There's also a bunch of [string-specific comparison
functions](http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm).
You'll want to use these if you're deploying *implementation-defined*
attributes of characters. Check your vendor's documentation in this
case.

Here are a few examples. Note that all functions that test for
inequality return the position of the first mismatch as a [generalized
boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean).
You can also use the generic sequence function
[`MISMATCH`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mismat.htm)
if you need more versatility.

    * (string= "Marx" "Marx")
    T
    * (string= "Marx" "marx")
    NIL
    * (string-equal "Marx" "marx")
    T
    * (string< "Groucho" "Zeppo")
    0
    * (string< "groucho" "Zeppo")
    NIL
    * (string-lessp "groucho" "Zeppo")
    0
    * (mismatch "Harpo Marx" "Zeppo Marx" :from-end t :test #'char=)
    3

