[The Common Lisp Cookbook](index.html) - Hash Tables
====================================================

Contents
--------

-   [Introduction](#intro)
-   [Creating a Hash Table](#create)
-   [Getting a value from a Hash Table](#get)
-   [Adding an Element to a Hash Table](#add)
-   [Testing for the Presence of a Key in a Hash Table](#test)
-   [Deleting from a Hash Table](#del)
-   [Traversing a Hash Table](#traverse)
-   [Counting the Entries in a Hash Table](#count)
-   [Performance Issues: The Size of your Hash Table](#size)

### Introduction

Hash Tables are a powerful data structure, associating keys with values
in a very efficient way. Hash Tables are often preferred over
association lists whenever performance is an issue, but they introduce a
little overhead that makes assoc lists better if there are only a few
key-value pairs to maintain.

### Creating a Hash Table

Hash Tables are created using the function
[`MAKE-HASH-TABLE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_has.htm).
It has no required argument. Its most used optional keyword argument is
`:TEST`, specifying the function used to test the equality of keys.

### Getting a value from a Hash Table

The function
[`GETHASH`](http://www.lispworks.com/documentation/HyperSpec/Body/f_gethas.htm)
takes two required arguments: a key and a hash table. It returns two
values: the value corresponding to the key in the hash table (or NIL if
not found), and a boolean indicating whether the key was found in the
table. That second value is necessary since NIL is a valid value in a
key-value pair, so getting NIL as first value from `GETHASH` does not
necessarily mean that the key was not found in the table.

### Adding an Element to a Hash Table

If you want to add an element to a hash table, you can use `GETHASH`,
the function to retrieve elements from the hash table, in conjunction
with
[`SETF`](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm).

    * (defparameter *my-hash* (make-hash-table))
    *MY-HASH*
    * (setf (gethash 'one-entry *my-hash*) "one")
    "one"
    * (setf (gethash 'another-entry *my-hash*) 2/4)
    1/2
    * (gethash 'one-entry *my-hash*)
    "one"
    T
    * (gethash 'another-entry *my-hash*)
    1/2
    T

### Testing for the Presence of a Key in a Hash Table

The first value returned by GETHASH is the object in the hash table
that's associated with the key you provided as an argument to GETHASH or
NIL if no value exists for this key. This value can act as a
[generalized
boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean)
if you want to test for the presence of keys.

    * (defparameter *my-hash* (make-hash-table))
    *MY-HASH*
    * (setf (gethash 'one-entry *my-hash*) "one")
    "one"
    * (if (gethash 'one-entry *my-hash*)
        "Key exists"
        "Key does not exist")
    "Key exists"
    * (if (gethash 'another-entry *my-hash*)
        "Key exists"
        "Key does not exist")
    "Key does not exist"

But note that this does *not* work if NIL is amongst the values that you
want to store in the hash.

    ;;; continued from above
    * (setf (gethash 'another-entry *my-hash*) nil)
    NIL
    * (if (gethash 'another-entry *my-hash*)
        "Key exists"
        "Key does not exist")
    "Key does not exist"

In this case you'll have to check the *second* return value of GETHASH
which will always return NIL if no value is found and T otherwise.

    ;;; continued from above
    * (if (nth-value 1 (gethash 'another-entry *my-hash*))
        "Key exists"
        "Key does not exist")
    "Key exists"
    * (if (nth-value 1 (gethash 'no-entry *my-hash*))
        "Key exists"
        "Key does not exist")
    "Key does not exist"

### Deleting from a Hash Table

Use
[`REMHASH`](http://www.lispworks.com/documentation/HyperSpec/Body/f_remhas.htm)
to delete a hash entry. Both the key and its associated value will be
removed from the hash table. REMHASH returns T if there was such an
entry, NIL otherwise.

    * (defparameter *my-hash* (make-hash-table))
    *MY-HASH*
    * (setf (gethash 'first-key *my-hash*) 'one)
    ONE
    * (gethash 'first-key *my-hash*)
    ONE
    T
    * (remhash 'first-key *my-hash*)
    T
    * (gethash 'first-key *my-hash*)
    NIL
    NIL
    * (gethash 'no-entry *my-hash*)
    NIL
    NIL
    * (remhash 'no-entry *my-hash*)
    NIL
    * (gethash 'no-entry *my-hash*)
    NIL
    NIL

### Traversing a Hash Table

If you want to perform an action on each entry (i.e., each key-value
pair) in a hash table, you have several options:

You can use
[`MAPHASH`](http://www.lispworks.com/documentation/HyperSpec/Body/f_maphas.htm)
which iterates over all entries in the hash table. Its first argument
must be a function which accepts *two* arguments, the key and the value
of each entry. Note that due to the nature of hash tables you *can't*
control the order in which the entries are provided by MAPHASH (or other
traversing constructs). MAPHASH always returns NIL.

    * (defparameter *my-hash* (make-hash-table))
    *MY-HASH*
    * (setf (gethash 'first-key *my-hash*) 'one)
    ONE
    * (setf (gethash 'second-key *my-hash*) 'two)
    TWO
    * (setf (gethash 'third-key *my-hash*) nil)
    NIL
    * (setf (gethash nil *my-hash*) 'nil-value)
    NIL-VALUE
    * (defun print-hash-entry (key value)
        (format t "The value associated with the key ~S is ~S~%" key value))
    PRINT-HASH-ENTRY
    * (maphash #'print-hash-entry *my-hash*)
    The value associated with the key FIRST-KEY is ONE
    The value associated with the key SECOND-KEY is TWO
    The value associated with the key THIRD-KEY is NIL
    The value associated with the key NIL is NIL-VALUE

You can also use
[`WITH-HASH-TABLE-ITERATOR`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_hash.htm),
a macro which turns (via
[`MACROLET`](http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm))
its first argument into an iterator that on each invocation returns
three values per hash table entry - a generalized boolean that's true if
an entry is returned, the key of the entry, and the value of the entry.
If there are no more entries, only one value is returned - NIL.

    ;;; same hash-table as above
    * (with-hash-table-iterator (my-iterator *my-hash*)
        (loop
          (multiple-value-bind (entry-p key value)
              (my-iterator)
            (if entry-p
                (print-hash-entry key value)
                (return)))))
    The value associated with the key FIRST-KEY is ONE
    The value associated with the key SECOND-KEY is TWO
    The value associated with the key THIRD-KEY is NIL
    The value associated with the key NIL is NIL-VALUE
    NIL

Note the following caveat from the HyperSpec: "It is unspecified what
happens if any of the implicit interior state of an iteration is
returned outside the dynamic extent of the `WITH-HASH-TABLE-ITERATOR`
form such as by returning some closure over the invocation form."

And there's always
[`LOOP`](http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm):

    ;;; same hash-table as above
    * (loop for key being the hash-keys of *my-hash*
            do (print key))
    FIRST-KEY 
    SECOND-KEY 
    THIRD-KEY 
    NIL 
    NIL
    * (loop for key being the hash-keys of *my-hash*
            using (hash-value value)
            do (format t "The value associated with the key ~S is ~S~%" key value))
    The value associated with the key FIRST-KEY is ONE
    The value associated with the key SECOND-KEY is TWO
    The value associated with the key THIRD-KEY is NIL
    The value associated with the key NIL is NIL-VALUE
    NIL
    * (loop for value being the hash-values of *my-hash*
            do (print value))
    ONE 
    TWO 
    NIL 
    NIL-VALUE 
    NIL
    * (loop for value being the hash-values of *my-hash*
            using (hash-key key)
            do (format t "~&~A -> ~A" key value))
    FIRST-KEY -> ONE
    SECOND-KEY -> TWO
    THIRD-KEY -> NIL
    NIL -> NIL-VALUE
    NIL

### Counting the Entries in a Hash Table

No need to use your fingers - Common Lisp has a built-in function to do
it for you:
[`HASH-TABLE-COUNT`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_1.htm).

    * (defparameter *my-hash* (make-hash-table))
    *MY-HASH*
    * (hash-table-count *my-hash*)
    0
    * (setf (gethash 'first *my-hash*) 1)
    1
    * (setf (gethash 'second *my-hash*) 2)
    2
    * (setf (gethash 'third *my-hash*) 3)
    3
    * (hash-table-count *my-hash*)
    3
    * (setf (gethash 'second *my-hash*) 'two)
    TWO
    * (hash-table-count *my-hash*)
    3
    * (clrhash *my-hash*)
    #
    * (hash-table-count *my-hash*)
    0

### Performance Issues: The Size of your Hash Table

The `MAKE-HASH-TABLE` function has a couple of optional parameters which
control the initial size of your hash table and how it'll grow if it
needs to grow. This can be an important performance issue if you're
working with large hash tables. Here's an (admittedly not very
scientific) example with [CMUCL](http://www.cons.org/cmucl) pre-18d on
Linux:

    * (defparameter *my-hash* (make-hash-table))
    *MY-HASH*
    * (hash-table-size *my-hash*)
    65
    * (hash-table-rehash-size *my-hash*)
    1.5
    * (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
    Compiling LAMBDA NIL: 
    Compiling Top-Level Form: 

    Evaluation took:
      0.27 seconds of real time
      0.25 seconds of user run time
      0.02 seconds of system run time
      0 page faults and
      8754768 bytes consed.
    NIL
    * (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
    Compiling LAMBDA NIL: 
    Compiling Top-Level Form: 

    Evaluation took:
      0.05 seconds of real time
      0.05 seconds of user run time
      0.0 seconds of system run time
      0 page faults and
      0 bytes consed.
    NIL

The values for
[`HASH-TABLE-SIZE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_4.htm)
and
[`HASH-TABLE-REHASH-SIZE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_2.htm)
are implementation-dependent. In our case, CMUCL chooses and initial
size of 65, and it will increase the size of the hash by 50 percent
whenever it needs to grow. Let's see how often we have to re-size the
hash until we reach the final size...

    * (log (/ 100000 65) 1.5)
    18.099062
    * (let ((size 65)) (dotimes (n 20) (print (list n size)) (setq size (* 1.5 size))))
    (0 65) 
    (1 97.5) 
    (2 146.25) 
    (3 219.375) 
    (4 329.0625) 
    (5 493.59375) 
    (6 740.3906) 
    (7 1110.5859) 
    (8 1665.8789) 
    (9 2498.8184) 
    (10 3748.2275) 
    (11 5622.3413) 
    (12 8433.512) 
    (13 12650.268) 
    (14 18975.402) 
    (15 28463.104) 
    (16 42694.656) 
    (17 64041.984) 
    (18 96062.98) 
    (19 144094.47) 
    NIL

The hash has to be re-sized 19 times until it's big enough to hold
100,000 entries. That explains why we saw a lot of consing and why it
took rather long to fill the hash table. It also explains why the second
run was much faster - the hash table already had the correct size.
Here's a faster way to do it: If we know in advance how big our hash
will be, we can start with the right size:

    * (defparameter *my-hash* (make-hash-table :size 100000))
    *MY-HASH*
    * (hash-table-size *my-hash*)
    100000
    * (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
    Compiling LAMBDA NIL: 
    Compiling Top-Level Form: 

    Evaluation took:
      0.04 seconds of real time
      0.04 seconds of user run time
      0.0 seconds of system run time
      0 page faults and
      0 bytes consed.
    NIL

That's obviously much faster. And there was no consing involved because
we didn't have to re-size at all. If we don't know the final size in
advance but can guess the growth behaviour of our hash table we can also
provide this value to `MAKE-HASH-TABLE`. We can provide an integer to
specify absolute growth or a float to specify relative growth.

    * (defparameter *my-hash* (make-hash-table :rehash-size 100000))
    *MY-HASH*
    * (hash-table-size *my-hash*)
    65
    * (hash-table-rehash-size *my-hash*)
    100000
    * (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
    Compiling LAMBDA NIL: 
    Compiling Top-Level Form: 

    Evaluation took:
      0.07 seconds of real time
      0.05 seconds of user run time
      0.01 seconds of system run time
      0 page faults and
      2001360 bytes consed.
    NIL

Also rather fast (we only needed one re-size) but much more consing
because almost the whole hash table (minus 65 initial elements) had to
be built during the loop.

Note that you can also specify the `REHASH-THRESHOLD` while creating a
new hash table. One final remark: Your implementation is allowed to
*completely ignore* the values provided for `REHASH-SIZE` and
`REHASH-THRESHOLD`...

* * * * *

[Copyright](license.html) © 2002-2007 The Common Lisp Cookbook Project

http://cl-cookbook.sourceforge.net/

\
\$Header: /cvsroot/cl-cookbook/cl-cookbook/hashes.html,v 1.9 2007/01/16
20:58:32 ozten Exp \$

