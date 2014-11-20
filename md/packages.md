[The Common Lisp Cookbook](index.html) - Packages
=================================================

Contents
--------

-   ["The Complete Idiot's Guide to Common Lisp
    Packages"](http://www.flownet.com/gat/packages.pdf)\
     (by Erann Gat - external link to PDF file)
-   [List all Symbols in a Package](#list)

[Tim Bradshaw](mailto:tfb@tfeb.org) is currently working on a whole
chapter about this topic. If you want to contribute something you should
contact him.

### List all Symbols in a Package

Common Lisp provides some macros to iterate through the symbols of a
package. The two most interesting are: [`DO-SYMBOLS` and
`DO-EXTERNAL-SYMBOLS`](http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm).
`DO-SYMBOLS` iterates over the symbols accessible in the package and
`DO-EXTERNAL-SYMBOLS` only iterates over the external symbols (you can
see them as the real package API).

To print all exported symbols of a package named "PACKAGE", you can
write:

    (do-external-symbols (s (find-package "PACKAGE"))
      (print s))

You can also collect all these symbols in a list by writing:

    (let (symbols)
      (do-external-symbols (s (find-package "PACKAGE"))
        (push s symbols))
      symbols)

Or you can do it with
[`LOOP`](http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm).

    (loop for s being the external-symbols of (find-package "PACKAGE")
          collect s)

* * * * *

[Copyright](license.html) © 2002-2007 The Common Lisp Cookbook Project

http://cl-cookbook.sourceforge.net/

\
\$Header: /cvsroot/cl-cookbook/cl-cookbook/packages.html,v 1.9
2007/01/16 20:58:32 ozten Exp \$

