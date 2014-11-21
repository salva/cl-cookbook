# Packages

## List all Symbols in a Package

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
