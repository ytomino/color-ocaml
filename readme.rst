very very simple color library for Objective-Caml
=================================================

What's this?
------------

This library has simple purpose for converting between RGB/sRGB/HSV/HSL.

Prerequisites
-------------

OCaml >= 4.08
 https://ocaml.org/

How to make
-----------

Install
+++++++

::

 make install PREFIX=/usr/local

Specify your preferred directory to ``PREFIX``.
The libraries would be installed into ``$PREFIX/lib/ocaml`` (default is
``ocamlc -where``).

Uninstall
+++++++++

::

 make uninstall PREFIX=/usr/local

Build examples
++++++++++++++

::

 make -C examples

Note
----

RGB/HSV/HSL are linear.
sRGB is gamma corrected for display.
Some calculations should be performed on linear types.
