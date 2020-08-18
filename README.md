# qmake-mode.el

This is an Emacs major mode for editing QMake project files.
It provides syntax highlighting and indentation rules.

To install, place this file somewhere in your load path, and put the
following into your `.emacs` file:

~~~
(require 'qmake-mode)
~~~

`qmake-mode` will be automatically activated for `.pro`, `.pri`,
`.prl` and `.prf` files.

# Extension for find-file-at-point

This file provides an extension for `find-file-at-point` to enable the
user to jump to files in QMake code like this:

~~~
SOURCES += $$PWD/foo.cpp
~~~

To enable the feature, put this into your init file:

~~~
(add-to-list 'ffap-alist '(qmake-mode . qmake-ffap))
~~~

# Acknowledgements

The nucleus for this mode is `qmake.el` from
[code.google.com/p/qmake-mode](https://code.google.com/p/qmake-mode/).
However, by now there's not much left of it.
