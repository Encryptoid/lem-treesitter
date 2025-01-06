# lem-treesitter
NOTES: Very rough, just getting it working personally.


The goal is to not just create a walker for the syntax tree, but to parse each visited buffer to a TS Tree, and then expose querying to other packages. One of the packages will be syntax highlighting.

# Use Cases


## Syntax Highlighting


## Extented Syntax Indications

This would include things like indicating in the line numbers bar if the line has a function/error, etc.


## Refactoring tools


## Text Objects

The main use case for this I know of would be in vi-mode. TS could be used to operate on text objects, such as `va"`. Extended functionality can then be provided based on the parser, eg. `daf` for delete-around-function.

Other use cases would be other text manipulation, such as swapping the position of arguments in a function call.


## Lem Calls
Lem can use tree sitter for many of it's syntax based calls such as `lisp-compile-defun` or `lisp-test-runner-run-current`.
These currently have bug where they seem to search for the last zero indented line to use:
```common-lisp
(defun adder ()
(+ 1 1)) ;; With cursor here, will not select defun

(defun adder ()
  (+ 1 1)) ;; Requires indentation
```

# Implementation

When a buffer is loaded, if it has a grammar, parse it's TS Tree and store.

Hook onto `(variable-value 'lem/buffer/internal:after-change-functions :buffer)` to track edits to the buffer.

We can either update immeditely or store the changes and send them to TS with `lem/common/timer:make-idle-timer`.


# Differences from lem-treesitter-mode
This is much more of a specific mode for syntax highlighting. Each syntax scan it will parse the buffer. My package wil instead parse the buffer once on load, and send changes to TS. We can choose to update the syntax here.
Good to still do the whole buffer, but it will only be on edit rather than a timer. The other option is to use a tight idle time, if we need to handle larger scale edits to a file.


# High Level Highlight Example

1. When buffer is loaded, try add ts-tree variable
2. In the same way a current `syntax-tree` is created, send an intial syntax scan of the tree with a cursor.
3.
