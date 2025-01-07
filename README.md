# lem-treesitter
NOTES: Very rough, just getting it working personally. The notes are not organised either.


The goal is to not just create a walker for the syntax tree, but to parse each visited buffer to a TS Tree, and then expose querying to other packages. One of the packages will be syntax highlighting.

The existing modes(python-mode, etc.) should still be activate to use their other functionality. `treesitter-mode` will try map the major mode to one of it's parsers.
If the mapping succeed, it will call the init for `treesitter/langs/python`. It is in here that if we have syntax highlighting for a mode, we will turn off the existing and switch to treesitters highlighting extension.

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
https://github.com/somniamble/lem-treesitter-mode
This is much more of a specific mode for syntax highlighting. It hooks into the existing syntax system so each syntax scan it will parse the buffer. My package will instead parse the buffer once on load, and send changes to TS. We can choose to update the syntax here.
It was great to see their work though, so thank you to them.

# High Level Highlight Example

1. When buffer is loaded, try add ts-tree variable
2. In the same way a current `syntax-tree` is created, send an intial syntax scan of the tree with a cursor.
3.
