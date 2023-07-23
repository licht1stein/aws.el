;;; xht.el --- The extensive hash table library  -*- lexical-binding: t -*-

;; SPDX-FileCopyrightText:  © flandrew <https://keyoxide.org/191F5B3E212EF1E515C19918AF32B9A5C1CCCB2D>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;---------------------------------------------------------------------------
;; Author:            flandrew
;; Created:           2022-04-10
;; Version:           2.0.0-git2023.07.01
;; Homepage:          <https://flandrew.srht.site/listful/software.html>
;; Keywords:          extensions, lisp
;; Package-Requires:  ((emacs "25.1") (dash "2.14") (s "1.12") (ht "2.3"))
;;---------------------------------------------------------------------------

;; This file is part of XHT, which is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. For more details, see the full license at
;; either LICENSES/GPL-3.0-or-later.txt or <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; XHT is a hash table library. It's extensive and systematically-structured.
;;
;; My intention with it is that:
;;
;;   1. dealing with hash tables in Emacs Lisp be pleasant,
;;
;;   2. hash tables become your go-to choice for most key–value operations in
;;      the language, and
;;
;;   3. for almost everything hash-table–related that you might want to do in
;;      Emacs Lisp, this library will have functions for it — or at least
;;      close enough that you can do it by composing a few of them.
;;
;; Let me know if I succeeded.
;;
;;;; For all the details, please do see the README
;;
;; Open it easily with any of the below:
;;   (find-file-read-only              "README.org")   <--- C-x C-e here¹, or
;;   (find-file-read-only-other-frame  "README.org")   <--- C-x C-e here¹, or
;;   (find-file-read-only-other-window "README.org")   <--- C-x C-e here¹
;;
;; or from any buffer:
;;   M-x xht-see-readme
;;
;; or read it online:
;;   <https://flandrew.srht.site/listful/sw-emacs-xht.html>
;;
;; ¹ or the key that ‘eval-last-sexp’ is bound to, if not C-x C-e.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Acknowledgments:
;;
;; A few of this library's functions adapted code from ht.el.
;; This is usually indicated in the functions themselves with a link to the
;; corresponding ht function. (See, e.g., ‘h-put!’ and ‘h-rem!’.)
;;
;; Thank you, Wilfred Hughes (and other ht contributors)!
;;
;; About ht:
;;   SPDX-FileCopyrightText:  © 2013 Wilfred Hughes
;;   SPDX-License-Identifier: GPL-3.0-or-later
;;   Author:                  Wilfred Hughes
;;   Homepage:                <https://github.com/Wilfred/ht.el>
;;
;; ------------------------------------------------------------------------
;;
;; A few parts of this library were inspired by dash.el.
;; The following functions borrowed and adapted code from dash:
;; - much of xht's fontlock/fontification (from dash's fontification)
;; - part of the h-zip and h-unzip family of functions
;;
;; My thanks to Magnar Sveen and the other dash contributors.
;;
;; About dash:
;;   SPDX-FileCopyrightText:  © 2012 Free Software Foundation, Inc.
;;   SPDX-License-Identifier: GPL-3.0-or-later
;;   Author:                  Magnar Sveen
;;   Homepage:                <https://github.com/magnars/dash.el>
;;
;; ------------------------------------------------------------------------
;;
;; The main and the helper functions of ‘h-let’ were inspired by, and
;; adapted from, ‘let-alist’.
;;
;; I thank Artur Malabarba for this handy macro.
;;
;; About let-alist:
;;   SPDX-FileCopyrightText:  © 2014 Free Software Foundation, Inc.
;;   SPDX-License-Identifier: GPL-3.0-or-later
;;   Author:                  Artur Malabarba
;;
;; ------------------------------------------------------------------------
;;
;; The functions ‘xht--do-string-cm-ify’, ‘xht--do-string-escape’, and
;; ‘xht--do-string-escape-ws’ were adapted from string-edit.el.
;;
;; About string-edit:
;;   SPDX-FileCopyrightText:  © 2013 Magnar Sveen
;;   SPDX-License-Identifier: GPL-3.0-or-later
;;   Author:                  Magnar Sveen
;;   Homepage:                <https://github.com/magnars/string-edit.el>
;;
;; ------------------------------------------------------------------------
;;
;; The function ‘h-alist?’ was adapted from json.el's ‘json-alist-p’.
;;
;; About json:
;;   SPDX-FileCopyrightText:  © 2006 Free Software Foundation, Inc.
;;   SPDX-License-Identifier: GPL-3.0-or-later
;;   Author:                  Theresa O'Connor
;;
;; ------------------------------------------------------------------------
;;
;; Helper ‘xht--str-nw?’ was adapted from org-macs.el's ‘org-string-nw-p’.
;;
;; About org:
;;   SPDX-FileCopyrightText:  © 2004 Free Software Foundation, Inc.
;;   SPDX-License-Identifier: GPL-3.0-or-later
;;   Author:                  Carsten Dominik
;;   Homepage:                <https://orgmode.org>
;;
;; ------------------------------------------------------------------------
;;
;; Macro ‘xht--describe’ was borrowed from OrgReadme-fy.
;;
;; Functions ‘xht-see-readme’, ‘xht--see-library-readme’, and
;; ‘xht--libname->libfile’ were adapted from ‘orgreadme-fy-see-readme’,
;; ‘orgreadme-fy-see-library-readme’, and ‘orgreadme-fy-libname->libfile’,
;; respectively.
;;
;; About orgreadme-fy:
;;   SPDX-FileCopyrightText:  © flandrew
;;   SPDX-License-Identifier: GPL-3.0-or-later
;;   Author:                  flandrew
;;   Homepage:                <https://sr.ht/~flandrew/orgreadme-fy>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Code:
;;;; Libraries

(require 's)
(require 'ht)
(require 'pp)
(require 'rx)
(require 'dash)
(require 'json)
(require 'pcase)
(require 'subr-x)    ; ‘if-let’, ‘when-let’
(require 'lisp-mnt)  ; ‘lm-summary’, ‘lm-version’, ‘lm-homepage’
(eval-when-compile
  (require 'inline))


;;;; Symbols from other packages

;; Silence "not known to be defined" compiler warnings
(declare-function org-mode          "ext:org"       ())
(declare-function org-at-table-p    "ext:org"       (&optional table-type))
(declare-function org-ctrl-c-ctrl-c "ext:org"       (&optional arg))
(declare-function orgtbl-mode       "ext:org-table" (&optional arg))
(declare-function org-table-begin   "ext:org-table" (&optional table-type))
(declare-function org-table-end     "ext:org-table" (&optional table-type))
(declare-function org-table-to-lisp "ext:org-table" (&optional txt))
(declare-function org-table-convert-region "ext:org-table" (beg0 end0 &optional
                                                                 separator))
(declare-function aggressive-indent-mode   "ext:aggressive-indent" (&optional
                                                                    arg))


;;;; Package metadata

(defconst xht--name "XHT")

(defconst xht--dot-el
  (format "%s.el" (file-name-sans-extension (eval-and-compile
                                              (or load-file-name
                                                  buffer-file-name)))))
(defconst xht--readme-org
  (expand-file-name "README.org" (file-name-directory xht--dot-el)))

(defconst xht--summary  (lm-summary  xht--dot-el))
(defconst xht--version  (lm-version  xht--dot-el))
(defconst xht--homepage (lm-homepage xht--dot-el))


;;;; Customizable variables

(defgroup xht nil
  (format "%s." xht--summary)
  :group 'extensions
  :group 'lisp
  :link  '(emacs-library-link :tag "Lisp file" "xht.el")
  :link  `(file-link :tag "README.org" ,xht--readme-org)
  :link  `(url-link  :tag "Homepage"   ,xht--homepage))

;; See also ‘xht-do-mode-lighter’ under xht-do minor mode.
;; See also ‘xht-fontlock-add-anaphoric-variables’ and other defcustom options
;; under Font lock minor mode.


;;;; Other variables

(defvar h-kvl-sep-re  " *= *"
  "Regular expression to match KVL separator (field delimiter).

So if a candidate KVL string has in every non-blank, non-commented
line a single equal sign surrounded or not by spaces, this will be
taken as a valid KVL, and the regex will be taken as the field
delimiter to parse the key–value lines.

This variable should not be directly changed. Rather, it should be
let-bound around KVL-related expressions when the default delimiter
is not applicable. Here are two examples for when colon is used as
delimiter (the first strictly with no spaces, the second accepting
it whether surrounded by spaces or not):

  (let ((h-kvl-sep-re \":\"))
    (h-kvl? \"Alice:42\\nBob:30\"))

  (let ((h-kvl-sep-re \" *: *\"))
    (h<-kvl \"\
Alice : 42
Bob   : 30\"))")

(defvar h-kvl-section-re
  "^[ \t]*\\[.*\\][ \t]*$"
  "Regular expression to match sections in KVLs.

This is used to non-destructively strip sections in key–value lines
in KVL–related functions performing conversion and type-testing.

It by default matches lines with [some text] surrounded or not by
spaces or TABs. This is useful to deal with config files that have
sections only for information purposes but which don't otherwise
modify or restrict the keys under it.

This variable should not be directly changed. Rather, it should be
let-bound around KVL-related expressions when the default section
regex is not applicable. Here is an example for a hypothetical KVL
that uses braces to mark sections:

  (let ((h-kvl-section-re \"^[ \\t]*{.*}[ \\t]*$\"))
    (h-kvl? \"\\
{People}
Alice = 42
Bob   = 30
Emily = 21\"))")

(defvar h-kvl-comment-re
  "^#.*\\|[ \t]#.*"
  "Regular expression to match comments in KVLs.

This is used to non-destructively strip comments in key–value lines
in KVL–related functions performing conversion and type-testing.

It by default matches:
- BOL followed by # followed by anything until EOL
- One space or TAB followed by # followed by anything until EOL

where BOL and EOL mean beginning and end of line, respectively.

This variable should not be directly changed. Rather, it should be
let-bound around KVL-related expressions when the default comment
regex is not applicable. Here is an example for a hypothetical KVL
that uses semicolons for comments:

  (let ((h-kvl-comment-re \"^;.*\\\\| +;.*\"))
    (h-kvl? \"\\
;; These are some people
Alice=42  ; Alice's real age!
Bob=30
Emily=21\"))")

;; See also ‘xht-do-mode-map’ under xht-do minor mode.


;;;; Description macro

(defmacro xht--describe (str)
  "Describe with string STR the contents under this heading.
Think of it as a docstring for the headings of your elisp files.

For the full docstring, look for ‘orgreadme-fy-describe’ in the
package ‘orgreadme-fy’."
  (declare (indent 0))
  (unless (stringp str)
    (user-error "‘xht--describe’ must receive a string")))


;;;; Naming conventions

(xht--describe
  "This library adopts the following naming conventions:

| These functions | look like |
|-----------------+-----------|
| Regular         | h-foo     |
| Anaphoric       | h--foo    |
| Interactive     | xht-foo   |
| Private         | xht--foo  |

and

| These variables | look like |
|-----------------+-----------|
| Regular         | h-foo     |
| Customizable    | xht-foo   |
| Private         | xht--foo  |

The two tables above represent almost all of the more than 500
symbols defined by ‘xht’. These also happen to be the same familiar
conventions that have long been adopted by ‘dash’.

There are some *functions that convert from and to hash tables*:

| These functions      | look like            |
|----------------------+----------------------|
| Regular (conversion) | h->foo, h<-foo       |
| Private (conversion) | xht-->foo, xht<--foo |

This intuitive use of '->' and '<-' to represent conversion will
certainly look familiar to those who have used ‘ht’.

That's pretty much it.

The only symbols that don't perfectly match the tables above are:

- The commands ‘global-xht-do-mode’ and ‘global-xht-fontify-mode’.

- Half a dozen regular functions:
  - The type-checking predicate ‘h?’.
  - The ubiquitous hash-table creator ‘h*’.
  - The hash-table equality predicates ‘h=’, ‘h==’, ‘h_=’, ‘h~=’.

Now, complementing the previous conventions, you'll notice that
some suffixes are consistently employed:

- ? :: Functions that have an ? as suffix are *predicates*
       (they \"ask\" a yes-or-no question and return t or nil).
       They have a corresponding -p alias.

- = :: Functions that have an = as suffix are *equality predicates*
       and therefore check whether two or more things can be
       considered equivalent according to some standard.

- * :: Functions that act on *nested hash tables* have an * as
       suffix, and vice-versa. (With the notable exception of ‘h*’,
       whose * has a slightly different, but hopefully obvious
       enough, connotation.)

- ! :: Functions that *destructively modify* the table that is
       passed as argument have an ! as suffix, and vice-versa.

       *There are no !-less aliases for destructive functions*:
       wherever the ! is dropped, it means it's side-effect free:
       the value of the result of the operations is returned, and
       TABLE remains unmodified. If you find it happening
       otherwise, it's most likely unintentional and a bug — please
       report.

       Note that this differs from ht's naming convention, in which
       ‘ht-clear’, ‘ht-remove’, ‘ht-set’, and ‘ht-update’ remain as
       aliases to, respectively, ‘ht-clear!’, ‘ht-remove!’,
       ‘ht-set!’, and ‘ht-update!’ — all of which destructive
       functions. (But ‘ht-reject’ is not an alias to ‘ht-reject!’:
       the former is non-destructive, the latter is destructive.)

       On the other hand, xht follows this convention consistently.
       For example:

       | Non-destructive | Destructive |
       |-----------------+-------------|
       | ‘h-rej’         | ‘h-rej!’    |
       | ‘h-sel’         | ‘h-sel!’    |
       | ‘h-put’         | ‘h-put!’    |
       | ‘h-mix’         | ‘h-mix!’    |
       | ‘h-mix*’        | ‘h-mix*!’   |

       etc.

The library itself may be called either XHT or xht. Both are fine.

Mixed casing is better avoided. Yes, you'll see it titleized in the
docstrings of ‘global-xht-do-mode’ and ‘global-xht-fontify-mode’ —
but only because these docstrings happen to be automatically
generated by ‘define-globalized-minor-mode’, which calls
‘easy-mmode-pretty-mode-name’, which titleizes names.")


;;;; Functions
;;;;; General hash table operations
;;;;;; Creation

(xht--describe
  "Functions that create a hash table.")

;;;;;;; Empty one

(xht--describe
  "Functions that create an empty hash table.")

(define-inline h-new (&optional sz ts we rs rt)
  "Create empty hash table.

Arguments passed correspond to, respectively, size, test, weakness,
rehash-size, rehash-threshold. When nil, they match that of TABLE.

SZ is the initial assigned size. When nil, default to 65.

TS is the function used to compare the hash keys.
It can be ‘eq’, ‘eql’, ‘equal’ or a user-supplied test created
via ‘define-hash-table-test’. When nil, default to ‘equal’.

The other three default to Emacs' defaults:
- WE defaults to nil.
- RS defaults to 1.5.
- RT defaults to 0.8.

This is similar to ‘ht-create’. The differences are:

- size as a first optional argument, test as second;

- possibility of passing other parameters accepted
  by ‘make-hash-table’, which see.

See also: ‘h-clr’ and ‘h-clr!’."
  (inline-quote
   (make-hash-table
    :size             (or ,sz 65)
    :test             (or ,ts 'equal)
    :weakness         ,we
    :rehash-size      (or ,rs 1.5)
    :rehash-threshold (or ,rt 0.8))))

(define-inline h-empty-clone (table &optional sz ts we rs rt)
  "Create empty hash table with some or all properties from TABLE.
Arguments passed correspond to, respectively, size, test, weakness,
rehash-size, rehash-threshold. When nil, they match that of TABLE.

This is a non-destructive function.
See also: ‘h-clr’ and ‘h-clr!’."
  (inline-quote
   (make-hash-table
    :size             (or ,sz (hash-table-size             ,table))
    :test             (or ,ts (hash-table-test             ,table))
    :weakness         (or ,we (hash-table-weakness         ,table))
    :rehash-size      (or ,rs (hash-table-rehash-size      ,table))
    :rehash-threshold (or ,rt (hash-table-rehash-threshold ,table)))))

(define-inline h-clr (table)
  "Create empty hash table whose properties match those of TABLE.

It preserves all of the properties of the original table — except
data, which is empty. It's therefore particularly useful as a basis
for other hash table functions (such as ‘h-hmap*’) that perform
non-destructive operations — for which a fresh empty table must be
created, and for which the use of ‘h-new’ or ‘ht-create’ would
result in a loss of these properties.

This is a non-destructive function.
Its destructive counterpart is ‘h-clr!’."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (make-hash-table
    :size             (hash-table-size             ,table)
    :test             (hash-table-test             ,table)
    :weakness         (hash-table-weakness         ,table)
    :rehash-size      (hash-table-rehash-size      ,table)
    :rehash-threshold (hash-table-rehash-threshold ,table))))
;; ^ We could just (defsubst h-clr (table) (h-empty-clone table))
;;   but this function is a bit too fundamental, so I'm optimizing it.

(define-inline h-clr! (table)
  "Remove all keys from TABLE.
This is equivalent to current ‘ht-clear!’.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-clr’."
  (inline-quote
   (ignore
    (clrhash ,table))))

;;;;;;; Cloning and copying

(xht--describe
  "See ‘h-clone*’ and ‘h<-ht’.")

;;;;;;; From keys and values

(xht--describe
  "Functions to create a hash table from keys and values.")

;;;;;;;; passed directly

(defun h* (&rest pairs)
  "Create a hash table with the key–value PAIRS.
Keys are compared with ‘equal’.

This function is similar to ‘ht’, with the difference that no
parentheses are used around each key–value pair: so its syntax is
identical to that used to create a plist, but replacing ‘list’ with
‘h*’.

Also, caution: with plists and alists, repeated items that come
first have priority, whereas when passing pairs to ‘ht’ or ‘h*’.
it's the opposite — the repeated item's last value wins.

Compare:
#+begin_src emacs-lisp
  (plist-get '(:a 1 :b 2 :a 5) :a)       => 1
  (ht-get (h*  :a 1 :b 2 :a 5) :a)       => 5
  (ht-get (ht (:a 1) (:b 2) (:a 5)) :a)  => 5
#+end_src

Another difference is that, instead of 65, its initial nominal size
defaults to the result of ‘xht--init-size’, which determines
initial nominal size as a function of the number of pairs.
Currently, ‘xht--init-size’ uses ‘xht--init-size-exact’, which
returns the exact number of pairs.

\(fn KEY-1 VALUE-1 KEY-2 VALUE-2 ...)"
  (declare (pure t) (side-effect-free t))
  (let ((len (length pairs)))
    (if (= 0 (% len 2))
        (let ((tbl (h-new (xht--init-size (/ len 2)))))
          (while pairs
            (h-put! tbl (pop pairs) (pop pairs)))
          tbl)
      (signal 'wrong-number-of-arguments '((3 . 3) 2)))))

(defun h-s* (size &rest pairs)
  "Create a hash table of size SIZE with the key–value PAIRS.
This is exactly like ‘h*’ (which see), with the difference that its
nominal size is explicitly passed as first argument instead of being
inferred.

When SIZE is nil, it defaults to h*'s default.

\(fn SIZE KEY-1 VALUE-1 KEY-2 VALUE-2 ...)"
  (declare (pure t) (side-effect-free t))
  (let ((len (length pairs)))
    (if (= 0 (% len 2))
        (let ((tbl (h-new (or size (xht--init-size (/ len 2)))
                          nil)))
          (while pairs
            (h-put! tbl (pop pairs) (pop pairs)))
          tbl)
      (signal 'wrong-number-of-arguments '((3 . 3) 2)))))

(defun h-t* (test &rest pairs)
  "Create a hash table of function TEST with the key–value PAIRS.
This is exactly like ‘h*’ (which see), with the difference that its
test function is explicitly passed as first argument instead of
being inferred.

When TEST is nil, it defaults to h*'s default, which is h-new's
default: 'equal.

\(fn TEST KEY-1 VALUE-1 KEY-2 VALUE-2 ...)"
  (declare (pure t) (side-effect-free t))
  (let ((len (length pairs)))
    (if (= 0 (% len 2))
        (let ((tbl (h-new (xht--init-size (/ len 2))
                          test)))
          (while pairs
            (h-put! tbl (pop pairs) (pop pairs)))
          tbl)
      (signal 'wrong-number-of-arguments '((3 . 3) 2)))))

(defun h-st* (size test &rest pairs)
  "Create a hash table of SIZE and TEST with the key–value PAIRS.
This is exactly like ‘h*’ (which see), with the difference that its
nominal size and its test function are explicitly passed as
first arguments instead of being inferred.

When SIZE is nil, it defaults to h*'s default.

When TEST is nil, it defaults to h*'s default, which is h-new's
default: 'equal.

\(fn SIZE TEST KEY-1 VALUE-1 KEY-2 VALUE-2 ...)"
  (declare (pure t) (side-effect-free t))
  (let ((len (length pairs)))
    (if (= 0 (% len 2))
        (let ((tbl (h-new (or size (xht--init-size (/ len 2)))
                          test)))
          (while pairs
            (h-put! tbl (pop pairs) (pop pairs)))
          tbl)
      (signal 'wrong-number-of-arguments '((3 . 3) 2)))))

(defun h-reduce-r (&rest keys-value)
  "Return a one-key nested table with the KEYS-VALUE sequence.
So, for example, (h-reduce-r :a :b :c 2) is equivalent
to (h* :a (h* :b (h* :c 2))), which is the same
as (ht (:a (ht (:b (ht (:c 2))))))."
  (declare (pure t) (side-effect-free t))
  (pcase (length keys-value)
    ((pred (<= 2)) (--reduce-r (h* it acc) keys-value))
    (1             (h* keys-value)) ;; wrong-number-of-arguments error
    (0             (h-new 1))))

;;;;;;;; passed as vectors

(defmacro h--zip-vectors-with (key-form val-form keys values
                                        &optional size test)
  "Anaphoric version of ‘h-zip-vectors-with’.
The form KEY-FORM modifies each key.
The form VAL-FORM modifies each value.

Each element of KEYS   is bound to the symbol key.
Each element of VALUES is bound to the symbol value.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

The (roughly) inverse of this function is ‘h--vunzip-with’."
  (let ((lk (make-symbol "length-of-keys"))
        (lv (make-symbol "length-of-values"))
        (i  (make-symbol "i"))
        (r  (make-symbol "results")))
    `(if ,keys
         (and (vectorp ,keys)
              (vectorp ,values)
              (let* ((,lk (length ,keys))
                     (,lv (length ,values))
                     (,i  0)
                     (,r  (h-new (or ,size
                                     (xht--init-size ,lk))
                                 ,test))
                     key value)
                (ignore key value)
                (while (< ,i ,lk)
                  (setq key   (aref ,keys ,i)
                        value (when (< ,i ,lv)
                                (aref ,values ,i))
                        ,i    (1+ ,i))
                  (h-put! ,r ,key-form ,val-form))
                ,r))
       (h-new 1))))

(defun h-zip-vectors-with (key-fun val-fun keys values &optional size test)
  "Create a hash table with KEYS and VALUES modified by functions.
The function KEY-FUN modifies each key.
The function VAL-FUN modifies each value.

Both functions take two variables: key and value, respectively.

Each KEY and VALUE is taken one by one from their corresponding
positions at the vectors KEYS and VALUES.

If KEYS is nil, return empty hash table.
If KEYS is shorter than VALUES, ignore additional elements of VALUES.
If VALUES is shorter than KEYS, complete VALUES with nils.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

The anaphoric version of this function is ‘h--zip-vectors-with’.

The (roughly) inverse of this function is ‘h-vunzip-with’."
  (declare (pure t) (side-effect-free t))
  (h--zip-vectors-with (funcall key-fun key value)
                       (funcall val-fun key value)
                       keys values size test))

(defun h-zip-vectors (keys values &optional size test)
  "Create a hash table with KEYS and VALUES. Both must be vectors.
This is just like ‘h-zip-vectors-with’, but without modifying any of the keys
and values before putting them on the table.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

The (roughly) inverse of this function is ‘h-vunzip’."
  (declare (pure t) (side-effect-free t))
  (h--zip-vectors-with key value keys values size test))

;;;;;;;; passed as lists

(defmacro h--zip-lists-with (key-form val-form keys values
                                      &optional size test)
  "Anaphoric version of ‘h-zip-lists-with’.
The form KEY-FORM modifies each key.
The form VAL-FORM modifies each value.

Each element of KEYS   is bound to the symbol key.
Each element of VALUES is bound to the symbol value.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

The (roughly) inverse of this function is ‘h--lunzip-with’."
  (let ((ks (make-symbol "keys"))
        (vs (make-symbol "values"))
        (lk (make-symbol "length-of-keys"))
        (r  (make-symbol "results")))
    `(if ,keys
         (and (listp ,keys)
              (listp ,values)
              (let* ((,ks (-copy ,keys))
                     (,vs (-copy ,values))
                     (,lk (length ,ks))
                     (,r  (h-new (or ,size
                                     (xht--init-size ,lk))
                                 ,test))
                     key value)
                (ignore key value)
                (while ,ks
                  (setq key   (car ,ks)
                        value (car ,vs))
                  (h-put! ,r ,key-form ,val-form)
                  (!cdr ,ks)
                  (!cdr ,vs))
                ,r))
       (h-new 1))))

(defun h-zip-lists-with (key-fun val-fun keys values &optional size test)
  "Create a hash table with KEYS and VALUES modified by functions.
The function KEY-FUN modifies each key.
The function VAL-FUN modifies each value.

Both functions take two variables: key and value, respectively.

Each KEY and VALUE is taken one by one from their corresponding
positions at the lists KEYS and VALUES.

If KEYS is nil, return empty hash table.
If KEYS is shorter than VALUES, ignore additional elements of VALUES.
If VALUES is shorter than KEYS, complete VALUES with nils.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

The anaphoric version of this function is ‘h--zip-lists-with’.

The (roughly) inverse of this function is ‘h-lunzip-with’."
  (declare (pure t) (side-effect-free t))
  (h--zip-lists-with (funcall key-fun key value)
                     (funcall val-fun key value)
                     keys values size test))

(defun h-zip-lists (keys values &optional size test)
  "Create a hash table with KEYS and VALUES. Both must be lists.
This is just like ‘h-zip-lists-with’, but without modifying any of the keys
and values before putting them on the table.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

The (roughly) inverse of this function is ‘h-lunzip’."
  (declare (pure t) (side-effect-free t))
  (h--zip-lists-with key value keys values size test))



;;;;;;;; helper: determine initial size

(defalias 'xht--init-size 'xht--init-size-exact
  "Function used to determine the initial size of a hash table.
If at some point we decide that a function exists which is more
appropriate for the initial nominal size of a hash table to be
created from a given number of pairs, refactoring is made easier by
simply changing this alias to point to that new, supposedly better,
xht--init-size-<something> function.")

(defsubst xht--init-size-exact (n-pairs)
  "Return N-PAIRS, the exact number of pairs expected.
The rationale is that the initial number of pairs will be, on
average, a much better estimate of how much space the table needs.
For tables (sometimes much) smaller than 65, this leads to better
performance, especially when the table won't be updated with more
items (or when only a few)."
  n-pairs)

(defsubst xht--init-size-fixed (&optional n-pairs)
  "Return 65, Emacs' default nominal creation size for hash tables.
Ignore N-PAIRS."
  (ignore n-pairs)
  65)


;;;;;; Binding

(xht--describe
  "Functions to bind the values of a hash table to variables.
The main and the helper functions of ‘h-let’ were inspired by, and
adapted from, ‘let-alist’.")

;;;;;;; Dot-binding (h-let and h-let-it)

;; ‘xht-do’ uses ‘h-let’, and this compiler error happened at the former:
;;   Error: Symbol’s function definition is void: ‘xht--let-deep-dot-search’
;; Adding an autoload to this private function solved it, but so did wrapping
;; all four in ‘eval-and-compile’, which seems preferable.

(eval-and-compile
  (defun xht--let-deep-dot-search (data)
    "Return alist of symbols inside DATA that start with a '.'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the '.'."
    (declare (pure t) (side-effect-free t))
    (cond
     ((symbolp data)
      (let ((name (symbol-name data)))
        (when (string-match "\\`\\." name)
          ;; Return the cons cell inside a list, so it can be appended
          ;; with other results in the clause below.
          (list (cons data (intern (replace-match "" nil nil name)))))))
     ((not (consp data)) nil)
     (t (append (xht--let-deep-dot-search (car data))
                (xht--let-deep-dot-search (cdr data))))))

  (defun xht--let-access-sexp (symbol variable)
    "Return a sexp used to access SYMBOL inside VARIABLE."
    (declare (pure t) (side-effect-free t))
    (let* ((clean (xht--let-remove-dot symbol))
           (name  (symbol-name clean)))
      (if (string-match "\\`\\." name)
          clean
        (xht--let-list-to-sexp
         (mapcar #'intern (nreverse (split-string name "\\.")))
         variable))))

  (defun xht--let-list-to-sexp (list var)
    "Turn symbols LIST into recursive calls to ‘gethash’ on VAR.
The symbol will be successively searched also formatted as string,
keyword, number."
    (declare (pure t) (side-effect-free t))
    (let* ((tbl (if (cdr list)
                    (xht--let-list-to-sexp (cdr list) var)
                  var))
           (cl  (car list))                 ;symbol
           (scl (format "%s" cl))           ;string
           (kcl (intern (concat ":" scl)))  ;keyword
           (rcl (read scl))
           (ncl (when (numberp rcl) rcl)))  ;maybe number
      `(or (gethash ',cl   ,tbl)
           (gethash  ,scl  ,tbl)
           (gethash  ,kcl  ,tbl)
           (gethash  ,ncl  ,tbl))))

  (defun xht--let-remove-dot (symbol)
    "Return SYMBOL without the initial dot."
    (declare (pure t) (side-effect-free t))
    (let ((name (symbol-name symbol)))
      (if (string-match "\\`\\." name)
          (intern (replace-match "" nil nil name))
        symbol))))

;;;###autoload
(defmacro h-let (table &rest body)
  "Let-bind dotted symbols to their value in TABLE and execute BODY.
Dotted symbol is any symbol starting with a dot. Only those present
in BODY are let-bound, and this search is done at compile time.

For instance, the following code:

#+begin_src emacs-lisp
  (h-let table
    (if (and .title .body)
        .body
      .site
      .site.contents))
#+end_src

essentially expands to:

#+begin_src emacs-lisp
  (let ((.title (h-get 'title table))
        (.body  (h-get 'body  table))
        (.site  (h-get 'site  table))
        (.site.contents (h-get 'contents (h-get 'site table))))
    (if (and .title .body)
        .body
      .site
      .site.contents))
#+end_src

That is still somewhat simplified, however. In fact, this line:

#+begin_src emacs-lisp
  (h-get 'title table)
#+end_src

looks more like:

#+begin_src emacs-lisp
  (or (h-get \"title\" table)
      (h-get 'title  table)
      (h-get :title  table))
#+end_src

so that your query should work regardless of whether the key is a
string, a symbol, or a keyword. (Or even number.)

Note that if you mix these types in the keys and two keys happen to
share the same word (such as having both 'title and :title as keys
of the same table), results will be unpredictable. But this case
should be as rare as it's frequent the need for type autodetection
in a given one-typed-key hash table.

If you nest ‘h-let’ invocations, the inner one can't access the
variables of the outer one. You can, however, access nested hash
tables by using dots inside the symbol, as showed in the example
above.

This function was inspired by, and adapted from, ‘let-alist’.

See also: ‘h-let-it’."
  (declare (indent 1) (debug t))
  (let ((var (make-symbol "table")))
    `(let ((,var ,table))
       (let ,(mapcar (lambda (x)
                       `(,(car x)
                         ,(xht--let-access-sexp (car x) var)))
                     (delete-dups (xht--let-deep-dot-search body)))
         ,@body))))

;;;###autoload
(defmacro h-let-it (thing &rest body)
  "Let-bind dotted symbols to their value in THING and execute BODY.
Behind the scenes, it converts key–value THING to hash table using
‘h<-it’, and then dot-binds the result using ‘h-let’."
  (declare (indent 1) (debug t))
  `(funcall #'h-let (h<-it ,thing) ,@body))

;;;;;; Canonical representations
;;;;;;; Changing canonical representation — from table

(xht--describe
  "Functions returning a canonical form that would've generated a hash table.

Note that hash tables will fail equality tests such as:
#+begin_src emacs-lisp
  (eq     #s(hash-table test equal data ('a 1))
          #s(hash-table test equal data ('a 1))) ;=> nil
#+end_src

or:
#+begin_src emacs-lisp
  (eql    #s(hash-table test equal data ('a 1))
          #s(hash-table test equal data ('a 1))) ;=> nil
#+end_src

or:
#+begin_src emacs-lisp
  (equal  #s(hash-table test equal data ('a 1))
          #s(hash-table test equal data ('a 1))) ;=> nil
#+end_src

or:
#+begin_src emacs-lisp
  (equal  (ht ('a 1))  (ht ('a 1))) => nil
#+end_src

But, of course:
#+begin_src emacs-lisp
  (equal '(ht ('a 1)) '(ht ('a 1))) => t
  (equal '(h*  'a 1)  '(h*  'a 1))  => t
#+end_src

So the comparisons below work.

Note: h-htbl-form, not shown here, is just the table itself in
regular form: #s(hash-table…)")

;;;;;;;; #s(hash-table…)

(defsubst h-htbl-form (table)
  "Hash TABLE object as itself.
Function defined only so we have a parallel with ht and h* forms."
  (declare (pure t) (side-effect-free t))
  table)

(defun h-htbl-cm-str (table)
  "Hash TABLE object as a string in compact representation."
  (declare (pure t) (side-effect-free t))
  (format "%S" table))

(defun h-htbl-pp-str (table)
  "Hash TABLE object as a string in pretty-printed representation."
  (declare (pure t) (side-effect-free t))
  (pp-to-string table))

;;;;;;;; (ht…)

(defun h-ht-form (table)
  "Restore canonical (ht…) form that creates hash TABLE.
Note that this doesn't preserve TABLE's equality test: (ht…)
defaults to 'equal as test, so if TABLE's test is anything else, it
will become 'equal if the form is evaluated."
  (declare (pure t) (side-effect-free t))
  (let* (result
         (print-quoted t)
         (keyvals (dolist (key (h-keys table 'rev) result)
                    (let ((value (h-get table key)))
                      (push (list (xht--quote key)
                                  (if (ht? value)
                                      (h-ht-form value) ;<-- recurse
                                    (if (equal 't value)
                                        value
                                      (xht--quote value))))
                            result)))))
    `(ht ,@keyvals)))

(defun h-ht-cm-str (table)
  "Restore as string canonical (ht…) form that creates hash TABLE.
Compact representation."
  (declare (pure t) (side-effect-free t))
  (let ((print-quoted t))
    (->> table  h-ht-form  (format "%S"))))

(defun h-ht-pp-str (table)
  "The pretty-print string of (ht…) form that creates hash TABLE."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((print-quoted t))
      (->> table  h-ht-form  pp  s-trim
           (replace-regexp-in-string "(ht\n *"      "(ht ")
           (replace-regexp-in-string "\\([^)]\\)\n *" "\\1 ")
           insert)
      (indent-region (point-min) (point-max))
      (xht--buff-str-no-prop))))

;;;;;;;; (h*…) and (h-st*…)

(defun h-h*-form (table)
  "Restore canonical (h*…) or (h-st*…) form that creates hash TABLE.
If TABLE's test is:

- 'equal, we use (h*…), which preserves it. Note that if this form
  is evaluated the nominal size of that table will be determined by
  ‘h*’. (This is unlikely to be an issue.)

- not 'equal, we recover its nominal size and equality test and
  use (h-st*…)."
  (declare (pure t) (side-effect-free t))
  (let* (result
         (print-quoted t)
         (keyvals (dolist (key (h-keys table 'rev) result)
                    (let ((value (h-get table key)))
                      (push (if (ht? value)
                                (h-h*-form value) ;<-- recurse
                              (if (equal 't value)
                                  value
                                (xht--quote value)))
                            result)
                      (push (xht--quote key)
                            result)))))
    (if (equal 'equal (h-prop table 'test))
        `(h* ,@keyvals)
      `(h-st* ,(h-prop table 'size)
              ',(h-prop table 'test)
              ,@keyvals))))

(defun h-h*-cm-str (table)
  "Restore as string (h*…) or (h-st*…) form that creates hash TABLE.
Compact representation."
  (declare (pure t) (side-effect-free t))
  (->> table  h-h*-form  (format "%S")))

(defun h-h*-pp-str (table)
  "The pretty-print string of (h*…) form that creates hash TABLE.
If table's test isn't 'equal, use (h-st*…) form."
  (let ((print-quoted t))
    (with-temp-buffer
      (with-syntax-table emacs-lisp-mode-syntax-table
        (->> table  h-h*-cm-str  s-trim  insert)
        (goto-char (point-min))
        (while (not (eobp))
          (cond ((or (looking-at " *(h\\* ")
                     (looking-at " *(h-st\\* "))
                 (goto-char (scan-lists (point) 1 -1))
                 (goto-char (scan-sexps (point) 2)))
                (t
                 (goto-char (scan-sexps (point) 1))
                 (when (looking-at " *)")
                   (re-search-forward " *)+" nil t))
                 (re-search-forward " +$" nil t)
                 (unless (eobp)
                   (insert "\n"
                           (let ((sz (save-excursion
                                       (backward-sexp 2)
                                       (1- (current-column)))))
                             (if (>= sz 0)
                                 (make-string sz ?\s)
                               "")))
                   (goto-char (scan-sexps (point) 1))))))
        (xht--buff-str-no-prop)))))

;;;;;;; Changing canonical representation — from any ht-like form

(xht--describe
  "Choose different representations of hash tables.")

(defun h-lambdify (ht-like)
  "Convert an HT-LIKE object into a lambda that creates the table.
HT-LIKE may come in any of the following autodetected formats:

As Lisp objects:
1. #s(hash-table…) = regular representation
2. (ht…) form
3. (h*…), (h-s*…), (h-t*…), or (h-st*…) form

or any of the above formatted as string (presumably with %S)."
  (pcase (xht--ht-like-format-type ht-like)
    ((or :ht-str :h*-str :h-s*-str :h-t*-str :h-st*-str)
     `(lambda () ,(read ht-like)))
    ((or :ht-form :h*-form :h-s*-form :h-t*-form :h-st*-form
         :htbl)
     `(lambda () ,ht-like))
    (:no-hash-table
     (user-error "‘h-lambdify’: Not hash-table–like — won't lambdify"))))

(defun h-format (format ht-like)
  "Format as FORMAT hash table–like object HT-LIKE.
HT-LIKE may come in any of the following autodetected formats:

As Lisp objects:
1. #s(hash-table…) = regular representation
2. (ht…) form
3. (h*…), (h-s*…), (h-t*…), or (h-st*…) form

or as strings:
Any of the above formatted as string (presumably with %S)

The output FORMAT, in turn, must be specified as one of:
1. htbl         :: #s(hash-table…) (Lisp object)
2. h*-form      :: (h*…) form      (Lisp object)
3. ht-form      :: (ht…) form      (Lisp object)

4. htbl-cm-str  :: #s(hash-table…) string in compact format
5. htbl-pp-str  :: #s(hash-table…) string in pretty-printed format
6. h*-cm-str    :: (h*…) string in compact format
7. h*-pp-str    :: (h*…) string in pretty-printed format
8. ht-cm-str    :: (ht…) string in compact format
9. ht-pp-str    :: (ht…) string in pretty-printed format

Any of these FORMAT options may be passed as either a string, a
keyword, or a symbol — as you prefer. So, for example, any among
\"htbl\", :htbl, or 'htbl works."
  (declare (pure t) (side-effect-free t))
  (let ((as-htbl
         (pcase (xht--ht-like-format-type ht-like)
           ((or :ht-str
                :h*-str
                :h-s*-str
                :h-t*-str
                :h-st*-str)  (-> ht-like  read  xht--dequote-list  eval))
           ;; it's likely not 'quoted in the str, ^ but it could be.
           ((or :ht-form
                :h*-form
                :h-s*-form
                :h-t*-form
                :h-st*-form) (-> ht-like        xht--dequote-list  eval))
           (:htbl-str        (-> ht-like  read))
           (:htbl                ht-like)
           (:no-hash-table
            (user-error "‘h-format’: Not a hash table: %s" ht-like)))))
    (pcase (h-as-keyword format)
      (:htbl                         as-htbl)
      (:h*-form      (h-h*-form      as-htbl))
      (:ht-form      (h-ht-form      as-htbl))
      (:htbl-cm-str  (h-htbl-cm-str  as-htbl))
      (:htbl-pp-str  (h-htbl-pp-str  as-htbl))
      (:h*-cm-str    (h-h*-cm-str    as-htbl))
      (:h*-pp-str    (h-h*-pp-str    as-htbl))
      (:ht-cm-str    (h-ht-cm-str    as-htbl))
      (:ht-pp-str    (h-ht-pp-str    as-htbl))
      (_ (user-error "‘h-format’: Not a reformat output option")))))

;;;;;;;; helper

(defun xht--ht-like-format-type (ht-like)
  "Return type of hash table representation of HT-LIKE."
  (declare (pure t) (side-effect-free t))
  (let* ((str?    (stringp ht-like))
         (lispobj (if str? (read ht-like) ht-like)))
    ;; Note: when string we won't test whether it's pretty-printed or not.
    ;; This is a bit tricky, so we'll take a simpler route.
    (cond ((h?    lispobj) (if str? :htbl-str :htbl))
          ((listp lispobj)
           (setq lispobj (xht--dequote-list lispobj))
           ;; it could be '''multiply-quoted^, so we remove excesses.
           (pcase (car lispobj)
             ('ht    (if str? :ht-str    :ht-form))
             ('h*    (if str? :h*-str    :h*-form))
             ('h-s*  (if str? :h-s*-str  :h-s*-form))
             ('h-t*  (if str? :h-t*-str  :h-t*-form))
             ('h-st* (if str? :h-st*-str :h-st*-form))
             ;; ^note that 'st' is about 'size' and 'test': nothing
             ;; to do with 'string', as is the case of '-str'.
             (_ :no-hash-table)))
          (t :no-hash-table))))

;;;;;; Mapping from hash table
;;;;;;; to vector

(xht--describe
  "Functions that apply a function to every key–value pair of a
hash table and write the results to a vector.

In particular, here's a guideline for the 'vmap family' of
functions:

- the added 'v' means 'result is a vector'
-  an added '-' means 'anaphoric: enter a form, not a lambda'

In the anaphoric version, if you don't use any or either of the
let-bound variables key and value in any or either of the forms,
it's ok — no warnings.

Summary:

Side-effect-free: don't modify original TABLE, return a vector
|----------------------+--------------+---------------------|
|                      | a lambda     | a form (anaphoric)  |
|----------------------+--------------+---------------------|
| vector may have nils | ‘h-vmap’     | ‘h--vmap’           |
|----------------------+--------------+---------------------|")

(defun h-vmap (fun table &optional reverse)
  "Apply FUN to each key–value pair of TABLE and return vector.
Function FUN is called with two arguments, key and value.

If REVERSE is non-nil, show it in reverse order.

If you don't use both of these variables, then to avoid warnings
use an underscore before the to-be-ignored variable. For example,
to return a vector of all (expected to be integer) values plus 1:

#+begin_src emacs-lisp
  (h-vmap (lambda (_key value) (1+ value)) table)
#+end_src"
  (declare (pure t) (side-effect-free t))
  (let* ((size    (h-size table))
         (results (make-vector size nil))
         (pos     (if reverse
                      (lambda (idx) (- size idx 1))
                    (lambda (idx) idx)))

         (idx     0))
    (maphash (lambda (key value)
               (aset results (funcall pos idx) (funcall fun key value))
               (setq idx (1+ idx)))
             table)
    results))

(defmacro h--vmap (form table &optional reverse)
  "Anaphoric version of ‘h-vmap’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound. Results given as vector.

If REVERSE is non-nil, show it in reverse order."
  `(h-vmap (lambda (key value) (ignore key value) ,form)
           ,table ,reverse))

(defmacro h--vunzip-with (key-form val-form table &optional reverse)
  "Anaphoric version of ‘h-vunzip-with’.
The form KEY-FORM modifies each key.
The form VAL-FORM modifies each value.

Both forms take two variables: key and value, respectively.

Each key and value is taken one by one from their corresponding
pairs at the hash table TABLE.

If REVERSE is non-nil, reverse the order of both vectors.

The (roughly) inverse of this function is ‘h-zip-vectors’.

Alias: ‘h--unzip-with’.

See also: ‘h--vunzip-with’."
  (let ((sz (make-symbol "size"))
        (ks (make-symbol "keys"))
        (vs (make-symbol "values"))
        (p  (make-symbol "pos"))
        (i  (make-symbol "i")))
    `(let* ((,sz (h-size ,table))
            (,ks (make-vector ,sz nil))
            (,vs (make-vector ,sz nil))
            (,p  (if ,reverse
                     (lambda (i) (- ,sz i 1))
                   (lambda (i) i)))
            (,i  0))
       (maphash (lambda (key value)
                  (ignore key value)
                  (aset ,ks (funcall ,p ,i) ,key-form)
                  (aset ,vs (funcall ,p ,i) ,val-form)
                  (setq ,i (1+ ,i)))
                ,table)
       (list ,ks ,vs))))

(defun h-vunzip-with (key-fun val-fun table &optional reverse)
  "Return a list of modified vectors of KEYS and VALUES from TABLE.
If REVERSE is non-nil, reverse the order of both vectors.

The function KEY-FUN modifies each key in the results.
The function VAL-FUN modifies each value in the results.

Both functions take two variables: key and value, respectively.

Each key and value is taken one by one from their corresponding
pairs at the hash table TABLE.

The (roughly) inverse of this function is ‘h-zip-vectors’.

Alias: ‘h-unzip-with’.

See also: ‘h-vunzip-with’."
  (declare (pure t) (side-effect-free t))
  (h--vunzip-with (funcall key-fun key value)
                  (funcall val-fun key value)
                  table reverse))

(defun h-vunzip (table &optional reverse)
  "Return a list of the vectors of KEYS and VALUES from TABLE.
If REVERSE is non-nil, reverse the order of both vectors.

The (roughly) inverse of this function is ‘h-zip-vectors’.

Alias: ‘h-unzip’.

See also: ‘h-vunzip’."
  (declare (pure t) (side-effect-free t))
  (h--vunzip-with key value table reverse))

(defun h-vitems (table &optional reverse)
  "Return a vector of two-element lists '(key value) from TABLE.
If REVERSE is non-nil, show it in reverse order (the vector;
the pair remains in the (key, value) order)."
  (declare (pure t) (side-effect-free t))
  (h--vmap (list key value) table reverse))

(defmacro h-vkeys (table &optional reverse)
  "Return a vector of all the keys in TABLE.
If REVERSE is non-nil, show it in reverse order."
  (declare (pure t) (side-effect-free t))
  `(h--vmap key ,table ,reverse))

(defmacro h-vvalues (table &optional reverse)
  "Return a vector of all the values in TABLE.
If REVERSE is non-nil, show it in reverse order."
  (declare (pure t) (side-effect-free t))
  `(h--vmap value ,table ,reverse))

(defun h-vrandom (table &optional n)
  "Return a vector of N pseudo-randomly chosen items from hash TABLE.
N must be an integer; if not, signal error.

- If N is nil, make it N=1.

- If N≥ size(TABLE), return a vector of all TABLE's items (same
  result as running ‘h-vitems’).

- If N=0, return nil.

- If N=1, item is returned as a two-item list (key, value).

- If N>1, items are returned as a vector of two-item lists (key, value).

- If N is negative, make N = N + size(TABLE). For example, if TABLE
  has 10 items and N=-2, return 8 pseudo-random items.

See also: ‘h-lrandom’."
  (declare (side-effect-free t))
  (let ((size (h-size table)))
    (cond
     ((null n) (setq n 1))
     ((not (integerp n))
      (user-error "‘h-vrandom’: n must be an integer"))
     ((< n 0)  (setq n (+ size n))))
    (cond
     ((= n 0)     nil)
     ((>= n size) (h-vitems table))
     (t           (let ((sel-keys (make-vector n nil))
                        (keys     (h-keys table))
                        idx key)
                    (while (> n 0)
                      (setq idx  (random (length keys))
                            key  (nth idx keys)
                            keys (-remove-at idx keys)
                            n    (1- n))
                      (aset sel-keys n (list key (h-get table key))))
                    sel-keys)))))

;;;;;;; to list

(xht--describe
  "Functions that apply a function to every key–value pair of a
hash table and write the results to a list.

In particular, here's a guideline for the 'lmap/lkeep family' of
functions:

- the added 'l' means 'result is a list'
-  an added '-' means 'anaphoric: enter a form, not a lambda'

In the anaphoric versions, if you don't use any or either of the
let-bound variables key and value in any or either of the forms,
it's ok — no warnings.

Summary:

Side-effect-free: don't modify original TABLE, return a list
|----------------------+--------------+---------------------|
|                      | a lambda     | a form (anaphoric)  |
|----------------------+--------------+---------------------|
| list may have nils   | ‘h-lmap’     | ‘h--lmap’           |
| any nils are removed | ‘h-lkeep’    | ‘h--lkeep’          |
|----------------------+--------------+---------------------|")

(defun h-lmap (fun table &optional reverse)
  "Apply FUN to each key–value pair of TABLE; list the results.
Function FUN is called with two arguments, key and value.

Identical to current ‘ht-map’, with the difference that the
returned list matches the current order of the TABLE's keys.

If REVERSE is non-nil, show it in reverse order.

If you don't use both of these variables, then to avoid warnings
use an underscore before the to-be-ignored variable. For example,
to return a list of all (expected to be integer) values plus 1:

#+begin_src emacs-lisp
  (h-lmap (lambda (_key value) (1+ value)) table).
#+end_src"
  ;; Note: Using ‘nreverse’ after push seemed to me as more natural for
  ;; mapping, especially when one had control of the order of items added to
  ;; the hash table.
  ;;
  ;; The cost seems worth it, since ‘nreverse’ takes less than a millisecond
  ;; to reverse a list with 100k elements — and I suspect that one dealing
  ;; with a hash table that large will be unlikely to want to map the whole of
  ;; it to a list. And when that is the case and 1 ms is too much, pass a
  ;; non-nil REVERSE flag.
  ;;
  ;; For simpler, smaller hash tables, a delay in the order of microseconds
  ;; won't matter, and the user will likely be more interested in reproducing
  ;; the items in the order that he or she added.
  (declare (pure t) (side-effect-free t))
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall fun key value) results))
     table)
    ;; That's right, because ‘push’ reversed it already:
    (if reverse
        results
      (nreverse results))))

(defmacro h--lmap (form table &optional reverse)
  "Anaphoric version of ‘h-lmap’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound. List the results.

If REVERSE is non-nil, show it in reverse order."
  `(h-lmap (lambda (key value) (ignore key value) ,form)
           ,table ,reverse))

(defun h-lkeep (fun table &optional reverse)
  "Apply FUN to each key–value pair of TABLE; list non-nil results.

We follow Dash's naming scheme here, for which 'keep' is just like
'map' — but with the nils removed.

Function FUN is called with two arguments, key and value.

If REVERSE is non-nil, show it in reverse order.

As with ‘h-lmap’, if you don't use both of these variables, then to
avoid warnings use an underscore before the to-be-ignored variable."
  (declare (pure t) (side-effect-free t))
  (let (results)
    (maphash
     (lambda (key value)
       (when-let ((fkv (funcall fun key value)))
         (push fkv results)))
     table)
    ;; That's right, because ‘push’ reversed it already:
    (if reverse
        results
      (nreverse results))))

(defmacro h--lkeep (form table &optional reverse)
  "Anaphoric version of ‘h-lkeep’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound. List the non-nil results.

If REVERSE is non-nil, show it in reverse order."
  `(h-lkeep (lambda (key value) (ignore key value) ,form)
            ,table ,reverse))

(defmacro h--lunzip-with (key-form val-form table &optional reverse)
  "Anaphoric version of ‘h-lunzip-with’.

The form KEY-FORM modifies each key.
The form VAL-FORM modifies each value.

Each element of KEYS   is bound to the symbol key.
Each element of VALUES is bound to the symbol value.

Each key and value is taken one by one from their corresponding
pairs at the hash table TABLE.

If REVERSE is non-nil, reverse the order of both lists.

The (roughly) inverse of this function is ‘h-zip-lists’.

Alias: ‘h--unzip-with’.

See also: ‘h--vunzip-with’."
  (let ((ks (make-symbol "keys"))
        (vs (make-symbol "values")))
    `(let (,ks ,vs)
       (maphash (lambda (key value)
                  (ignore key value)
                  (push ,key-form ,ks)
                  (push ,val-form ,vs))
                ,table)
       (list (if ,reverse ,ks (nreverse ,ks))
             (if ,reverse ,vs (nreverse ,vs))))))

(defun h-lunzip-with (key-fun val-fun table &optional reverse)
  "Return a list of modified lists of KEYS and VALUES from TABLE.
If REVERSE is non-nil, reverse the order of both lists.

The function KEY-FUN modifies each key in the results.
The function VAL-FUN modifies each value in the results.

Both functions take two variables: key and value, respectively.

Each key and value is taken one by one from their corresponding
pairs at the hash table TABLE.

The (roughly) inverse of this function is ‘h-zip-lists’.

Alias: ‘h-unzip-with’.

See also: ‘h-vunzip-with’."
  (declare (pure t) (side-effect-free t))
  (h--lunzip-with (funcall key-fun key value)
                  (funcall val-fun key value)
                  table reverse))

(defun h-lunzip (table &optional reverse)
  "Return a list of the lists of KEYS and VALUES from TABLE.
If REVERSE is non-nil, reverse the order of both lists.

The (roughly) inverse of this function is ‘h-zip-lists’.

Alias: ‘h-unzip’.

See also: ‘h-vunzip’."
  (declare (pure t) (side-effect-free t))
  (h--lunzip-with key value table reverse))

(defun h-litems (table &optional reverse)
  "Return a list of two-element lists '(key value) from TABLE.
Like ‘ht-items’, but in TABLE's stored order by default.

If REVERSE is non-nil, show it in reverse order (the larger list;
the pair remains in the (key, value) order).

Alias: ‘h-items’."
  (declare (pure t) (side-effect-free t))
  (h--lmap (list key value) table reverse))

(defun h-lkeys (table &optional reverse)
  "Return a list of all the keys in TABLE.
Like ‘ht-keys’, but in TABLE's stored order by default.

If REVERSE is non-nil, show it in reverse order.

Aliases: ‘h-keys’, ‘h-2d-keys’."
  (declare (pure t) (side-effect-free t))
  (h-lmap (lambda (key _value) key) table reverse))

(defun h-lvalues (table &optional reverse)
  "Return a list of all the values in TABLE.
Like ‘ht-values’, but in TABLE's stored order.

If REVERSE is non-nil, show it in reverse order.

Alias: ‘h-values’."
  (declare (pure t) (side-effect-free t))
  (h-lmap (lambda (_key value) value) table reverse))

(defun h-lrandom (table &optional n)
  "Return a list of N pseudo-randomly chosen items from hash TABLE.
N must be an integer; if not, signal error.

- If N is nil, make it N=1.

- If N≥ size(TABLE), return a list of all TABLE's items in pseudo-random order

  (like ‘h-litems’, but shuffled).

- If N=0, return nil.

- If N=1, item is returned as a two-item list (key, value).

- If N>1, items are returned as a list of two-item lists (key, value).

- If N is negative, make N = N + size(TABLE). For example, if TABLE has 10
  items and N=-2, return 8 pseudo-random items.

See also: ‘h-vrandom’, ‘h-pop-random’, and ‘h-pop-random!’."
  (declare (side-effect-free t))
  (let ((size (h-size table)))
    ;; Adjust n:
    (cond ((null n)           (setq n 1))
          ((not (integerp n)) (user-error
                               "‘h-lrandom’: n must be an integer"))
          ((< n 0)            (setq n (+ size n)))
          ((>= n size)        (setq n size)))
    ;; Decide on n:
    (if (= n 0)
        nil
      (let ((keys (h-keys table))
            idx key sel-keys)
        (while (> n 0)
          (setq idx  (random (length keys))
                key  (nth idx keys)
                keys (-remove-at idx keys)
                n    (1- n))
          (push (list key (h-get table key))
                sel-keys))
        sel-keys))))

;;;;;;; to hash table

(xht--describe
  "Functions that apply two functions to every key–value pair of a hash table
and write the results to a hash table.

For example, if you want to produce a fresh table whose keys
\(currently strings) are upcased and the values \(currently
integers) are increased by 1, you could run:

#+begin_src emacs-lisp
  (h-hmap (lambda (key _value) (upcase key))
          (lambda (_key value) (1+ value))
          table)
#+end_src

The underlines above are so that there's no warnings about unused
variables. We do it because we didn't use VALUE in KEY-FUN, nor KEY
in VAL-FUN — but this doesn't need to be so: we could want that the
new key be also (or instead) a function of the VALUE, and
vice-versa.

Note that the pair is only added if KEY-FUN returns non-nil. So no
spurious (nil, something) pair results from some reasonable
conditional KEY-FUN such as:

#+begin_src emacs-lisp
  (lambda (key, value) (when (> value 2) key))
#+end_src

In the rare case where for some reason you chose to have nil as a
key, you'll have to treat it specially.

IMPORTANT: unlike mapping to a list, mapping to a hash table
demands that the results of the keys be unique. So you must pay
attention to possible collisions. If, for example, in the case
above the original table had both \"a\" and \"A\" as original keys,
one of them would end up overwritten, because both return \"A\"
when upcased. Which one will depend on the key order. So:

  actual size of resulting table  ≤  size of original table.

In mathematical terms, excepting the rare case of a nil key in the
original, the number of keys in the resulting hash table will match
the number of keys in the original one if, and only if:

1. there's a bijection between the set of keys and the set of
   key-fun(key,value).

2. there's no (key,value) for which key-fun(key,value) returns nil.


Guideline for the 'hmap family' of functions:

- the added 'h' means 'result is a hash table'

-  an added '-' means 'anaphoric: enter 2 forms, not 2 lambdas'

-  an added '*' means 'recurse: apply the values-function or -form
                       to the values whenever these are hash tables'

-  an added '!' means 'destructive: modify TABLE instead of
                       creating a fresh one'

In the anaphoric versions, if you don't use any or either of the
let-bound variables key and value in any or either of the forms,
it's ok — no warnings.

Summary:

Side-effect-free: don't modify original TABLE, return fresh table
|-------------------------+---------------+---------------------|
|                         | 2 lambdas     | 2 forms (anaphoric) |
|-------------------------+---------------+---------------------|
| simple, doesn't recurse | ‘h-hmap’      | ‘h--hmap’           |
| nesting-aware, recurses | ‘h-hmap*’     | ‘h--hmap*’          |
|-------------------------+---------------+---------------------|

Destructive: modify original TABLE, return nil
|-------------------------+---------------+---------------------|
|                         | 2 lambdas     | 2 forms (anaphoric) |
|-------------------------+---------------+---------------------|
| simple, doesn't recurse | ‘h-hmap!’     | ‘h--hmap!’          |
| nesting-aware, recurses | ‘h-hmap*!’    | ‘h--hmap*!’         |
|-------------------------+---------------+---------------------|

See also: ‘h-2d-hmap’, ‘h--2d-hmap’, ‘h-2d-hmap!’, ‘h--2d-hmap!’")

;;;;;;;; non-recursive

(defun h-hmap (key-fun val-fun table)
  "Return a table from applying KEY-FUN, VAL-FUN to each pair of TABLE.
While ‘h-lmap’ produces a list, this one produces a hash table.

Both KEY-FUN and VAL-FUN are each called with two arguments, KEY
and VALUE.
- The result of KEY-FUN is the new key.
- The result of VAL-FUN is the new value."
  (declare (pure t) (side-effect-free t))
  (let ((result (h-clr table)))
    (maphash
     (lambda (key value)
       (let ((newk (funcall key-fun key value)))
         (when newk
           (puthash newk
                    (funcall val-fun key value)
                    result))))
     table)
    result))

(defmacro h--hmap (key-form val-form table)
  "Anaphoric version of ‘h-hmap’.

The form KEY-FORM modifies each key.
The form VAL-FORM modifies each value.

For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound.

While ‘h--lkeep’ produces a list, this one produces a hash table."
  `(h-hmap (lambda (key value) (ignore key value) ,key-form)
           (lambda (key value) (ignore key value) ,val-form)
           ,table))

(defun h-hmap! (key-fun val-fun table)
  "Update TABLE by applying KEY-FUN, VAL-FUN to its pairs.
Just like ‘h-hmap’ (which see), but modifies TABLE instead of
producing a fresh one."
  (maphash
   (lambda (key value)
     (let ((newk (funcall key-fun key value)))
       (remhash key table)
       (when newk
         (puthash newk
                  (funcall val-fun key value)
                  table))))
   table))

(defmacro h--hmap! (key-form val-form table)
  "Anaphoric version of ‘h-hmap!’.
For every key–value pair in TABLE, evaluate KEY-FORM and VAL-FORM
with the variables key and value bound."
  `(h-hmap! (lambda (key value) (ignore key value) ,key-form)
            (lambda (key value) (ignore key value) ,val-form)
            ,table))

;;;;;;;; recursive

(defun h-hmap* (key-fun val-fun table)
  "Return a table from applying KEY-FUN, VAL-FUN to each pair of TABLE.
Just like ‘h-hmap’, but recurses wherever TABLE is nested.

This means that for any VALUE that is itself a hash table, instead
of replacing it with the result of VAL-FUN, as ‘h-hmap’ would
do, VALUE will be this very ‘h-hmap*’ recursively applied to
that hash table with KEY-FUN and VAL-FUN."
  (declare (pure t) (side-effect-free t))
  (let ((result (h-clr table)))
    (maphash
     (lambda (key value)
       (let ((newk (funcall key-fun key value)))
         (when newk
           (puthash newk
                    (if (ht? value)
                        (h-hmap* key-fun val-fun value)
                      ;; ^ recurses
                      (funcall val-fun key value))
                    result))))
     table)
    result))

(defmacro h--hmap* (key-form val-form table)
  "Anaphoric version of ‘h-hmap*’.
For every key–value pair in TABLE, evaluate KEY-FORM and VAL-FORM
with the variables key and value bound."
  `(h-hmap* (lambda (key value) (ignore key value) ,key-form)
            (lambda (key value) (ignore key value) ,val-form)
            ,table))

(defun h-hmap*! (key-fun val-fun table)
  "Update TABLE by applying KEY-FUN, VAL-FUN to each of its pairs.
Just like ‘h-hmap*’ (which see), but modifies TABLE instead of
producing a fresh one."
  (maphash
   (lambda (key value)
     (let ((newk (funcall key-fun key value)))
       (remhash key table)
       (when newk
         (puthash newk
                  (if (ht? value)
                      (h-hmap* key-fun val-fun value)
                    (funcall val-fun key value))
                  table))))
   table))

(defmacro h--hmap*! (key-form val-form table)
  "Anaphoric version of ‘h-hmap*!’.
For every key–value pair in TABLE, evaluate KEY-FORM and VAL-FORM
with the variables key and value bound."
  `(h-hmap*! (lambda (key value) (ignore key value) ,key-form)
             (lambda (key value) (ignore key value) ,val-form)
             ,table))

;;;;;;; for side-effects only

(xht--describe
  "Functions that map over the hash table for producing side-effects
somewhere.")

(defun h-each (table fun)
  "Apply function FUN to each key–value pair of TABLE.
FUN is called with two arguments: key and value.

Intended to be used for side-effects only. Returns nil.

This function is similar to current ‘ht-each’ (in turn an alias to
‘maphash’), but with TABLE as the first argument."
  (declare (indent 1))
  (maphash fun table))

(defmacro h--each (table &rest body)
  "Anaphoric version of ‘h-each’.
For every key–value pair in TABLE, evaluate BODY with the variables
key and value bound.

This function is similar to current ‘ht-aeach’. Differences:

1. TABLE is the first argument.

2. BODY instead of FORM, obviating the need of ‘progn’ whenever
   more than one form is needed.

3. issues no warnings if either KEY or VALUE isn't used."
  (declare (debug (sexp body)) (indent 1))
  `(maphash (lambda (key value)
              (ignore key value)
              ,@body)
            ,table))

;;;;;; Keys operations
;;;;;;; Retrieval (getting)

(xht--describe
  "Functions that retrieve key–value pairs from tables.")

;;;;;;;; Return value given key

(xht--describe
  "Currently, there are no functions here.
- ‘h-get’  is aliased to ‘ht-get’.
- ‘h-get*’ is aliased to ‘ht-get*’.")

;;;;;;;; Return the very first pair (pop)

(defun h-pop (table)
  "Return the very first pair from TABLE.
Pair is returned as a two-element list: '(key value).

Note that this function is not destructive: it won't remove the
pair from TABLE. For that, use ‘h-pop!’."
  (catch 'break
    (maphash
     (lambda (key value)
       (throw 'break (list key value)))
     table)))

(defun h-pop! (table)
  "Return the very first pair from TABLE and remove it.
Pair is returned as a two-element list: '(key value).

If you don't want it removed, use ‘h-pop’ instead."
  (let ((pop (h-pop table)))
    (h-rem! table (car pop))
    pop))

;;;;;;;; Return a random pair

(defun h-pop-random (table)
  "Return a pseudo-random pair from TABLE.
Pair is returned as a two-element list: '(key value).

Note that this function is not destructive: it won't remove the
pair from TABLE. For that, use ‘h-pop-random!’.

See also: ‘h-vrandom’ and ‘h-lrandom’."
  (let ((goal (random (h-size table)))
        (idx  0))
    (catch 'break
      (maphash
       (lambda (key value)
         (when (= idx goal)
           (throw 'break (list key value)))
         (setq idx (1+ idx)))
       table))))

(defun h-pop-random! (table)
  "Return a pseudo-random pair from TABLE and remove it.
Pair is returned as a two-element list: '(key value).

If you don't want it removed, use ‘h-pop-random’.

See also: ‘h-vrandom’ and ‘h-lrandom’."
  (let ((pop (h-pop-random table)))
    (h-rem! table (car pop))
    pop))

;;;;;;;; Return the first pair matching a predicate

(defun h-first (fun table)
  "Return the first pair from TABLE for which FUN returns non-nil.
In this case, pair is returned as a two-element list: '(key value).
Return nil otherwise.

Function FUN is called with two arguments, key and value.

This function is exactly like current ‘ht-find’."
  (catch 'break
    (maphash
     (lambda (key value)
       (when (funcall fun key value)
         (throw 'break (list key value))))
     table)))

(defmacro h--first (form table)
  "Anaphoric version of ‘h-first’.
Return the first pair from TABLE for which FORM returns non-nil.
In this case, pair is returned as a two-element list: '(key value).
Return nil otherwise.

FORM is called with two arguments: key and value."
  `(catch 'break
     (maphash
      (lambda (key value)
        (ignore key value)
        (when ,form
          (throw 'break (list key value))))
      ,table)))

(defun h-first! (fun table)
  "Remove from TABLE the first pair for which FUN returns non-nil.
In this case, pair is returned as a two-element list: '(key value).
Return nil otherwise.

Function FUN is called with two arguments, key and value."
  (let ((first (h-first fun table)))
    (h-rem! table (car first))
    first))

(defmacro h--first! (form table)
  "Anaphoric version of ‘h-first!’.
Remove from TABLE the first pair for which FORM returns non-nil.
In this case, pair is returned as a two-element list: '(key value).
Return nil otherwise.

FORM is called with two arguments, key and value."
  (let ((f (make-symbol "first")))
    `(let ((,f (h--first ,form ,table)))
       (h-rem! ,table (car ,f))
       ,f)))

;;;;;;; Addition (setting)

(xht--describe
  "Functions that add a key–value pair to tables, either destructively or
side-effects-free. Included are those that take a sequence of keys
and a final value — for nested hash tables.

Same logic applies to put functions as with mix functions — only
that it's table + key–value instead of table + tables; and only one
kv pair makes sense \(otherwise it'd be either a hash table with
the pairs, to which we'd apply mixing; or some alist or plist,
which we'd convert to htbl then mix).

It may seem a bit odd to \"put a pair non-destructively\", but the
logic is the same as that of h-mix, and having an exactly analogous
abstraction is useful.")

;;;;;;;; put

(xht--describe "Summary:
|-------------------------+-----------------+-------------|
|                         | Non-destructive | Destructive |
|-------------------------+-----------------+-------------|
| simple, doesn't recurse | ‘h-put’         | ‘h-put!’    |
| nesting-aware, recurses | ‘h-put*’        | ‘h-put*!’   |
|-------------------------+-----------------+-------------|

While the destructive ‘h-put!’ is like ‘ht-set!’, there's no non-destructive
counterparts in ht library.")


(define-inline h-put! (table key value)
  "Associate KEY in TABLE with VALUE.
This is EXACTLY equivalent to current ‘ht-set!’.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-put’.

This function is not nesting-aware.
Its also-destructive but nesting-aware counterpart is ‘h-put*!’.

For nesting-awareness with no side-effects, use ‘h-put*’."
  (inline-quote
   (ignore
    (puthash ,key ,value ,table))))

(defun h-put (table key value)
  "Return a table that is TABLE with KEY–VALUE applied.
This is a side-effect-free function.
Its destructive counterpart is ‘h-put!’.

This function is not nesting-aware.
Its also-side-effect-free but nesting-aware counterpart is ‘h-put*’.

For nesting-awareness with side-effects, use ‘h-put*!’."
  (declare (pure t) (side-effect-free t))
  (let ((htbl (h-clone* table)))
    (h-put! htbl key value)
    htbl))

(defun h-put*! (table &rest keys-value)
  "Set VALUE to KEYS sequence in hash table TABLE.
KEYS-VALUE are KEYS and VALUE.

Each key in KEYS has as value the next, all of which hash tables,
except for the final key, which could return any value.

This last key will be set to VALUE.

If any of the keys in the sequence KEYS can't be found,
they will be created on-the-fly.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-put*’.

This function is nesting-aware.
Its also-destructive but only-first-level counterpart is ‘h-put!’.

For no nesting-awareness and no side-effects, use ‘h-put’."
  (let ((newh (apply #'h-reduce-r keys-value)))
    (h-mix*! table newh)))

(defun h-put* (table &rest keys-value)
  "Return a table that is TABLE with KEYS–VALUE applied.
KEYS-VALUE are KEYS and VALUE.

Each key in KEYS has as value the next, all of which hash tables,
except for the final key, which could return any value.

This last key will be set to VALUE.

If any of the keys in the sequence KEYS can't be found,
they will be created on-the-fly.

This is a side-effect-free function.
Its destructive counterpart is ‘h-put*!’.

This function is nesting-aware.
Its also-side-effect-free but only-first-level counterpart is ‘h-put’.

For no nesting-awareness but with side-effects, use ‘h-put!’."
  (declare (pure t) (side-effect-free t))
  (let ((htbl (h-clone* table))
        (newh (apply #'h-reduce-r keys-value)))
    (h-mix* htbl newh)))

;;;;;;;; put-add

(xht--describe
  "While 'put' replaces any current value (call it 'curval')
with the newly provided value, 'put-add' adds to it.

The idea is that you are adding to a set. So:

| Current value  | New value                                          |
|----------------+----------------------------------------------------|
| #s(hash table) | - if VALUE is also a hash table,                   |
|                |   then (‘h-mix*!’ VALUE CURVAL);                   |
|                | - if VALUE is an explicit key–value structure, try |
|                |   to convert it to hash table, then ‘h-mix*!’;     |
|                | - otherwise, don't add: keep CURVAL intact.        |
|                |                                                    |
|                | By 'explicit' I mean: no implicit ones, such as    |
|                | simple list, vector, or string lines.              |
|                |                                                    |
|                | And for simplicity, 'key–value lines' string       |
|                | is also out.                                       |
|----------------+----------------------------------------------------|
| [some vector]  |  [VALUE some vector]                               |
| '(some list)   | '(VALUE some list)                                 |
| nil            |   VALUE                                            |

With anything else, make a list, adding VALUE to its first place.
Example:

| Current value | New value            |
|---------------+----------------------|
| 42            | '(VALUE 42)          |
| :keyword      | '(VALUE :keyword)    |
| 'symbol       | '(VALUE symbol)      |
| \"string\"    | '(VALUE \"string\")  |
| etc.          |                      |

Summary:
|-------------------------+-----------------+---------------|
|                         | Non-destructive | Destructive   |
|-------------------------+-----------------+---------------|
| simple, doesn't recurse | ‘h-put-add’     | ‘h-put-add!’  |
| nesting-aware, recurses | ‘h-put-add*’    | ‘h-put-add*!’ |
|-------------------------+-----------------+---------------|")

(defun h-put-add! (table key value)
  "Add VALUE to the current value of KEY in hash table TABLE.
While ‘h-put!’ replaces any current value (call it 'curval')
with the newly provided value, ‘h-put-add!’ adds to it.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-put-add’.

This function is not nesting-aware.
Its also-destructive but nesting-aware counterpart is ‘h-put-add*!’.

For nesting-awareness with no side-effects, use ‘h-put-add*’."
  (let* ((curval (ht-get table key))
         (newval (xht--put-add curval value)))
    (h-put! table key newval)))

(defun h-put-add (table key value)
  "Return a table that is TABLE with VALUE added to KEY's current.
While ‘h-put’ replaces any current value with the newly provided
value, ‘h-put-add’ adds to it.

This is a side-effect-free function.
Its destructive counterpart is ‘h-put-add!’.

This function is not nesting-aware.
Its also-side-effect-free but nesting-aware counterpart is ‘h-put-add*’.

For nesting-awareness with side-effects, use ‘h-put-add*!’."
  (declare (pure t) (side-effect-free t))
  (let* ((htbl   (h-clone* table))
         (curval (ht-get table key))
         (newval (xht--put-add curval value)))
    (h-put htbl key newval)))

(defun h-put-add*! (table &rest keys-value)
  "Add VALUE to the current KEYS sequence's value in hash table TABLE.
KEYS-VALUE are KEYS and VALUE.

While ‘h-put*!’ replaces any current value with the newly provided
value, ‘h-put-add*!’ adds to it.

If any of the keys in the sequence KEYS can't be found,
they will be created on-the-fly.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-put-add*’.

This function is nesting-aware.
Its also-destructive but only-first-level counterpart is ‘h-put-add!’.

For no nesting-awareness and no side-effects, use ‘h-put-add’."
  (let* ((keys   (-butlast   keys-value))
         (value  (-last-item keys-value))
         (curval (apply #'ht-get* table keys))
         (newval (xht--put-add curval value)))
    (apply #'h-put*! table (-snoc keys newval))))

(defun h-put-add* (table &rest keys-value)
  "Return a table that is TABLE with VALUE added to KEYS' current.
KEYS-VALUE are KEYS and VALUE.

While ‘h-put*’ replaces any current value with the newly provided
value, ‘h-put-add*’ adds to it.

For more information, please see ‘h-put-add!’.

If any of the keys in the sequence KEYS can't be found,
they will be created on-the-fly.

This is a side-effect-free function.
Its destructive counterpart is ‘h-put-add*!’.

This function is nesting-aware.
Its also-side-effect-free but only-first-level counterpart is ‘h-put-add’.

For no nesting-awareness but with side-effects, use ‘h-put-add!’."
  (declare (pure t) (side-effect-free t))
  (let* ((htbl   (h-clone* table))
         (keys   (-butlast   keys-value))
         (value  (-last-item keys-value))
         (curval (apply #'ht-get* table keys))
         (newval (xht--put-add curval value)))
    (apply #'h-put* htbl (-snoc keys newval))))

(defun xht--put-add (curval value)
  "Try to add VALUE to CURVAL depending on types.
Helper to ‘h-put-add’ functions, which see."
  (declare (pure t) (side-effect-free t))
  (let ((good    (list :ht  :lol  :alist  :plist
                       :cons-pair :orgtbl
                       :json  :tsv  :csv  :ssv))
        (no-good (list :vector :list
                       :kvl    :lines
                       :empty  :null)))
    (cond
     ((ht?     curval) (let ((type (h-type value)))
                         (cond ((memq type    good) (h-mix curval
                                                           (h<-it value)))
                               ((memq type no-good) curval)
                               ;; What else could it be?
                               ;; Well, we'll keep curval.
                               (t                   curval))))
     ((null    curval) value)
     ((vectorp curval) (->> (append curval nil)  (cons value)  vconcat))
     ((listp   curval) (cons value curval))
     (t                (list value curval)))))

;;;;;;; Removal

(xht--describe
  "Functions that remove a key–value pair from tables, either destructively
or side-effects-free. Included are those that take a sequence of keys — for
nested hash tables.")

;;;;;;;; rem

(xht--describe "Summary:
|-------------------------+-----------------+-------------|
|                         | Non-destructive | Destructive |
|-------------------------+-----------------+-------------|
| simple, doesn't recurse | ‘h-rem’         | ‘h-rem!’    |
| nesting-aware, recurses | ‘h-rem*’        | ‘h-rem*!’   |
|-------------------------+-----------------+-------------|")

(define-inline h-rem! (table key)
  "Remove KEY from hash table TABLE.
This is EXACTLY equivalent to current ‘ht-remove!’.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-rem’.

This function is not nesting-aware.
Its also-destructive but nesting-aware counterpart is ‘h-rem*!’.

For nesting-awareness with no side-effects, use ‘h-rem*’."
  (inline-quote
   (remhash ,key ,table)))

(defun h-rem (table key)
  "Return a table that is TABLE with KEY removed.
This is a side-effect-free function.
Its destructive counterpart is ‘h-rem!’.

This function is not nesting-aware.
Its also-side-effect-free but nesting-aware counterpart is ‘h-rem*’.

For nesting-awareness with side-effects, use ‘h-rem*!’."
  (declare (pure t) (side-effect-free t))
  (let ((htbl (h-clone* table)))
    (h-rem! htbl key)
    htbl))

(defun h-rem*! (table &rest keys)
  "Remove key at end of KEYS sequence from hash table TABLE.

Each key in KEYS has as value the next, all of which hash tables,
except for the final key, which could return any value.

The last will be removed, together with the value associated with it.

If any of the keys in the sequence KEYS can't be found,
no action is taken.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-rem*’.

This function is nesting-aware.
Its also-destructive but only-first-level counterpart is ‘h-rem!’.

For no nesting-awareness and no side-effects, use ‘h-rem’."
  (let* ((keys-b (-butlast   keys))
         (key-v  (-last-item keys))
         (key-k  (with-demoted-errors
                     (apply #'ht-get* table keys-b))))
    ;; If last key in KEYS is a key of the hash table found by ht-getting* the
    ;; penultimate key, so remove it; otherwise it doesn't exist, so it can't
    ;; be removed
    (and (ht?           key-k)
         (ht-contains?  key-k key-v) ;; (memq key-v (ht-keys key-k))
         (remhash key-v key-k))))

(defun h-rem* (table &rest keys)
  "Return a table that is TABLE with key at end of KEYS removed.

Each key in KEYS has as value the next, all of which hash tables,
except for the final key, which could return any value.

The last will be removed, together with the value associated with it.

If any of the keys in the sequence KEYS can't be found,
no action is taken.

This is a side-effect-free function.
Its destructive counterpart is ‘h-rem*!’.

This function is nesting-aware.
Its also-side-effect-free but only-first-level counterpart is ‘h-rem’.

For no nesting-awareness but with side-effects, use ‘h-rem!’."
  (declare (pure t) (side-effect-free t))
  (let* ((htbl   (h-clone* table))
         (keys-b (-butlast   keys))
         (key-v  (-last-item keys))
         (key-k  (with-demoted-errors
                     (apply #'ht-get* htbl keys-b))))
    ;; If last key in KEYS is a key of the hash table found by ht-getting* the
    ;; penultimate key, so remove it; otherwise it doesn't exist, so it can't
    ;; be removed, so return the table as is:
    (and (ht? key-k)
         (memq key-v (ht-keys key-k))
         (remhash key-v key-k))
    htbl))

;;;;;;; Selection

(xht--describe
  "Functions that create a hash table that has only the specified keys.")

;;;;;;;; sel-keys

(xht--describe "Summary:
|-------------------------+-----------------+---------------|
|                         | Non-destructive | Destructive   |
|-------------------------+-----------------+---------------|
| simple, doesn't recurse | ‘h-sel-keys’    | ‘h-sel-keys!’ |
|-------------------------+-----------------+---------------|")

(defun h-sel-keys (table keys)
  "Return a copy of TABLE with only the pairs specified by KEYS.
KEYS is a list.

This function is similar to current ‘ht-select-keys’. The main
difference is that the copy's size is equal to TABLE's instead of
the default 65. All properties are preserved.

This is a side-effect-free function.
Its destructive counterpart is ‘h-sel-keys!’."
  (declare (pure t) (side-effect-free t))
  (let ((result (h-clr table)))
    (dolist (key keys result)
      (unless (equal 'key-not-found
                     (gethash key table 'key-not-found))
        (puthash key (gethash key table) result)))))

(defun h-sel-keys! (table keys)
  "Update TABLE to contain only the pairs specified by KEYS.
KEYS is a list.

This is a destructive function.
Its side-effect-free counterpart is ‘h-sel-keys’."
  (maphash
   (lambda (key _value)
     (unless (member key keys)
       (remhash key table)))
   table))

;;;;;;;; sel

(xht--describe
  "Guideline for the 'sel family' of functions:

- 'sel' means 'select: a nil in the result of FUN excludes KEY
               from the results.'

- an added '-' means 'anaphoric: enter a form, not a lambda'

- an added '*' means 'recurse: apply the values-function or -form
                      to the values whenever these are hash tables'

- an added '!' means 'destructive: modify TABLE instead of
                      creating a fresh one'

In the anaphoric versions, if you don't use any or either of the
let-bound variables key and value in any or either of the forms,
it's ok — no warnings.

Summary:

Side-effect-free: don't modify original TABLE, return a fresh table
|-------------------------+-------------+---------------------|
|                         | a lambda    | a form (anaphoric)  |
|-------------------------+-------------+---------------------|
| simple, doesn't recurse | ‘h-sel’     | ‘h--sel’            |
| nesting-aware, recurses | ‘h-sel*’    | ‘h--sel*’           |
|-------------------------+-------------+---------------------|

Destructive: modify original TABLE, return nil
|-------------------------+-------------+---------------------|
|                         | a lambda    | a form (anaphoric)  |
|-------------------------+-------------+---------------------|
| simple, doesn't recurse | ‘h-sel!’    | ‘h--sel!’           |
| nesting-aware, recurses | ‘h-sel*!’   | ‘h--sel*!’          |
|-------------------------+-------------+---------------------|")

(defmacro h-sel (fun table)
  "Return a table with pairs from TABLE for which FUN returns non-nil.
Function FUN is called with two arguments, key and value.

This function is similar to current ‘ht-select’."
  `(h--hmap (when (funcall ,fun key value) key)
            value ,table))

(defmacro h--sel (form table)
  "Anaphoric version of ‘h-sel’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h--hmap (when ,form key)
            value ,table))

(defmacro h-sel! (fun table)
  "Update TABLE by keeping only pairs for which FUN returns non-nil.
Function FUN is called with two arguments, key and value."
  `(h--hmap! (when (funcall ,fun key value) key)
             value ,table))

(defmacro h--sel! (form table)
  "Anaphoric version of ‘h-sel!’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h--hmap! (when ,form key)
             value ,table))

(defun h-sel* (fun table)
  "Return a table with pairs from TABLE for which FUN returns non-nil.
Function FUN is called with two arguments, key and value.

Just like ‘h-sel’, but recurses wherever TABLE is nested.

This means that for any VALUE that is itself a hash table, instead
of returning it as it is when FUNCTION returns non-nil, as ‘h-sel’
would do, VALUE will be the very ‘h-sel*’ recursively applied to
that hash table with the same FUN."
  (declare (pure t) (side-effect-free t))
  (let ((result (h-clr table)))
    (maphash
     (lambda (key value)
       (when (funcall fun key value)
         (h-put! result key (if (ht? value)
                                (h-sel* fun value)
                              ;; ^ recurses
                              value))))
     table)
    result))

(defmacro h--sel* (form table)
  "Anaphoric version of ‘h-sel*’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h-sel* (lambda (key value) (ignore key value) ,form)
           ,table))

(defun h-sel*! (fun table)
  "Update TABLE by keeping only pairs for which FUN returns non-nil.
Function FUN is called with two arguments, key and value."
  (maphash
   (lambda (key value)
     (if (funcall fun key value)
         (when (ht? value)
           (h-put! table key
                   ;; recurses:
                   (h-sel* fun value)))
       (h-rem! table key)))
   table))

(defmacro h--sel*! (form table)
  "Anaphoric version of ‘h-sel*!’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h-sel*! (lambda (key value) (ignore key value) ,form)
            ,table))

;;;;;;; Rejection

(xht--describe
  "Functions that create a hash table that has all but the specified keys.")

;;;;;;;; rej-keys

(xht--describe "Summary:
|-------------------------+-----------------+---------------|
|                         | Non-destructive | Destructive   |
|-------------------------+-----------------+---------------|
| simple, doesn't recurse | ‘h-rej-keys’    | ‘h-rej-keys!’ |
|-------------------------+-----------------+---------------|")

(defun h-rej-keys (table keys)
  "Return a copy of TABLE with all but the pairs specified by KEYS.
KEYS is a list.

This is a side-effect-free function.
Its destructive counterpart is ‘h-rej-keys!’."
  (declare (pure t) (side-effect-free t))
  (let ((result (h-clr table)))
    (maphash
     (lambda (key _value)
       (unless (member key keys)
         (puthash key (gethash key table) result)))
     table)
    result))

(defun h-rej-keys! (table keys)
  "Update TABLE by removing the pairs specified by KEYS.
KEYS is a list.

This is a destructive function.
Its side-effect-free counterpart is ‘h-rej-keys’."
  (dolist (key keys)
    (remhash key table)))

;;;;;;;; rej

(xht--describe
  "Guideline for the 'rej family' of functions:

- 'rej' means 'reject: non-nil in the result of FUN excludes KEY
               from the results.'

- an added '-' means 'anaphoric: enter a form, not a lambda'

- an added '*' means 'recurse: apply the values-function or -form
                      to the values whenever these are hash tables'

- an added '!' means 'destructive: modify TABLE instead of
                      creating a fresh one'

In the anaphoric versions, if you don't use any or either of the
let-bound variables key and value in any or either of the forms,
it's ok — no warnings.

Summary:

Side-effect-free: don't modify original TABLE, return a fresh table
|-------------------------+-------------+---------------------|
|                         | a lambda    | a form (anaphoric)  |
|-------------------------+-------------+---------------------|
| simple, doesn't recurse | ‘h-rej’     | ‘h--rej’            |
| nesting-aware, recurses | ‘h-rej*’    | ‘h--rej*’           |
|-------------------------+-------------+---------------------|

Destructive: modify original TABLE, return nil
|-------------------------+-------------+---------------------|
|                         | a lambda    | a form (anaphoric)  |
|-------------------------+-------------+---------------------|
| simple, doesn't recurse | ‘h-rej!’    | ‘h--rej!’           |
| nesting-aware, recurses | ‘h-rej*!’   | ‘h--rej*!’          |
|-------------------------+-------------+---------------------|")

(defmacro h-rej (fun table)
  "Return a table with pairs from TABLE for which FUN returns nil.
Function FUN is called with two arguments, key and value.

This function is similar to current ‘ht-reject’."
  `(h--hmap (unless (funcall ,fun key value) key)
            value ,table))

(defmacro h--rej (form table)
  "Anaphoric version of ‘h-rej’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h--hmap (unless ,form key)
            value ,table))

(defmacro h-rej! (fun table)
  "Update TABLE by keeping only pairs for which FUN returns nil.
Function FUN is called with two arguments, key and value.

This function is similar to current ‘ht-reject!’."
  `(h--hmap! (unless (funcall ,fun key value) key)
             value ,table))

(defmacro h--rej! (form table)
  "Anaphoric version of ‘h-rej!’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h--hmap! (unless ,form key)
             value ,table))

(defun h-rej* (fun table)
  "Return a table with pairs from TABLE for which FUN returns nil.
Function FUN is called with two arguments, key and value.

Just like ‘h-rej’, but recurses wherever TABLE is nested.

This means that for any VALUE that is itself a hash table, instead
of returning it as it is when FUN returns nil, as ‘h-rej’ would do,
VALUE will be the very ‘h-rej*’ recursively applied to that hash
table with the same FUN."
  (declare (pure t) (side-effect-free t))
  (let ((result (h-clr table)))
    (maphash
     (lambda (key value)
       (unless (funcall fun key value)
         (h-put! result key (if (ht? value)
                                (h-rej* fun value)
                              ;; ^ recurses
                              value))))
     table)
    result))

(defmacro h--rej* (form table)
  "Anaphoric version of ‘h-rej*’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h-rej* (lambda (key value) (ignore key value) ,form)
           value ,table))

(defun h-rej*! (fun table)
  "Update TABLE by keeping only pairs for which FUN returns nil.
Function FUN is called with two arguments, key and value."
  (maphash
   (lambda (key value)
     (if (funcall fun key value)
         (h-rem! table key)
       (when (ht? value)
         (h-put! table key
                 ;; recurses:
                 (h-rej* fun value)))))
   table))

(defmacro h--rej*! (form table)
  "Anaphoric version of ‘h-rej*!’.
For every key–value pair in TABLE, evaluate FORM with the variables
key and value bound."
  `(h-rej*! (lambda (key value) (ignore key value) ,form)
            value ,table))

;;;;;;; Reverse order

(xht--describe
  "Functions to reverse the order in which the items are stored in
the table.

Summary:
|-------------------------+-----------------|---------------+
|                         | non-destructive | destructive   |
|-------------------------+-----------------|---------------+
| simple, doesn't recurse | ‘h-reverse’     | ‘h-reverse!’  |
| nesting-aware, recurses | ‘h-reverse*’    | ‘h-reverse*!’ |
|-------------------------+-----------------|---------------+")

(defun h-reverse (table)
  "Create a hash table identical to TABLE with key order reversed."
  (let ((r (h-clone* table)))
    (xht--reverse*! r nil)
    r))

(defun h-reverse! (table)
  "Reverse the key order of hash table TABLE."
  (xht--reverse*! table nil))

(defun h-reverse* (table)
  "Create a hash table identical to TABLE with key order reversed.
When TABLE is nested, do the same to all values that are hash
tables."
  (let ((r (h-clone* table)))
    (xht--reverse*! r 'recurse)
    r))

(defun h-reverse*! (table)
  "Reverse the key order of hash table TABLE.
When TABLE is nested, do the same to all values that are hash
tables."
  (xht--reverse*! table 'recurse))

(defun xht--reverse*! (table recurse)
  "Reverse the key order of hash table TABLE.
If RECURSE is non-nil, recurse.

Helper function for:
‘h-reverse!’, ‘h-reverse’, ‘h-reverse*!’, and ‘h-reverse*’."
  (let ((items (h-vitems table 'reverse))
        (size  (h-size table))
        (idx   0))
    (h-clr! table)
    (while (< idx size)
      (-let [(key value) (aref items idx)]
        (and recurse
             (h? value)
             (xht--reverse*! value recurse))
        (h-put! table key value)
        (setq idx (1+ idx))))))

;;;;;;; Change numerical value, including #'1+ and #'1-

(xht--describe
  "Functions to alter values of keys when these values are numeric or
number-like.

Summary:
|               | Non-destructive   | Destructive        |
|---------------+-------------------+--------------------|
| Generic       | ‘h-put-num-with*’ | ‘h-put-num-with*!’ |
| Increase by 1 | ‘h-put-inc*’      | ‘h-put-inc*!’      |
| Decrease by 1 | ‘h-put-dec*’      | ‘h-put-dec*!’      |")

(defun h-put-num-with* (fun table &rest keys)
  "Return copy of TABLE with FUN applied to numeric value of KEYS.
KEYS is a chain of keys to access the key of an internal hash table
if TABLE is nested — just as with ‘h-get*’. If it's a simple,
non-nested TABLE, or if the value is at the 'first level', enter
just one key.

FUN is a unary function that can be applied to numerical values.
These are some examples: ‘1+’, ‘1-’, ‘sqrt’, ‘log10’.

You may also create partial application ones, such as:

#+begin_src emacs-lisp
  (-partial '+ 10)
  (-partial '* 2)
#+end_src

or:

#+begin_src emacs-lisp
  (lambda (x) (expt x 2))
#+end_src

The value must, of course, be a number — but the function is
flexible about what a number is. If it quacks like a number, we
increase it. So it may be found stored also as a string or keyword,
and it will be increased while maintaining the type. Moreover, it
tries to preserve original leading zeros when they were there in
strings, keywords, or symbols.

So \"041\", receiving #'1+, becomes \"042\", and so on.

When value is nil or key is not found, interpret it as being 0.

The function uses base 10."
  (declare (pure t) (side-effect-free t))
  (let ((htbl (h-clone* table)))
    (apply #'h-put-num-with*! fun htbl keys)
    htbl))

(defun h-put-num-with*! (fun table &rest keys)
  "Apply FUN to the numeric value of KEYS in possibly nested hash TABLE.

This function returns nil."
  (apply #'h-put*! table
         (-snoc keys (xht--number-with
                      fun
                      (apply #'h-get* table keys)))))

(defun h-put-inc* (table &rest keys)
  "Return a copy of TABLE with 1 added to the value of KEYS.
KEYS is a chain of keys to access the key of an internal hash table
if TABLE is nested — just as with ‘h-get*’. If it's a simple,
non-nested TABLE, or if the value is at the 'first level', enter
just one key.

The value must, of course, be a number — but the function is
flexible about what a number is. If it quacks like a number, we
increase it. So it may be found stored also as a string or keyword,
and it will be increased while maintaining the type. Moreover, it
tries to preserve original leading zeros when they were there in
strings, keywords, or symbols.

So \"041\" becomes \"042\", and so on.

When value is nil or key is not found, interpret it as being 0.

The function uses base 10."
  (declare (pure t) (side-effect-free t))
  (apply #'h-put-num-with* #'1+ table keys))

(defun h-put-inc*! (table &rest keys)
  "Add 1 to the value of KEYS in possibly nested hash TABLE.
See ‘h-put-inc*’ for more information.

This function returns nil."
  (apply #'h-put-num-with*! #'1+ table keys))

(defun h-put-dec* (table &rest keys)
  "Return a copy of TABLE with 1 subtracted from the value of KEYS.
KEYS is a chain of keys to access the key of an internal hash table
if TABLE is nested — just as with ‘h-get*’. If it's a simple,
non-nested TABLE, or if the value is at the 'first level', enter
just one key.

The value must, of course, be a number — but the function is
flexible about what a number is. If it quacks like a number, we
decrease it. So it may be found stored also as a string or keyword,
and it will be increased while maintaining the type. Moreover, it
tries to preserve original leading zeros when they were there in
strings, keywords, or symbols.

So \"043\" becomes \"042\", and so on.

The function uses base 10."
  (declare (pure t) (side-effect-free t))
  (apply #'h-put-num-with* #'1- table keys))

(defun h-put-dec*! (table &rest keys)
  "Subtract 1 from the value of KEYS in possibly nested hash TABLE.
See ‘h-put-dec*’ for more information.

This function returns nil."
  (apply #'h-put-num-with*! #'1- table keys))

;;;;;;;; helper

(defun xht--number-with (fun n)
  "Apply function FUN to number-like creature N.
Whatever quacks like a number should accept it.

Return it in the same type it was found, and preserving original
leading zeros when they were there in strings, keywords, or
symbols.

Interpret nil as 0.

Helper function to ‘xht--number-with-1+’, ‘xht--number-with-1-’."
  (declare (pure t) (side-effect-free t))
  (let* ((err-msg "Doesn't look like a number — nothing done")
         (as-num-maybe (cond
                        ((null     n)                             0)
                        ((numberp  n)                             n)
                        ((stringp  n) (read                       n))
                        ((keywordp n) (read (xht--keyword->string n)))
                        ((symbolp  n) (read (format "%s"          n)))
                        (t (user-error err-msg)))))
    (unless (numberp as-num-maybe)
      (user-error err-msg))
    (let ((res (funcall fun as-num-maybe)))
      (if (or (numberp n) (null n))
          res
        (let* ((num-was (->> n (format "%s") (s-chop-prefix ":")))
               (num-fmt
                (cond
                 ;; Floats: to avoid spurious loss of precision after
                 ;; a series of applications, we won't try to truncate
                 ;; the numbers to the significant digits of the
                 ;; original — you round it when you needed it.
                 ((floatp   res) "%s")
                 ;; Integers: if no leading 0s in the original, we'll
                 ;; just return the results; otherwise we'll check the
                 ;; original's width and set it as a minimum. Note
                 ;; that if the original was stored as integers, no
                 ;; leading 0 would have been possible — this is for
                 ;; strings or keywords etc.
                 ((integerp res) (if (/= ?0 (aref num-was 0))
                                     "%s"
                                   (format "%%0%sd"
                                           (->> num-was  length
                                                (format "%s")))))
                 (t (error "‘xht--number-with’: %s"
                           "neither integer nor float?!")))))
          (--> (format num-fmt res)
               (cond ((stringp  n) it)
                     ((keywordp n) (xht--string->keyword it))
                     ((symbolp  n) (make-symbol it)))))))))

(defsubst xht--number-with-1+ (n)
  "Apply #'1+ to number-like creature N.
Helper function to ‘h-put-inc*’ and ‘h-put-inc*!’."
  (xht--number-with #'1+ n))

(defsubst xht--number-with-1- (n)
  "Apply #'1- to number-like creature N.
Helper function to ‘h-put-dec*’ and ‘h-put-dec*!’."
  (xht--number-with #'1- n))

;;;;;;; Miscellaneous key, value, pair lookups

(xht--describe
  "Other functions about checking keys, values, and pairs.")

;;;;;;;; Has this key, this value, or this pair at some level?

(define-inline h-has-key? (table key)
  "Return t if TABLE has KEY.
This function is just like current ‘ht-contains?’"
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (let ((nf (make-symbol "xht--not-found")))
     (not (eq nf (gethash ,key ,table nf))))))

(defun h-has-key*? (table key)
  "Whether possibly-nested TABLE has KEY at some level.
Similar to ‘h-has-key?’, but if TABLE is nested look for KEY also
in the internal hash tables of TABLE."
  (declare (pure t) (side-effect-free t))
  (if (h-has-key? table key)
      t           ;; recurse:
    (xht--has-x*? #'h-has-key*?
                  table key)))

(define-inline h-has-value? (table value)
  "Return t if TABLE has some key whose value is VALUE."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (when (h-first (lambda (_k v)
                    (equal v ,value))
                  ,table)
     t)))

(defun h-has-value*? (table value)
  "Whether possibly-nested TABLE has at some level key–VALUE pair.
Similar to ‘h-has-value?’, but if TABLE is nested look for VALUE
also in the internal hash tables of TABLE."
  (declare (pure t) (side-effect-free t))
  (if (h-has-value? table value)
      t           ;; recurse:
    (xht--has-x*? #'h-has-value*?
                  table value)))

(define-inline h-has-pair? (table key value)
  "Return t if TABLE has KEY and its value is VALUE."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (equal ,value
          (gethash ,key ,table
                   (make-symbol "xht--not-found")))))

(defun h-has-pair*? (table key value)
  "Whether possibly-nested TABLE has at some level KEY–VALUE pair.
Similar to ‘h-has-pair?’, but if TABLE is nested look for KEY–VALUE
pair also in the internal hash tables of TABLE."
  (declare (pure t) (side-effect-free t))
  (if (h-has-pair? table key value)
      t           ;; recurse:
    (xht--has-x*? #'h-has-pair*?
                  table key value)))

(defun xht--has-x*? (fun table &rest x)
  "Generic helper for ‘h-has-key*?’, ‘h-has-value*?’, ‘h-has-pair*?’.
FUN is one of these functions above.
Whether possibly-nested TABLE has at some level X."
  (declare (pure t) (side-effect-free t))
  (when-let ((ht-vals (-filter #'hash-table-p
                               (h-lvalues table 'reverse))))
    (let (found) ;; 'reverse is actually cheaper ^
      (while (and ht-vals (not found))
        (setq found (apply fun (pop ht-vals) x)))
      found)))

;;;;;;;; Has all these keys, these values, or these pairs?

(defun h-has-keys? (table keys)
  "Return t if TABLE has all KEYS.
KEYS is either a single key or a list of keys."
  (declare (pure t) (side-effect-free t))
  (--all? (h-has-key? table it) (-list keys)))

(defun h-has-values? (table values)
  "Return t if TABLE has all VALUES.
VALUES is either a single value or a list of values."
  (declare (pure t) (side-effect-free t))
  (--all? (h-has-value? table it) (-list values)))

(defun h-has-pairs? (table pairs)
  "Return t if TABLE has all PAIRS.
PAIRS is either a single pair (a two-item list) or a list of such
two-item lists."
  (declare (pure t) (side-effect-free t))
  (--all? (h-has-pair? table (car it) (cadr it))
          (if (-all? #'listp pairs)
              pairs
            (if (= 2 (length pairs))
                (list pairs)
              (user-error "Malformed list of pairs")))))

;; I currently see no need for writing (and testing) nestedness-aware versions
;; of these previous three. In any case, it seems to me they could be
;; constructed in the exact same way, just adding the ?*.

;;;;;; Multi-table operations

(xht--describe
  "Functions that act on two or more hash tables simultaneously.

XHT loves short function names.

Sometimes, however, their meaning might not be obvious at first
sight. Thankfully, this is easy to fix — and by third sight they'll
already be familiar to you.

With this table, it'll take you but a minute to figure out the
meaning of the multi-table functions you'll see next:

| Function | Implying...                | Operation    |
|----------+----------------------------+--------------|
| h-mix    | Mixing the tables together | Union        |
| h-dif    | Difference between tables  | Difference   |
| h-cmn    | Commonality between tables | Intersection |

Easy?

Let's see them.")

;;;;;;; Union

(xht--describe
  "Functions that combine tables, either destructively or side-effects-free.

Note that:

- An '*' means it works with nesting; its absence means only first level.

- A  '!' means destructive: it changes the original table; its absence
  means that a new table is created, free of side-effects to the original.

- When input has multiple tables, the first one is 'updated' with the
  others, which are processed from left to right. When not destructive,
  a shallow copy of the first is updated.

  So you can always think of it as: values of table1 updated with those of
  table2; the result updated with those of table3; the result updated with
  those of table4; and so on. The difference is that when there's a '!'
  table1 will be itself modified; otherwise not.

- Functions for side-effect ('!') return nil; otherwise, the (1st) table.")

;;;;;;;; mix

(xht--describe
  "The destructive ‘h-mix!’ is similar to ht library's ‘ht-update!’, whereas
the side-effect-free ‘h-mix’ is similar to ‘ht-merge’.")

(defun h-mix! (table &rest from-tables)
  "Update TABLE according to every key–value pair in FROM-TABLES.

FROM-TABLES are processed from left to right, and
only the first table will be modified — not the others.

So (h-mix! table1 table2 table3 table4) will:
- modify table1 with the data from table2; then
- modify table1 with the data from table3; then
- modify table1 with the data from table4.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-mix’.

This function only works at 'the first level' — no nesting.
Its also-destructive nesting-aware counterpart is ‘h-mix*!’.

For nesting-awareness but no side-effects, use ‘h-mix*’."
  (dolist (from-table from-tables)
    (xht--mix! table from-table)))

(defun h-mix (table &rest from-tables)
  "Return a table that has all key–value pairs from the tables.
The tables (TABLE plus FROM-TABLES) are processed from left to
right, and no table is modified.

So (h-mix table1 table2 table3 table4) will:
- create a clone of table1 (let's call it 'table0')
- modify table0 with the data from table2; then
- modify table0 with the data from table3; then
- modify table0 with the data from table4;
- return table0.

Tables 1 to 4 haven't changed.

This is a side-effect-free function.
Its destructive counterpart is ‘h-mix!’.

This function is not nesting-aware.
Its also-side-effect-free but nesting-aware counterpart is ‘h-mix*’.

For nesting-awareness with side-effects, use ‘h-mix*!’."
  (declare (pure t) (side-effect-free t))
  (let ((mixed (h-clone* table)))
    (dolist (from-table from-tables mixed)
      (h-mix! mixed from-table))))

(defun h-mix*! (table &rest from-tables)
  "Update TABLE by adding every key–value pair in FROM-TABLES.
Unlike ‘h-mix!’ and ‘ht-update!’, it updates nested levels:
so if both values are themselves hash tables, the new value
will be the ‘h-mix*!’ of them.

FROM-TABLES are processed from left to right, and
only the first table will be modified — not the others.

So (h-mix! table1 table2 table3 table4) will:
- modify table1 with the data from table2; then
- modify table1 with the data from table3; then
- modify table1 with the data from table4.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-mix*’.

This function is nesting-aware.
Its also-destructive but only-first-level counterpart is ‘h-mix!’.

For no nesting-awareness and no side-effects, use ‘h-mix’."
  (dolist (from-table from-tables)
    (xht--mix*! table from-table)))

(defun h-mix* (table &rest from-tables)
  "Return a table that has all key–value pairs from the tables.
Unlike ‘h-mix’ and ‘ht-merge’, it updates nested levels:
so if both values are themselves hash tables, the new value
will be the ‘h-mix*’ of them.

The tables (TABLE plus FROM-TABLES) are processed from left to
right, and no table is modified.

So (h-mix* table1 table2 table3 table4) will:
- create a clone of table1 (let's call it 'table0')
- modify table0 with the data from table2; then
- modify table0 with the data from table3; then
- modify table0 with the data from table4;
- return table0.

Tables 1 to 4 haven't changed.

This is a side-effect-free function.
Its destructive counterpart is ‘h-mix*!’.

This function is nesting-aware.

Its also-side-effect-free but only-first-level counterpart is ‘h-mix’.

For no nesting-awareness but with side-effects, use ‘h-mix!’."
  (declare (pure t) (side-effect-free t))
  (let ((mixed (h-clone* table)))
    (dolist (from-table from-tables mixed)
      (h-mix*! mixed from-table))))

;;;;;;;;; helper
(defun xht--mix! (table from-table)
  "Update TABLE by adding every key–value pair in FROM-TABLE.
\(Destructive, no nesting, only two tables.)

This function returns nil.

This is EXACTLY equivalent to current ‘ht-update!’. It's used as basis
for ‘h-mix!’, ‘h-mix’, ‘h-mix*!’, and ‘h-mix*’, which see."
  (ignore
   (maphash (lambda (key value)
              (puthash key value table))
            from-table)))

(defun xht--mix*! (table from-table)
  "Update TABLE by adding every key–value pair in FROM-TABLE.
\(Destructive, accepts nesting, only two tables.)

This function returns nil.

Helper function for ‘h-mix*!’, which see."
  (ignore
   (maphash (lambda (key value)
              (puthash key
                       (let ((v-dest (gethash key table)))
                         (if (and (ht? v-dest)
                                  (ht? value))
                             (progn (xht--mix*! v-dest value)
                                    ;; ^ recursive
                                    v-dest)
                           value))
                       table))
            from-table)))

;;;;;;; Difference

(xht--describe
  "Functions that subtract tables, either destructively or side-effects-free.")

;;;;;;;; dif

(defun h-dif! (table &rest from-tables)
  "Update TABLE by removing all matching key–value pairs in FROM-TABLES.

That is: the removal only happens when both the key and the value
in one of the tables in FROM-TABLES matches that found in TABLE.
If only the key is matched, there's no removal.

FROM-TABLES are processed from left to right, and
only the first table will be modified — not the others.

So (h-dif! table1 table2 table3 table4) will:
- modify table1 with the data from table2; then
- modify table1 with the data from table3; then
- modify table1 with the data from table4.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-dif’.

This function only works at 'the first level' — no nesting.
Its also-destructive nesting-aware counterpart is ‘h-dif*!’.

For nesting-awareness but no side-effects, use ‘h-dif*’."
  (dolist (from-table from-tables)
    (xht--dif! table from-table)))

(defun h-dif (table &rest from-tables)
  "Return a table of all key–value pairs in TABLE not in FROM-TABLES.

That is: all pairs in TABLE appear in the results, except those that also
appear in one of the tables in FROM-TABLES. If only the key is matched,
there's no removal.

FROM-TABLES are processed from left to right, and no table is modified.

So (h-dif table1 table2 table3 table4) will:
- create a clone of table1 (let's call it 'table0')
- modify table0 with the data from table2; then
- modify table0 with the data from table3; then
- modify table0 with the data from table4;
- return table0.

Tables 1 to 4 haven't changed.

This is a side-effect-free function.
Its destructive counterpart is ‘h-dif!’.

This function is not nesting-aware.
Its also-side-effect-free but nesting-aware counterpart is ‘h-dif*’.

For nesting-awareness with side-effects, use ‘h-dif*!’."
  (declare (pure t) (side-effect-free t))
  (let ((diffed (h-clone* table)))
    (dolist (from-table from-tables diffed)
      (h-dif! diffed from-table))))

(defun h-dif*! (table &rest from-tables)
  "Update TABLE by removing all matching key–value pairs in FROM-TABLES.

That is: the removal only happens when both the key and the value
in one of the tables in FROM-TABLES matches that found in TABLE.
If only the key is matched, there's no removal.

Unlike ‘h-dif!’, it updates nested levels:
so if both values are themselves hash tables, the new value
will be the ‘h-dif*!’ of them.

FROM-TABLES are processed from left to right, and
only the first table will be modified — not the others.

So (h-dif! table1 table2 table3 table4) will:
- modify table1 with the data from table2; then
- modify table1 with the data from table3; then
- modify table1 with the data from table4.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-dif*’.

This function is nesting-aware.
Its also-destructive but only-first-level counterpart is ‘h-dif!’.

For no nesting-awareness and no side-effects, use ‘h-dif’."
  (dolist (from-table from-tables)
    (xht--dif*! table from-table)))

(defun h-dif* (table &rest from-tables)
  "Return a table of all key–value pairs in TABLE not in FROM-TABLES.

That is: all pairs in TABLE appear in the results, except those that also
appear in one of the tables in FROM-TABLES. If only the key is matched,
there's no removal.

Unlike ‘h-dif’, it updates nested levels: so if both values are
themselves hash tables, the new value will be the ‘h-dif*’ of them.

FROM-TABLES are processed from left to right, and no table is modified.

So (h-dif* table1 table2 table3 table4) will:
- create a clone of table1 (let's call it 'table0')
- modify table0 with the data from table2; then
- modify table0 with the data from table3; then
- modify table0 with the data from table4;
- return table0.

Tables 1 to 4 haven't changed.

This is a side-effect-free function.
Its destructive counterpart is ‘h-dif*!’.

This function is nesting-aware.
Its also-side-effect-free but only-first-level counterpart is ‘h-dif’.

For no nesting-awareness but with side-effects, use ‘h-dif!’."
  (declare (pure t) (side-effect-free t))
  (let ((diffed (h-clone* table)))
    (dolist (from-table from-tables diffed)
      (h-dif*! diffed from-table))))

;;;;;;;;; helper
(defun xht--dif! (table from-table)
  "Update TABLE by removing every key–value pair in FROM-TABLE.
\(Destructive, no nesting, only two tables.)

This function returns nil.

It's used as basis for ‘h-dif!’, ‘h-dif’, ‘h-dif*!’,
and ‘h-dif*’, which see."
  (ignore
   (maphash (lambda (key value)
              (when (h-it= (gethash key table) value)
                (remhash key table)))
            from-table)))

(defun xht--dif*! (table from-table)
  "Update TABLE by removing every key–value pair in FROM-TABLE.
\(Destructive, accepts nesting, only two tables.)

This function returns nil.

Helper function for ‘h-dif*!’, which see."
  (ignore
   (maphash (lambda (key value)
              (let* ((v-dest (gethash key table)))
                (if (h-it= v-dest value)
                    (remhash key table)
                  (and (ht? v-dest)
                       (ht? value)
                       (puthash key
                                ;; recursive:
                                (progn (xht--dif*! v-dest value)
                                       v-dest)
                                table)))))
            from-table)))

;;;;;;; Intersection

(xht--describe
  "Functions that build a table with the pairs common to all tables, either
destructively or side-effects-free.")

;;;;;;;; cmn

(xht--describe
  "cmn as in: 'common' to all tables.")

(defun h-cmn! (table &rest from-tables)
  "Update TABLE by keeping only the intersection with FROM-TABLES.

That is: the key is kept only when the pair is found in all TABLES.
If only the key is matched, not kept.

FROM-TABLES are processed from left to right, and
only the first table will be modified — not the others.

So (h-cmn! table1 table2 table3 table4) will:
- modify table1 with the data from table2; then
- modify table1 with the data from table3; then
- modify table1 with the data from table4.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-cmn’.

This function only works at 'the first level' — no nesting.
Its also-destructive nesting-aware counterpart is ‘h-cmn*!’.

For nesting-awareness but no side-effects, use ‘h-cmn*’."
  (dolist (from-table from-tables)
    (xht--cmn! table from-table)))

(defun h-cmn (table &rest from-tables)
  "Return a table of all key–value pairs in TABLE also in FROM-TABLES.

That is: no pairs in TABLE appear in the results, except those that
appear in all the tables in FROM-TABLES. If only the key is
matched, there's no removal.

FROM-TABLES are processed from left to right, and no table is modified.

So (h-cmn table1 table2 table3 table4) will:
- create a clone of table1 (let's call it 'table0')
- modify table0 with the data from table2; then
- modify table0 with the data from table3; then
- modify table0 with the data from table4;
- return table0.

Tables 1 to 4 haven't changed.

This is a side-effect-free function.
Its destructive counterpart is ‘h-cmn!’.

This function is not nesting-aware.
Its also-side-effect-free but nesting-aware counterpart is ‘h-cmn*’.

For nesting-awareness with side-effects, use ‘h-cmn*!’."
  (declare (pure t) (side-effect-free t))
  (let ((intersection (h-clone* table)))
    (dolist (from-table from-tables intersection)
      (h-cmn! intersection from-table))))

(defun h-cmn*! (table &rest from-tables)
  "Update TABLE by keeping only the intersection with FROM-TABLES.

That is: the key is kept only when the pair is found in all TABLES.
If only the key is matched, not kept.

Unlike ‘h-cmn!’, it updates nested levels:
so if both values are themselves hash tables, the new value
will be the ‘h-cmn*!’ of them.

FROM-TABLES are processed from left to right, and
only the first table will be modified — not the others.

So (h-cmn! table1 table2 table3 table4) will:
- modify table1 with the data from table2; then
- modify table1 with the data from table3; then
- modify table1 with the data from table4.

This function returns nil.

This is a destructive function.
Its side-effect-free counterpart is ‘h-cmn*’.

This function is nesting-aware.
Its also-destructive but only-first-level counterpart is ‘h-cmn!’.

For no nesting-awareness and no side-effects, use ‘h-cmn’."
  (dolist (from-table from-tables)
    (xht--cmn*! table from-table)))

(defun h-cmn* (table &rest from-tables)
  "Return a table of all key–value pairs in TABLE also in FROM-TABLES.

That is: no pairs in TABLE appear in the results, except those that
appear in all the tables in FROM-TABLES. If only the key is
matched, there's no removal.

Unlike ‘h-cmn’, it updates nested levels: so if both values are
themselves hash tables, the new value will be the ‘h-cmn*’ of them.

FROM-TABLES are processed from left to right, and no table is modified.

So (h-cmn* table1 table2 table3 table4) will:
- create a clone of table1 (let's call it 'table0')
- modify table0 with the data from table2; then
- modify table0 with the data from table3; then
- modify table0 with the data from table4;
- return table0.

Tables 1 to 4 haven't changed.

This is a side-effect-free function.
Its destructive counterpart is ‘h-cmn*!’.

This function is nesting-aware.
Its also-side-effect-free but only-first-level counterpart is ‘h-cmn’.

For no nesting-awareness but with side-effects, use ‘h-cmn!’."
  (declare (pure t) (side-effect-free t))
  (let ((intersection (h-clone* table)))
    (dolist (from-table from-tables intersection)
      (h-cmn*! intersection from-table))))

;;;;;;;;; helper
(defun xht--cmn! (table from-table)
  "Update TABLE by keeping only the intersection with FROM-TABLE.
\(Destructive, no nesting, only two tables.)

This function returns nil.

It's used as basis for ‘h-cmn!’, ‘h-cmn’, ‘h-cmn*!’, and ‘h-cmn*’,
which see."
  ;; Based on the following mathematical equality:
  ;;
  ;; A ∩ B = A - (A - B)
  ;; (h-dif! table
  ;;           (h-dif table
  ;;                  from-table)))
  ;;
  ;; ^ The above is fine, but the below
  ;;   should be faster and replaced it:
  (ignore
   (let ((sentinel (make-symbol "xht--sentinel")))
     (maphash (lambda (key value)
                (let ((v-from (gethash key from-table sentinel)))
                  (when (or (eq v-from sentinel)
                            (not (h-it= value v-from)))
                    (remhash key table))))
              table))))

(defun xht--cmn*! (table from-table)
  "Update TABLE by keeping only the intersection with FROM-TABLE.
\(Destructive, accepts nesting, only two tables.)

This function returns nil.

Helper function for ‘h-cmn*!’, which see."
  (ignore
   (let ((sentinel (make-symbol "xht--sentinel")))
     (maphash (lambda (key value)
                (let ((v-from (gethash key from-table sentinel)))
                  (if (eq v-from sentinel)
                      (remhash key table)
                    (unless (h-it= value v-from)
                      (if (and (ht? v-from)
                               (ht? value))
                          (puthash key
                                   ;; recursive:
                                   (progn (xht--cmn*! value v-from)
                                          value)
                                   table)
                        (remhash key table))))))
              table))))

;;;;;;; Comparison / Equality / Equivalence

;;;;;;;; 1D: implicit    (Vector, List, Lines) (indices as keys)

(xht--describe
  "Not needed.

Vectors, lists, and strings can be compared with ‘equal’.
Examples:

#+begin_src emacs-lisp
  (equal '(a x) '(a x))      ;=> t
  (equal [a x] [a x])        ;=> t
  (equal \"a\\nx\\n\" \"a\\nx\\n\")  ;=> t
#+end_src

And they can only be equal to exactly themselves.

Also note that we'll keep these two (one of which with extra
trailing newline) not being equal:

#+begin_src emacs-lisp
  (equal \"a\\nx\\n\" \"a\\nx\")    ;=> nil
#+end_src")

;;;;;;;; 1D: explicit    (KVL, Cons Cell)

(xht--describe
  "Functions that compare if two or more objects are equal.

Key–value pairs are of dimension 1: not nested.

No need for a function to compare cons cells: equal will do:

#+begin_src emacs-lisp
  (equal '(a . 1) '(a . 1)) ;=> t
#+end_src")

(defun h-kvl= (kvl1 kvl2 &rest more-kvls)
  "Return t only if all KEY–VALUE LINES have same KEY–VALUE pairs.
It accepts any number ≥2 of KEY–VALUE LINES as input."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-kvl
         kvl1 kvl2 more-kvls))

;;;;;;;; ≥1D unspecified (Hash table)          (possibly nested)

(xht--describe
  "Functions that compare if two or more hash tables are equivalent.
Key–value pairs are of unspecified dimension >1 (nested).

Many distinctions here, so a summary of what the operators mean
follows. These tests return t if:

| Operator | Condition for returning a truthy value                    |
|----------+-----------------------------------------------------------|
| ‘h==’    | tables have same keys in THE SAME order, and the values:  |
|          | - when both are hash tables, are also ‘h==’.              |
|          | - when both are of some different type, they are ‘equal’. |
|----------+-----------------------------------------------------------|
| ‘h-pr==’ | same as the previous, but stricter: properties*           |
|          | also match: same size, test function, etc.                |
|----------+-----------------------------------------------------------|
| ‘h=’     | tables have same keys in ANY order, and the values:       |
|          | - when both are hash tables, are also ‘h=’.               |
|          | - when both are of some different type, they are ‘equal’. |
|----------+-----------------------------------------------------------|
| ‘h-pr=’  | same as the previous, but stricter: properties*           |
|          | also match: same size, test function, etc.                |
|----------+-----------------------------------------------------------|
| ‘h_=’    | tables have same keys in ANY order, and the values:       |
|          | - are ‘equal’, OR:                                        |
|          | -- when both are hash tables, are also ‘h_=’.             |
|          | -- when both are of some different type, they return t    |
|          | when tested with their respective equality function;      |
|          | so, for example, if they're both alists, comparison       |
|          | of them using ‘h-alist=’ returns t; when both TSVs.       |
|          | comparison of them using ‘h-tsv=’ returns t; etc.         |
|----------+-----------------------------------------------------------|
| ‘h~=’    | tables have same keys in ANY order, and the values:       |
|          | - are ‘equal’, OR:                                        |
|          | -- when both are hash tables, are also ‘h~=’.             |
|          | -- when both are of some different type, they             |
|          | return t when tested with ‘h-it~’, which in               |
|          | turn means that they can even be of different             |
|          | types but, when converted to a hash table,                |
|          | become ‘h~=’.                                             |

So there's a gradation of strictness of equivalence, where:
- ‘h-pr==’ > ‘h==’ > ‘h=’ > ‘h_=’ > ‘h~=’.
and:
- ‘h-pr=’  > ‘h=’

And so ‘h~=’ is the most permissive, where equivalences of each
item are themselves deeply tested.

The ‘h==’ one may be useful, for example, for deciding whether
two hash tables would return the exact same results when mapping or
converting.

We could say that ‘h=’ is to ‘h-pr=’ as ‘equal’ is to
‘equal-including-properties’.

When in doubt, ‘h=’ is likely the best choice, because:

- It accepts different ordering of keys inside the tables. Order is
  not supposed to matter in hash tables, so this is most likely
  what you want: being stricter with ‘h==’ or any of the -pr
  would deem unequal tables that for most regular purposes are
  equivalent.

- When nested, it applies the same logic to any hash tables stored
  in it as values, ignoring order: suffices that their keys are the
  same and that the values are either ‘equal’ or ‘h=’.

- Any non–hash-table values are tested with ‘equal’, so you don't
  run the risk of false equivalence of lists that were not intended
  as plists (and therefore sensitive to ordering and to repeated
  items), and other such oddities.")

(defun h-pr== (table1 table2 &rest more-tables)
  "One of the ways to see if all hash tables TABLES are equivalent.

| Compare properties?                 | YES     |
| Compare keys order?                 | YES     |
| Compare non–hash-table values with… | #'equal |

When the values are hash tables, they are compared with
this very function, recursively.

For more details, see the helper function ‘xht--equal?’, as well as
the section heading description in the code source.

See all: ‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’."
  (apply #'xht--equal-many #'xht---pr== #'identity
         table1 table2 more-tables))

(defun h-pr= (table1 table2 &rest more-tables)
  "One of the ways to see if all hash tables TABLES are equivalent.

| Compare properties?                 | YES     |
| Compare keys order?                 | NO      |
| Compare non–hash-table values with… | #'equal |

When the values are hash tables, they are compared with this very
function, recursively.

For more details, see the helper function ‘xht--equal?’, as well as
the section heading description in the code source.

See all: ‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’."
  (apply #'xht--equal-many #'xht---pr= #'identity
         table1 table2 more-tables))

(defun h== (table1 table2 &rest more-tables)
  "One of the ways to see if all hash tables TABLES are equivalent.

| Compare properties?                 | NO      |
| Compare keys order?                 | YES     |
| Compare non–hash-table values with… | #'equal |

When the values are hash tables, they are compared with
this very function, recursively.

For more details, see the helper function ‘xht--equal?’, as well as
the section heading description in the code source.

See all: ‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’."
  (apply #'xht--equal-many #'xht--== #'identity
         table1 table2 more-tables))

(defun h= (table1 table2 &rest more-tables)
  "One of the ways to see if all hash tables TABLES are equivalent.

| Compare properties?                 | NO      |
| Compare keys order?                 | NO      |
| Compare non–hash-table values with… | #'equal |

When the values are hash tables, they are compared with this very
function, recursively.

When in doubt, use this one.

This function can be thought of as a variadic version of
‘ht-equal?’: it accepts any number ≥2 of tables as input.

Moreover, it returns t when comparing two indistinguishable nested
hash tables (of same key–value pairs), for which ‘ht-equal?’ until
v2.4 returns nil:

#+begin_src emacs-lisp
  (ht-equal? (ht (:a 1))
             (ht (:a 1)))           ;=> t

  (ht-equal? (ht (:a (ht (:b 2))))
             (ht (:a (ht (:b 2))))) ;=> nil !!
#+end_src

For more details, see the helper function ‘xht--equal?’, as well as
the section heading description in the code source.

See all: ‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’."
  (apply #'xht--equal-many #'xht--= #'identity
         table1 table2 more-tables))

(defun h_= (table1 table2 &rest more-tables)
  "One of the ways to see if all hash tables TABLES are equivalent.

| Compare properties?                 | NO      |
| Compare keys order?                 | NO      |
| Compare non–hash-table values with… | #'h-it= |

When the values are hash tables, they are compared with this very
function, recursively.

For more details, see the helper function ‘xht--equal?’, as well as
the section heading description in the code source.

See all: ‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’."
  (apply #'xht--equal-many #'xht--_= #'identity
         table1 table2 more-tables))

(defun h~= (table1 table2 &rest more-tables)
  "One of the ways to see if all hash tables TABLES are equivalent.

| Compare properties?                 | NO      |
| Compare keys order?                 | NO      |
| Compare non–hash-table values with… | #'h-it~ |

When the values are hash tables, they are compared with this very
function, recursively.

For more details, see the helper function ‘xht--equal?’, as well as
the section heading description in the code source.

See all: ‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’."
  (apply #'xht--equal-many #'xht--~= #'identity
         table1 table2 more-tables))

(defun h-props= (table1 table2 &rest more-tables)
  "Whether all hash tables TABLES have the same properties.
Note: but not necessarily the same key–value pairs, just the properties.

| Compare properties? | YES |
| Compare data?       | NO  |

See also: ‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--props= #'identity
         table1 table2 more-tables))



;;;;;;;;; helper
;; Note: ‘xht--equal?’ fixes ‘ht-equal?’ returning nil for two nested HTs.
;; It also doesn't return error when either arg isn't a hash table —
;; returns nil instead. I think this is fine: ‘equal’ itself returns
;; nil when different types: (equal '(:a 1) [:a 1]) ;=> nil

(defun xht--equal? (table1 table2 &optional strictness)
  "Return t if TABLE1 and TABLE2 have the same keys and values.
Also works with nested hash tables.

This is a helper function to build these:
‘h-pr==’, ‘h-pr=’, ‘h==’, ‘h=’, ‘h_=’, ‘h~=’.

\(Note: when in doubt, ‘h=’ is probably what you want.)

When STRICTNESS is nil, default to '=.

When STRICTNESS is the symbol:
- '-pr==
- test their properties with ‘xht--props=’.
- test the keys order of both.
- test non–hash-table values with ‘equal’.
- test hash-table values with (‘xht--equal?’ v1 v2 'pr==).

- '-pr=
- test their properties with ‘xht--props=’.
- test non–hash-table values with ‘equal’.
- test hash-table values with (‘xht--equal?’ v1 v2 'pr=).

- '==
- test the keys order of both.
- test non–hash-table values with ‘equal’.
- test hash-table values with (‘xht--equal?’ v1 v2 '==).

- '=
- test non–hash-table values with ‘equal’.
- test hash-table values with (‘xht--equal?’ v1 v2 '=).

- '_=
- test non–hash-table values with ‘h-it=’.
- test hash-table values with (‘xht--equal?’ v1 v2 '_=).

- '~=
- test non–hash-table values with ‘h-it~’.
- test hash-table values with (‘xht--equal?’ v1 v2 '~=).

For any chosen option, all tests must pass to return t.

Comparison of properties mean: besides the data, they must all be
of same nominal size, test function, weakness, rehash-size and
rehash-threshold.

Some comments.

So if '_= is chosen, when the values of matching keys are of same
type but aren't equal, try to convert them to hash tables and
compare again. They could be, for example, equivalent plists or
equivalent alists or equivalent JSONs or equivalent org tables:
with just the order changed or updated (repeated) keys not deleted
— but equivalent. In this case, values will be considered equal. So
when this argument is '_= any pair among the following would return
t to this function:

#+begin_src emacs-lisp
  (h* :a 1 :b '(:c 3 :d 4))
  (h* :a 1 :b '(:d 4 :c 3)))
  (h* :a 1 :b '(:d 4 :c 3 :c 1 :d 2)))
#+end_src

and the same for when 2D: org tables, lists of lists, TSVs, etc.

And if '~= is chosen, it's like the previous but allow for the
values' types to be different. So when this argument is '~= any
pair among the following would return t to this function:

#+begin_src emacs-lisp
  (h* :a 1 :b '(:c 3 :d 4))                   ; plists
  (h* :a 1 :b '(:d 4 :c 3))                   ;
  (h* :a 1 :b '(:d 4 :c 3 :c 1 :d 2))         ;
  (h* :a 1 :b '((:c . 3) (:d . 4)))           ; alists
  (h* :a 1 :b '((:d . 4) (:c . 3) (:c . 20))) ;
  (h* :a 1 :b '(h* :c 3 :d 4))                ;
  (h* :a 1 :b \":c\t3\n:d\t4\")               ; key–value lines
#+end_src

and the same for when 2D: org tables, lists of lists, TSVs, etc.

Why bother with a separate function? Isn't it always desirable to
have these equivalent things being considered equivalent?

Not necessarily. Take the case of a value that looks like this:

#+begin_src emacs-lisp
  '(a f n nil b x b nil a m w v a k)
#+end_src

Who knows what this list is supposed to mean? It may represent the
first letter of the names of cats in your building from lowest- to
highest-numbered apartments — and thus NOT intended to be a plist.

So the list above would NOT be equivalent to:

#+begin_src emacs-lisp
  '(a f n nil b x w v)
#+end_src

Doing that would have replaced the cat in apartment 107; and added
one cat to 108, whose resident is allergic and won't be pleased.

What about alists? Wouldn't it be safe to do it automatically to them?

Also not necessarily. Perhaps you want to keep track of items as they
are replaced. Maybe this:

#+begin_src emacs-lisp
  '((Alice . 10) (Bob . 5)
    (Alice . 15) (Bob . 2)
    (Alice . 0)  (Bob . 0))
#+end_src

represents how much money you owed your friends Alice and Bob at
the end of three consecutive days, which you want to keep a record
of AND be able to ‘assoc’ the alist to get the current values (10
and 5, respectively). And you could get the value of the previous
day using (nth 2 list) and (nth 3 list).

What about org tables? Same thing. Maybe the one you have stored is
not at all about some tabular data structure of unique IDs, and you
don't want to reduce it to one.

So we have separate functions, to be used according to what you'd
like to consider equivalent. This depends on the data, and also on
what you're doing."
  (declare (pure t) (side-effect-free t))
  (if strictness
      (unless (memq strictness '(-pr==  -pr=  ==  =  _=  ~=))
        (user-error "‘xht--equal?’: Unrecognized strictness option %s"
                    strictness))
    (setq strictness '=))
  (and
   (h? table1)
   (h? table2)
   (pcase strictness
     ((or '-pr== '-pr=) (xht--props= table1 table2))
     (_ t))
   (let ((keys1 (h-lkeys table1 'rev))
         (keys2 (h-lkeys table2 'rev)))
     (and
      (pcase strictness
        ((or '-pr== '==) (equal keys1 keys2))
        (_ t))
      (equal (length keys1)
             (length keys2))
      (equal (->> keys1  (--map (format "%S" it))  (-sort #'string<))
             (->> keys2  (--map (format "%S" it))  (-sort #'string<)))
      (--all? (let* ((sentinel (make-symbol "xht--sentinel"))
                     (val1     (h-get table1 it))
                     (val2     (h-get table2 it sentinel)))
                (if (and (h? val1)
                         (h? val2))
                    ;; recurse:
                    (xht--equal? val1 val2 strictness)
                  (pcase strictness
                    ('-pr== (equal    val1 val2))
                    ('-pr=  (equal    val1 val2))
                    ('==    (equal    val1 val2))
                    ('=     (equal    val1 val2))
                    ('_=    (xht--it= val1 val2))
                    ('~=    (xht--it~ val1 val2))
                    (_      (error "‘xht--equal?’: impossible option")))))
              ;; Note:
              ;; - ‘xht--it~’ calls ‘h~=’ which is...
              ;; (xht--equal? h1 h2 '~=), where h1 and h2 will be the
              ;; hash tables found in the conversion.
              ;;
              ;; - ‘xht--it=’ calls ‘h=’ which is...
              ;; (xht--equal? h1 h2 '=), where h1 and h2 will be the
              ;; hash tables found in the conversion.
              ;;
              ;; Eventually these recursions end up in two things that can
              ;; be compared with ‘equal’ instead of converted.
              ;;
              ;; I suppose ‘h_=’ would be more consistent for the second
              ;; case, but this seems more than good enough. There are more
              ;; distinctions that could be made, such as what happens when
              ;; the hash tables have an alist that has a plist that itself
              ;; has a hash table with a JSON object storing two TSVs that may
              ;; or not be equivalent — but if you find yourself staring at
              ;; that, you may be better off reviewing your data structure...
              keys1)))))

(defun xht--props= (table1 table2)
  "Whether TABLE1 and TABLE2 have the same properties.
But not necessarily the same key–value pairs.

Helper function for ‘xht--props-and-data=’ and ‘h-props=’."
  (declare (pure t) (side-effect-free t))
  (h= (h-props table1)
      (h-props table2)))

(defsubst xht---pr== (table1 table2)
  "Whether TABLE1 and TABLE2 are ‘xht--equal?’ with strictness '-pr==."
  (xht--equal?                    table1 table2                '-pr==))
(defsubst xht---pr=  (table1 table2)
  "Whether TABLE1 and TABLE2 are ‘xht--equal?’ with strictness '-pr=."
  (xht--equal?                    table1 table2                '-pr=))
(defsubst xht--==    (table1 table2)
  "Whether TABLE1 and TABLE2 are ‘xht--equal?’ with strictness '==."
  (xht--equal?                    table1 table2                '==))
(defsubst xht--=     (table1 table2)
  "Whether TABLE1 and TABLE2 are ‘xht--equal?’ with strictness '=."
  (xht--equal?                    table1 table2                '=))
(defsubst xht--_=    (table1 table2)
  "Whether TABLE1 and TABLE2 are ‘xht--equal?’ with strictness '_=."
  (xht--equal?                    table1 table2                '_=))
(defsubst xht--~=    (table1 table2)
  "Whether TABLE1 and TABLE2 are ‘xht--equal?’ with strictness '~=."
  (xht--equal?                    table1 table2                '~=))

;;;;;;;; ≥1D unspecified (Alist, Plist, JSON)  (possibly nested)

(xht--describe
  "Functions that compare if two or more alists, plists, or json objects are
equivalent. Key–value pairs are of unspecified dimension >1 (nested).")

(defun h-alist= (alist1 alist2 &rest more-alists)
  "Return t only if all ALISTs have same KEY–VALUE pairs.
It accepts any number ≥2 of ALISTs as input.

See ‘h-lol=’ for more information on equivalence, which also
applies here."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-alist*
         alist1 alist2 more-alists))

(defun h-plist= (plist1 plist2 &rest more-plists)
  "Return t only if all PLISTs have same KEY–VALUE pairs.
It accepts any number ≥2 of PLISTs as input.

See ‘h-lol=’ for more information on equivalence, which also
applies here."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-plist*
         plist1 plist2 more-plists))

(defun h-json= (json1 json2 &rest more-jsons)
  "Return t only if all JSONs have same KEY–VALUE pairs.
It accepts any number ≥2 of JSONs as input.

See ‘h-lol=’ for more information on equivalence, which also
applies here."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-json*
         json1 json2 more-jsons))

;;;;;;;; 2D              (LOL, Org table, TSV, CSV, SSV)

(xht--describe
  "Functions that compare if two or more objects are equal.
Key–value pairs are of dimension 2: tabular.")

(defun h-lol= (lol1 lol2 &rest more-lols)
  "Return t only if all LISTS OF LISTS have same KEY–VALUE pairs.
It accepts any number ≥2 of LISTS OF LISTS as input.

Note that regular lists and vectors are the same if they have the
same elements at the same indices, so they can be compared with
plain ‘equal’.

Two LISTS OF LISTS, however, may, for our purposes, be equal while
being nominally different.

For example, these two:

#+begin_src emacs-lisp
  '((:id  :name  :age)   '((:id  :name  :age)
    (1    alice  42)       (2    bob    30)
    (2    bob    30)       (1    alice  42))
    (1    alice  21))
#+end_src

As with alists and plists, the top-most element of same ID wins,
and the order between different items doesn't matter. Each unique
ID returns the same information. The same applied when we convert
from Org Table, TSV, CSV, SSV, or JSON."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-lol
         lol1 lol2 more-lols))

(defun h-orgtbl= (orgtbl1 orgtbl2 &rest more-orgtbls)
  "Return t only if all ORG TABLES have same KEY–VALUE pairs.
It accepts any number ≥2 of ORG TABLES as input.

See ‘h-lol=’ for more information on equivalence, which also
applies here."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-orgtbl
         orgtbl1 orgtbl2 more-orgtbls))

(defun h-tsv= (tsv1 tsv2 &rest more-tsvs)
  "Return t only if all TSVs have same KEY–VALUE pairs.
It accepts any number ≥2 of TSVs as input.

See ‘h-lol=’ for more information on equivalence, which also
applies here."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-tsv
         tsv1 tsv2 more-tsvs))

(defun h-csv= (csv1 csv2 &rest more-csvs)
  "Return t only if all CSVs have same KEY–VALUE pairs.
It accepts any number ≥2 of CSVs as input.

See ‘h-lol=’ for more information on equivalence, which also
applies here."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-csv
         csv1 csv2 more-csvs))

(defun h-ssv= (ssv1 ssv2 &rest more-ssvs)
  "Return t only if all SSVs have same KEY–VALUE pairs.
It accepts any number ≥2 of SSVs as input.

See ‘h-lol=’ for more information on equivalence, which also
applies here."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--= #'h<-ssv
         ssv1 ssv2 more-ssvs))



;;;;;;;; Unknown thing

(xht--describe
  "Functions that compare if two or more objects are equal.
Objects are unspecified.")

(defun h-type (obj)
  "Detect object OBJ's type-for-conversion. Return a keyword.

This isn't the same as ‘type-of’; it's more concerned with
types of interest for conversions and lookups.

So, for example, it'd return :tsv for a TSV, while ‘type-of’
would return 'string.

Testing is done in a selected order to best guess some potentially
ambiguous types by trying to match the most specific of them first.

- So LOL is tested for before alist and plist, which are also
  tested before list.

- Moreover, if, while testing, the value of ‘h-kvl-sep-re’ matches
  TAB, then ‘h-kvl?’ could return t if all lines have a single TAB.
  Since by default this variable matches \" *= *\", it'll be
  interpreted that it's being let-bound, and that the OBJ is a KVL
  rather than a two-column TSV. Likewise in case the value of
  ‘h-kvl-sep-re’ matches a comma \(preference over CSV) or simply
  two or more spaces \(preference over SSV).

  It's worth reminding that the difference between a two-column *sv
  and a KVL of matching delimiter is that the latter has no headers
  \(one-dimensional: just key and value), whereas the former has a
  header \(two-dimensional: key, field, and value)"
  (declare (pure t) (side-effect-free t))
  (let ((type (type-of obj)))
    (pcase type
      ('hash-table                     :ht)
      ('vector                         :vector)
      ('cons       (cond
                    ((-cons-pair? obj) :cons-pair)
                    ((h-lol?      obj) :lol)
                    ((h-alist?    obj) :alist)
                    ((h-plist?    obj) :plist)
                    (t                 :list)))
      ('string     (cond
                    ((string= ""  obj) :empty)
                    ((h-orgtbl?   obj) :orgtbl)
                    ((h-json?     obj) :json)
                    ((h-kvl?      obj) :kvl)
                    ((h-tsv?      obj) :tsv)
                    ((h-csv?      obj) :csv)
                    ((h-ssv?      obj) :ssv)
                    (t                 :lines)))
      ('symbol     (cond
                    ((null        obj) :null)
                    (t                 type)))
      (_                               type))))

(defun h-type= (obj1 obj2 &rest more-objs)
  "Are all OBJECTS of same type?
For example, are they all hash tables? Or all org tables?
Or all lists of lists? Or all alists? Or all TSVs? JSONs?

Return type.

This isn't the same as ‘type-of’; it's more concerned with
types of interest for this conversion and lookup.

So, for example, it'd return :tsv for a TSV, while ‘type-of’
would return 'string."
  (declare (pure t) (side-effect-free t))
  (when (apply #'xht--equal-many #'xht--type= #'identity
               obj1 obj2 more-objs)
    (h-type obj1)))

(defun h-it= (obj1 obj2 &rest more-objs)
  "Are all OBJECTS, all of same type, reducible to each other?
Try to guess what they are.

The following tests are applied, and if any of them return t for
every pair of objects, they are considered same-type equals:
  ‘equal’      ‘h-tsv=’     ‘h-json=’
  ‘h=’         ‘h-csv=’     ‘h-alist=’
  ‘h-lol=’     ‘h-ssv=’     ‘h-plist=’
  ‘h-orgtbl=’.
If so, return type. Otherwise, return nil."
  (declare (pure t) (side-effect-free t))
  (when (apply #'xht--equal-many #'xht--it= #'identity
               obj1 obj2 more-objs)
    (h-type obj1)))

(defun h-it~ (obj1 obj2 &rest more-objs)
  "Are all OBJECTS equivalent according to XHT tests?
They are if, and only if, either they are equal or, when converted
to a hash table, ‘h~=’ returns t for the pair. This means that
repeated keys (in alists, plists, TSVs etc) are dealt with, and
that, after that, ordering is irrelevant."
  (declare (pure t) (side-effect-free t))
  (apply #'xht--equal-many #'xht--it~ #'identity
         obj1 obj2 more-objs))

;;;;;;;;; helper

(defun xht--type= (obj1 obj2)
  "Are OBJ1 and OBJ2 of same type?"
  (declare (pure t) (side-effect-free t))
  (equal (h-type obj1)
         (h-type obj2)))

(defun xht--it= (obj1 obj2)
  "Are OBJ1 and OBJ2, of same type, reducible to each other?
If so, return type. Otherwise, return nil."
  (declare (pure t) (side-effect-free t))
  (let ((t1 (h-type obj1))
        (t2 (h-type obj2)))
    (and (equal t1 t2)
         (pcase t1
           (:ht         (h=         obj1  obj2))
           (:lol        (h-lol=     obj1  obj2))
           (:tsv        (h-tsv=     obj1  obj2))
           (:csv        (h-csv=     obj1  obj2))
           (:ssv        (h-ssv=     obj1  obj2))
           (:kvl        (h-kvl=     obj1  obj2))
           (:json       (h-json=    obj1  obj2))
           (:alist      (h-alist=   obj1  obj2))
           (:plist      (h-plist=   obj1  obj2))
           (:orgtbl     (h-orgtbl=  obj1  obj2))
           (:empty      t)
           (:null       t)
           (:list       (equal      obj1  obj2))
           (:lines      (equal      obj1  obj2))
           (:vector     (equal      obj1  obj2))
           (:cons-pair  (equal      obj1  obj2))
           (_           (equal      obj1  obj2)))
         t1)))

(defun xht--it~ (obj1 obj2)
  "Are OBJ1 and OBJ2 equivalents according to XHT tests?
They are if, and only if, when converted to a hash table, ‘h~=’
returns t for the pair."
  (declare (pure t) (side-effect-free t))
  (or (equal obj1 obj2)
      (let ((h1 (ignore-errors (h<-it obj1)))
            (h2 (ignore-errors (h<-it obj2))))
        (h~= h1 h2))))

;;;;;;;; Helper
(defun xht--equal-many (fn-compare fn-transform obj1 obj2 &rest more-objs)
  "Compare OBJECTS with FN-COMPARE after applying FN-TRANSFORM on them."
  (declare (pure t) (side-effect-free t))
  ;; Note: choice of obj1 and obj2 is to guarantee that at least 2 objects are
  ;; passed; and repetition below was chosen in order to avoid the unnecessary
  ;; consing that would come with the following simpler abstraction:
  ;; (--all? (funcall fn-compare
  ;;                  (funcall fn-transform obj1)
  ;;                  (funcall fn-transform it))
  ;;         (cons obj2 more-objs)))
  (and (funcall fn-compare
                (funcall fn-transform obj1)
                (funcall fn-transform obj2))
       (--all? (funcall fn-compare
                        (funcall fn-transform obj1)
                        (funcall fn-transform it))
               more-objs)))

;;;;;; Conversion

(xht--describe
  "Functions to convert from/to hash tables.

To hash tables:
‘h<-kvl’    ‘h<-lines’
‘h<-vector’ ‘h<-list’
‘h<-lol’    ‘h<-orgtbl’
‘h<-alist’  ‘h<-plist’  ‘h<-alist*’  ‘h<-plist*’  ‘h<-json*’
‘h<-tsv’    ‘h<-csv’    ‘h<-ssv’

From hash tables:
‘h->kvl’    ‘h->lines’
‘h->vector’ ‘h->list’
‘h->lol’    ‘h->orgtbl’
‘h->alist’  ‘h->plist’  ‘h->alist*’  ‘h->plist*’  ‘h->json*’
‘h->tsv’    ‘h->csv’    ‘h->ssv’

You can navigate functions easily with:
M-. and M-, (elisp-slime-nav-mode)

Note that when results on both sides evaluate to hash tables, we use
for equality comparison the ‘H=>’ symbol (which uses ‘h=’).

For getting (‘alist-get’, ‘plist-get’, ‘h-get’, etc.):
| alist, plist:        | Top-most values override bottom-most values |
| lol, org-table, *sv: | Top-most values override bottom-most values |
| kvl:                 | Bottom-most values override top-most values |
| vector, list, lines: | Uniqueness guaranteed by index              |

Their nature:
| kvl:                 | no nesting,     keys are given                     |
| lines:               | no nesting,     keys = line number (starting at 0) |
| vector, list:        | may be nested¹, keys = indices                     |
| alist, plist, json:  | may be nested², keys are given                     |
| lol, org-table, *sv: | tabular, assumed no nesting (no lols inside lols)  |

¹ treatment of nested not implemented for vectors and lists.
² treatment of nested implemented for alists, plists, and jsons —
  both ways.")

;;;;;;; Hash table to hash table

(xht--describe
  "Functions that read a hash table and return a hash table.

For shallow copying, ‘h-copy’ is offered as an alias to
‘copy-hash-table’. However, this function is not at all appropriate
for dealing with nested hash tables. See ‘h-clone*’ for more.")

(defun h-clone* (table &optional sz ts we rs rt)
  "Create a deep copy of possibly-nested hash table TABLE.
Optional arguments after TABLE are SZ, TS, WE, RS, and RT — which
correspond to, respectively, size, test, weakness, rehash-size,
rehash-threshold. When nil, they match that of TABLE.

The result has the same elements and structure of TABLE, but any
nested (internal) hash tables are recursively replaced by new ones.
This allows you to modify the internal hash tables without altering
the original one.

This is not possible with ‘copy-hash-table’, whose copy is shallow
and any internal tables that might be present in the copy will
point to the very same objects of the original one. Likewise with
‘ht-copy’, which uses ‘copy-hash-table’, and ‘h-copy’, which is an
alias to the latter.

You also shouldn't use shallow copying if you intend to apply
destructive functions such as ‘sort’, ‘nconc’, or ‘nreverse’ to any
list values of the copied table, as this will likely modify the
original. To avoid that, either use non-destructive alternatives
such as ‘-sort’ and ‘reverse’; or ‘h-clone*’ the original instead
of using ‘h-copy’.

See also: ‘h<-ht’."
  (declare (pure t) (side-effect-free t))
  (let ((result (h-empty-clone table sz ts we rs rt)))
    (maphash (lambda (key value)
               (puthash key
                        (if (h? value)
                            (h-clone* value)
                          ;; ^ recurses
                          value)
                        result))
             table)
    result))
;; ^ Could have used this:
;;     (h-hmap* (lambda (key _value) key)
;;              (lambda (_key value) value)
;;              table)
;;   but:
;;   1. h-hmap* removes nil keys (rare case, but still, we're cloning)
;;   2. chosen one is more direct, possibly faster, no need for funcalling

(defun h<-ht (table &optional sz ts we rs rt)
  "Given hash table TABLE, return it or a clone, depending on params.
Optional arguments after TABLE are SZ, TS, WE, RS, and RT — which
correspond to, respectively, size, test, weakness, rehash-size,
rehash-threshold. When nil, they match that of TABLE.

If any of the optional arguments passed differs from the TABLE's
corresponding properties, ‘h-clone*’ the table with these
parameters. Otherwise just return TABLE.

This is a non-destructive function: TABLE isn't changed.

See also: ‘h-clone*’."
  (declare (pure t) (side-effect-free t))
  (let ((rest  (list sz ts we rs rt))
        (props (-> table  h-props  h-lvalues)))
    (if (and (equal (nth 2 rest)
                    (nth 2 props))
             ;; This ^ because the below is not sufficient when passed
             ;; weakness is nil and TABLE's isn't. The other parameters, on
             ;; the other hand, cannot have nil as value in the table, so a
             ;; passed nil is just omission.
             (--all? (-let [(r . p) it]
                       (or (not r)
                           (equal r p)))
                     (-zip-with #'cons rest props)))
        table
      (h-clone* table sz ts we rs rt))))

(defun h-2d<-1d (table &optional f1 f2 size test)
  "Make 2D the 1D hash table TABLE by adding header fields F1, F2.
If F1 is nil, use the string \"key\". If F2 is nil, use \"value\".

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

Alias: ‘h-1d->2d’."
  (declare (pure t) (side-effect-free t))
  (let* ((sznow (h-size table))
         (size  (or size (xht--init-size sznow)))
         (htbl  (h-new size test)))
    (h--each table
      (h-put! htbl key
              (h* (or f1 "key")   key
                  (or f2 "value") value)))
    htbl))

(defun h-1d<-2d (table2d &optional size test)
  "Make 1D (KVL-like) the 2D hash table TABLE2D.
Infer current header with ‘h-2d-header’.
If header isn't of length 2, signal error.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’.

Alias: ‘h-2d->1d’."
  (declare (pure t) (side-effect-free t))
  (let ((header (h-2d-header table2d)))
    (unless (= 2 (length header))
      (error "Header length of hash table isn't 2"))
    (let* ((sznow (h-size table2d))
           (size  (or size (xht--init-size sznow)))
           (htbl  (h-new size test)))
      (h--each table2d
        (h-put! htbl key
                (h-get* table2d key
                        (cadr header))))
      htbl)))

;;;;;;; 1D: implicit    (indices as keys)

(xht--describe
  "Functions in this category convert from other formats to hash table or
vice-versa. Key–value pairs are of dimension 1: not nested. Indices serve as
keys.")

;;;;;;;; Vectors

(defun h<-vector (vector &optional size test)
  "Convert vector VECTOR to a hash table using VECTOR's indices as keys.

Keys are by default of integer type, not strings, and start at 0.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (let* ((sznow (length vector))
         (size  (or size (xht--init-size sznow)))
         (htbl  (h-new size test))
         (idx   0))
    (while (< idx sznow)
      (h-put! htbl idx (aref vector idx))
      (setq idx (1+ idx)))
    htbl))

(defun h->vector (table)
  "Convert hash table TABLE to vector.
Do the opposite of ‘h<-vector’, which see. Return vector.

Select only those keys that are natural numbers (an integer ≥0) or
strings that could be converted to a natural number, which it
automatically does.

Any holes in the number sequence from 0 to max(keys) are filled
in the vector with nil.

If the table has keys that reduce to the same integer, such as
having both 3 and \"3\" as keys, behavior is unpredictable."
  (declare (pure t) (side-effect-free t))
  ;; `keys` is another hash table, where each pair is (newkey key):
  ;; e.g. (3 "3")
  ;; It maps the post-natural-num keys to the original keys.
  (let* ((tkeys (h-keys table))
         (keys  (h-new (xht--init-size (length tkeys))))
         maxkey  vector  keys-keys)
    (dolist (key tkeys)
      (when-let ((keyN (xht--thing-to-nat key)))
        (h-put! keys keyN key)))
    (setq keys-keys (h-keys keys))
    (if (null keys-keys)
        []
      (setq maxkey (->> keys-keys  (apply #'max))
            vector (make-vector (1+ maxkey) nil))
      (h--each keys
        (aset vector key (h-get table value)))
      vector)))

(defun xht--thing-to-nat (obj)
  "Return OBJ as a natural number if it looks like one.
If it's a string, read it. Then check if it's a natural number.
If so, return it. Otherwise, return nil."
  (declare (pure t) (side-effect-free t))
  (if (natnump obj)
      obj
    (--> obj  h-as-string  read
         (when (natnump it) it))))

;;;;;;;; Lists

(defun h<-list (list &optional size test)
  "Convert LIST to a hash table using LIST's indices as keys.
Keys are by default of integer type, not strings, and start at 0.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (let* ((sznow (length list))
         (size  (or size (xht--init-size sznow)))
         (htbl  (h-new size test))
         (idx   0))
    (while (< idx sznow)
      (h-put! htbl idx (elt list idx))
      (setq idx (1+ idx)))
    htbl))

(defun h->list (table)
  "Convert hash table TABLE to list.
Select only those keys that are natural numbers (an integer ≥0)
or strings that could be converted to a natural number, which it
automatically does.

Any holes in the number sequence from 0 to max(keys) are filled
in the list with nil.

If the table has keys that reduce to the same integer, such as
having both 3 and \"3\" as keys, behavior is unpredictable."
  (declare (pure t) (side-effect-free t))
  (--> table
       (h->vector it)
       (append it nil)))

;;;;;;;; Lines

(defun h<-lines (str &optional size test)
  "Convert string STR to a hash table using line numbers as keys.
Keys are by default of integer type, not strings, and start at 0.

The choice of zero for the first line number (instead of 1) was due
to transitivity: a plain conversion from lines to hash table to
vector to lines should yield the initial string. The alternatives
seemed worse.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (h<-list (s-lines str) size test))

(defun h->lines (table)
  "Convert hash table TABLE whose keys are numbers to string.
Select only those keys that are natural numbers (an integer ≥0)
or strings that could be converted to a natural number, which it
automatically does.

Any holes in the number sequence from 0 to max(keys) are filled
in the string with empty lines.

If the table has keys that reduce to the same integer, such as
having both 3 and \"3\" as keys, behavior is unpredictable."
  (declare (pure t) (side-effect-free t))
  (->> table  h->list
       (s-join "\n")))

;;;;;;; 1D: explicit

(xht--describe
  "Functions in this category convert from other formats to hash table or
vice-versa. Strings with one key–value pair per line are of dimension 1: not
nested. Likewise cons pairs.")

;;;;;;;; Key–Value Lines

(defun h<-kvl (kvl &optional size test obsolete)
  "Convert key–value lines KVL to a hash table.
Optional arguments SIZE and TEST may be passed.

A KVL is the equivalent of a non-nested alist translated to a
simple flat config file as used in Unix-like systems.

It should look, for example, like this:

--- data begins --->
key1 = val1
key2 = val2
key3 = val3
<--- data ends -----

One pair per line. There is no header. If there were, it'd be taken
as another key–value line.

The field delimiter (separator) to be used is given by the value of
the regular expression in the variable ‘h-kvl-sep-re’, which see.
By default it's equal sign surrounded or not by spaces. The
variable can be temporarily let-bound when change is desirable.

Comments are non-destructively stripped before conversion. Comments
are anything matching ‘h-kvl-comment-re’ (which see), which can
also be temporarily let-bound when some other comment regex is
expected in KVL.

Note that sections (as used in .ini files and some types of .conf)
aren't implemented or dealt with here. KVLs are to be considered
flat. So lines with [Section name] are not considered as nodes, and
no tree-like structure will be generated. These section lines, if
present, are ignored by the default ‘h-kvl-section-re’ (which see)
as if they were comments, since this may be useful if the file
you're processing has sections only for information purposes and
the keys inside them are unique. If this is not the case, then this
function is not suitable for this conversion. An option would be to
pre-convert the input to JSON using some other tool, and then apply
‘h<-json*’ to the result.

Also note that in an alist, when two elements have equal key, the
first is used. With KVL data, we'll consider that the last row
wins. This is because in an alist a new pair is pushed to the top,
whereas KVL files are usually (but not always, as there are varying
standards) read from top to bottom.

An OBSOLETE form of calling this function is
  \(h<-kvl kvl &optional sep size test)
where the optional arg SEP specified the field delimiter. This use
is deprecated. The delimiter is now passed by the variable
‘h-kvl-sep-re’, which should be let-bound when the default is not
the one expected in the KVL to be converted."
  (declare (advertised-calling-convention (kvl &optional size test) "2.0")
           (pure t) (side-effect-free t))
  (let (delim-re strip-re lines htbl)
    ;; Backward compatibility with obsolete calling convention:
    (when (or (stringp size) (natnump test) obsolete)
      (setq delim-re size
            size     test
            test     obsolete))
    (setq strip-re (format "%s\\|%s" h-kvl-comment-re h-kvl-section-re)
          delim-re (or delim-re h-kvl-sep-re)
          lines    (s-lines kvl)
          size     (or size (xht--init-size (length lines)))
          htbl     (h-new size test))
    (while lines
      (--> (pop lines)
           (s-replace-regexp strip-re "" it)
           (unless (s-blank-str? it)
             (-let [(k v) (s-split delim-re (s-trim it))]
               (h-put! htbl k v)))))
    htbl))

(defun h->kvl (table &optional sep)
  "Convert hash table TABLE to SEP-separated key–value lines.
The optional separator SEP is a literal string, and could be, for
example, \" = \" or \":\" or \"\t\". When nil, default to \"=\"."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string
    (h--each table
      (princ (format "%s%s%s\n"
                     key (or sep "=") value)))))

;;;;;;;; Cons Pairs

(defun h<-cons-pair (cons-pair &optional size test)
  "Convert CONS-PAIR to hash table.
SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (unless (-cons-pair? cons-pair)
    (error "%s is not a cons pair" cons-pair))
  (-let [(key . value) cons-pair]
    (h-st* (or size (xht--init-size 1)) test
           key value)))

(defun h->cons-pair (table)
  "Convert hash table TABLE to cons pair."
  (declare (pure t) (side-effect-free t))
  (pcase (h-size table)
    (1 (car (h--lmap `(,key  . ,value) table)))
    (_ (error "Can only convert to cons pair hash tables of size 1"))))


;;;;;;; ≥1D unspecific  (possibly nested)

(xht--describe
  "Functions in this category convert from other formats to hash table or
vice-versa. Key–value pairs are of undetermined dimension >1 (nested).")

;;;;;;;; Association lists
;;;;;;;;; from alist

(defun xht<--alist* (alist &optional size test recurse)
  "Convert ALIST to hash table, maybe recursing.
Helper function, basis for ‘h<-alist’ and ‘h<-alist*’.

Optional arguments SIZE and TEST may be passed.
If RECURSE is non-nil, recurse."
  (declare (pure t) (side-effect-free t))
  (let ((htbl (h-new (or size
                         (-> alist  length  xht--init-size))
                     test)))
    (dolist (pair alist htbl)
      (-let [(key . value) pair]
        (when (eq 'xht:new-one (h-get htbl key 'xht:new-one))
          ;; ^ Fastest solution I can think of to both preserve the original
          ;; ordering and exclude eventual repeats. The cost of this extra
          ;; h-get instead of processing it in reverse is ~1 μs, and I'm fine
          ;; with an extra 1 ms for processing some (rare) 1000-item list if
          ;; it preserves the list's order.
          (h-put! htbl key (if (and recurse
                                    (h-alist? value))
                               (xht<--alist* value nil test recurse)
                             ;; sublevel sizes auto^ ; recursive^
                             value)))))))

(defun h<-alist (alist &optional size test)
  "Convert ALIST to hash table.
Similar to ‘ht<-alist’, except that it preserves original order and
SIZE can also be passed as an argument. When SIZE is nil, it
defaults to the result of applying ‘xht--init-size’ to half the
plist's length.

For the meaning of TEST, see ‘h-new’.

Like ‘ht<-alist’, this function is not aware of nested alists. Its
nesting-aware counterpart is ‘h<-alist*’."
  (declare (pure t) (side-effect-free t))
  (xht<--alist* alist size test nil))

(defun h<-alist* (alist &optional size test)
  "Convert possibly nested ALIST to hash table.
If the ALIST is nested, each alist found as value will be
recursively converted into a hash table as well.

SIZE is the nominal initial size of the table to be created.
When SIZE is nil, it defaults to the result of applying
‘xht--init-size’ to the alist's length.

For the meaning of TEST, see ‘h-new’.

This function is aware of nested alists. Its non–nesting-aware
counterpart is ‘h<-alist’."
  (declare (pure t) (side-effect-free t))
  (xht<--alist* alist size test 'recurse))

;;;;;;;;; to alist

(defun xht-->alist* (table &optional recurse)
  "Convert hash table TABLE to alist, maybe recursing.
Helper function, basis for ‘h->alist’ and ‘h->alist*’.

Optional arguments SIZE and TEST may be passed.
If RECURSE is non-nil, recurse."
  (declare (pure t) (side-effect-free t))
  (h--lmap  (cons key
                  (if (and recurse
                           (ht? value))
                      (xht-->alist* value recurse)
                    ;; recursive^
                    value))
            table))

(defun h->alist (table)
  "Convert hash table TABLE to alist.
Similar to current ‘ht->alist’, with the difference that it
preserves the hash table's order of keys.

Like ‘ht->alist’, this function is not aware of nested alists. Its
nesting-aware counterpart is ‘h->alist*’."
  (declare (pure t) (side-effect-free t))
  (xht-->alist* table nil))

(defun h->alist* (table)
  "Convert hash table TABLE to alist, maybe recursing.
If the ALIST is nested, each alist found as value will be
recursively converted into a hash table as well.

This function is aware of nested alists. Its non–nesting-aware
counterpart is ‘h->alist’."
  (declare (pure t) (side-effect-free t))
  (xht-->alist* table 'recurse))

;;;;;;;; Property lists
;;;;;;;;; from plist

(defun xht<--plist* (plist &optional size test recurse)
  "Convert PLIST to hash table, maybe recursing.
Helper function, basis for ‘h<-plist’ and ‘h<-plist*’.

Optional arguments SIZE and TEST may be passed.
If RECURSE is non-nil, recurse."
  (declare (pure t) (side-effect-free t))
  (let* ((lenpl (length plist))
         (htbl  (h-new (or size
                           (-> lenpl  (/ 2)  xht--init-size))
                       test))
         (even  (if (= 0 (% lenpl 2))
                    (-copy plist)
                  (-butlast plist))))
    (while even
      (let ((key   (pop even))
            (value (pop even)))
        (when (eq 'xht:new-one (h-get htbl key 'xht:new-one))
          ;; ^ Fastest solution I can think of to both preserve the original
          ;; ordering and exclude eventual repeats. The cost of this extra
          ;; h-get instead of processing it in reverse is ~1 μs, and I'm fine
          ;; with an extra 1 ms for processing some (rare) 1000-item list if
          ;; it preserves the list's order.
          (h-put! htbl key (if (and recurse
                                    (h-plist? value))
                               (xht<--plist* value nil test recurse)
                             ;; sublevel sizes auto^ ; recursive^
                             value)))))
    htbl))

(defun h<-plist (plist &optional size test)
  "Convert PLIST to hash table.
Similar to ‘ht<-plist’, except that it preserves original order and
SIZE can also be passed as an argument. When SIZE is nil, it
defaults to the result of applying ‘xht--init-size’ to half the
plist's length.

For the meaning of TEST, see ‘h-new’.

This function is not aware of nested plists. Its nesting-aware
counterpart is ‘h<-plist*’."
  (declare (pure t) (side-effect-free t))
  (xht<--plist* plist size test nil))

(defun h<-plist* (plist &optional size test)
  "Convert possibly nested PLIST to hash table.
If the PLIST is nested, each plist found as value will be
recursively converted into a hash table as well.

SIZE is the nominal initial size of the table to be created.
When SIZE is nil, it defaults to the result of applying
‘xht--init-size’ to half the plist's length.

For the meaning of TEST, see ‘h-new’.

This function is aware of nested plists. Its non–nesting-aware
counterpart is ‘h<-plist’."
  (declare (pure t) (side-effect-free t))
  (xht<--plist* plist size test 'recurse))

;;;;;;;;; to plist

(defun xht-->plist* (table &optional recurse)
  "Convert hash table TABLE to plist, maybe recursing.
Helper function, basis for ‘h->plist’ and ‘h->plist*’.

If RECURSE is non-nil, recurse."
  (declare (pure t) (side-effect-free t))
  (apply #'append
         (h--lmap (list key
                        (if (and recurse
                                 (ht? value))
                            (xht-->plist* value recurse)
                          ;; recursive^
                          value))
                  table)))

(defun h->plist (table)
  "Convert hash table TABLE to plist.
Similar to current ‘ht->plist’, with the difference that it
preserves the hash table's order of keys.

This function is not aware of nested plists. Its nesting-aware
counterpart is ‘h->plist*’."
  (declare (pure t) (side-effect-free t))
  (xht-->plist* table nil))

(defun h->plist* (table)
  "Convert hash table TABLE to plist, maybe recursing.
If the PLIST is nested, each plist found as value will be
recursively converted into a hash table as well.

This function is aware of nested plists. Its non–nesting-aware
counterpart is ‘h->plist’."
  (declare (pure t) (side-effect-free t))
  (xht-->plist* table 'recurse))

;;;;;;;; JSON

(defun h<-json* (json &optional size test)
  "Convert possibly nested JSON to hash table.

If JSON is a string enclosed in [] instead of {}, the first step of
the conversion will output a vector instead of directly a hash
table. The vector will be inspected, and:

- If every item is a hash table, guess from it the ID for creating
  a 2D hash table, and return such table.

- Otherwise, treat it as vector and convert to hash table as usual:
  indices as numeric keys, items as values.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (let* ((json-object-type 'hash-table)
         (converted (json-read-from-string json)))
    (cond
     ((ht? converted)
      (h<-ht converted size (or test 'equal)))
     ((vectorp converted)
      (let ((values (append converted nil)))
        (if (-all? #'ht? values)
            (let* ((headers-lol (-map    #'h-keys        values))
                   (common      (-reduce #'-intersection headers-lol)))
              (unless common
                (error "h<-json*: dim < 2D? Don't know how to convert"))
              (let ((id   (car common))
                    (htbl (h-new size test)))
                (--each values
                  (h-put! htbl (h-get it id) it))
                htbl))
          (h<-vector converted)))))))

(defun h->json* (table &optional no-pp sort)
  "Convert hash table TABLE to JSON, maybe recursing.
If NO-PP is nil, return formatted (pretty-printed string).
If NO-PP is non-nil, return as is (compact string).

If SORT is non-nil, return it sorted. Otherwise don't sort it."
  (declare (pure t) (side-effect-free t))
  ;; It seems JSON needs keys to be strings or symbols.
  ;; No numbers as keys, for example.
  (let ((json-encoding-object-sort-predicate (when sort 'string<))
        (json-encoding-pretty-print (not no-pp))
        (htbl (h-new (h-size table))))
    ;; Problem: ‘json-encode-hash-table’ reverses the key order of the hash
    ;; table, and of its values (also hash tables) — but we'd rather preserve
    ;; the order when sort is nil (it won't matter if we choose to sort it, of
    ;; course). So I thought it was time to write some functions to reverse
    ;; the key order of a hash table...
    (h--each table
      (h-put! htbl (h-as-string key) value))
    (json-encode-hash-table (if sort
                                htbl
                              (h-reverse* htbl)))))

;;;;;;; 2D              (tabular)

(xht--describe
  "Functions in this category convert from other formats to hash table or
vice-versa. Key–value pairs are of dimension 2: tabular.")

;;;;;;;; Lisp tables (lists of lists)

(defun h<-lol (lol &optional size test)
  "Create a hash table with initial values according to list of lists LOL.

- The first element of the LOL is a list of keys.

- The other elements of the LOL are their respective values.

- The values of the first column of the LOL are used as unique IDs:
  keys to the first hash table.

- The value of each unique ID is a hash table composed of all
  key–value pairs associated with this unique ID.

As with alists, in case of repeated unique IDs the one uppermost in
the list has preference for returning values.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (let* ((sentinel (make-symbol "xht--sentinel"))
         (size     (or size (xht--init-size (length lol))))
         (htbl     (h-new size test))
         (header   (car lol))
         key)
    (dolist (row (cdr lol) htbl)
      (setq key (car row))
      (when (eq sentinel (h-get htbl key sentinel))
        ;; ^ Fastest solution I can think of to both preserve the original
        ;; ordering and exclude eventual repeats. The cost of this extra h-get
        ;; instead of processing it in reverse is ~1 μs, and I'm fine with an
        ;; extra 1 ms for processing some (rare) 1000-item list if it
        ;; preserves the list's order.
        (h-put! htbl key (h-zip-lists header row))))))

(defun h->lol (table2d &optional reverse)
  "Do the opposite of ‘h<-lol’, which see. Return list of lists.
Input is TABLE2D.

If REVERSE is non-nil, reverse the display order of all rows after
header. Notice that the internal order of items depends on where
the data came from and whether it has been updated."
  (declare (pure t) (side-effect-free t))
  (unless (h-empty? table2d)
    (let* ((header (h-2d-header table2d))
           (rest   (h-lmap (lambda (_k value)
                             (--map (ht-get value it) header))
                           table2d 'reverse))) ;<--more efficient
      (cons header (if reverse
                       rest
                     (reverse rest))))))

;;;;;;;; Org tables

(defun h<-orgtbl (orgtbl &optional size test)
  "Create a hash table with initial values according to org table ORGTBL.
Does to an org table what ‘h<-lol’ does to Lisp tables (lists of lists).

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (autoload #'org-table-to-lisp "org-table")
  (let ((lol (->> orgtbl
                  org-table-to-lisp
                  (-remove-item 'hline))))
    (h<-lol lol size test)))

(defun h->orgtbl (table2d &optional reverse)
  "Do the opposite of ‘h<-orgtbl’, which see. Return org table as string.
Input is TABLE2D.

If REVERSE is non-nil, reverse the display order of all rows after
header. Notice that the internal order of items depends on where
the data came from and whether it has been updated."
  (declare (pure t) (side-effect-free t))
  (if (ht-empty? table2d)
      ""
    (with-temp-buffer
      (let* ((standard-output (current-buffer))
             (header (h-2d-header table2d))
             (rest   (h-lmap (lambda (_k value)
                               (--map (ht-get value it) header))
                             table2d 'reverse))) ;<-- more efficient
        (princ (xht--list-to-orgtbl-row header))
        (princ "|----------|\n")
        (dolist (item (if reverse
                          rest
                        (reverse rest)))
          ;; That's right ^ because if it came from a lol or
          ;; org table or tsv, it was originally read from
          ;; bottom to top, so it's already internally reversed.
          (princ (xht--list-to-orgtbl-row item))))
      ;; Without org it will also work — but the org table
      ;; output, although valid, will remain unformatted.
      (ignore-errors (org-mode))
      (when (equal major-mode 'org-mode)
        (goto-char 3)
        (call-interactively #'org-ctrl-c-ctrl-c))
      (s-trim (xht--buff-str-no-prop)))))

(defun xht--list-to-orgtbl-row (list)
  "Convert a flat LIST to an org table's row."
  (declare (pure t) (side-effect-free t))
  (->> (-map #'h-as-string list)
       (s-join " | ")
       (format "| %s |\n")))


;;;;;;;; TSV, CSV, SSV
;;;;;;;;; Without using org (preferred)

(defun h<-tsv (tsv &optional size test)
  "Create a hash table with initial values according to TSV.
SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (let (lol)
    (--> (s-lines tsv)
         (dolist (row it (nreverse lol))
           (unless (s-blank-str? row)
             (push (s-split "\t" row) lol)))
         (h<-lol it size test))))
;; This ^ should be faster and more direct than using org, and without limits:
;; ‘org-table-convert-region’ uses ‘org-table-convert-region-max-lines’,
;; normally set to 999.

(defun h->tsv (table2d &optional reverse)
  "Do the opposite of ‘h<-tsv’, which see. Return tsv.
Input is TABLE2D.

If REVERSE is non-nil, reverse the display order of all rows after
header. Notice that the internal order of items depends on where
the data came from and whether it has been updated."
  (declare (pure t) (side-effect-free t))
  (if (h-empty? table2d)
      ""
    (with-temp-buffer
      (let* ((standard-output (current-buffer))
             (header (h-2d-header table2d))
             (rest   (h-lmap (lambda (_k v)
                               (h-values v))
                             table2d 'reverse))) ;<-- more efficient
        (princ (xht--list-to-tsv-row header))
        (dolist (item (if reverse
                          rest
                        (reverse rest)))
          ;; That's right ^ because if it came from a lol or
          ;; org table or tsv, it was originally read from
          ;; bottom to top, so it's already internally reversed.
          (princ (xht--list-to-tsv-row item))))
      (->> (xht--buff-str-no-prop)
           s-trim-right
           (format "%s\n")))))

(defun xht--list-to-tsv-row (list)
  "Convert a flat LIST to a TSV's row."
  (declare (pure t) (side-effect-free t))
  (->> (-map #'h-as-string list)
       (s-join   "\t")
       (s-append "\n")))

(defun h<-ssv (ssv &optional size test)
  "Create a hash table with initial values according to SSV.
SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (let (lol)
    (--> (s-lines ssv)
         (dolist (row it (nreverse lol))
           (unless (s-blank-str? row)
             (push (s-split "  +" row) lol)))
         (h<-lol it size test))))

(defun h->ssv (table2d &optional reverse)
  "Do the opposite of ‘h<-ssv’, which see. Return ssv.
Input is TABLE2D.

If REVERSE is non-nil, reverse the display order of all rows after
header. Notice that the internal order of items depends on where
the data came from and whether it has been updated."
  (declare (pure t) (side-effect-free t))
  (if (h-empty? table2d)
      ""
    (with-temp-buffer
      (let* ((standard-output (current-buffer))
             (header  (h-2d-header table2d))
             (rest    (h-lmap (lambda (_k v)
                                (h-values v))
                              table2d 'reverse)) ;<--more efficient
             (widths  (--map (xht--2d-col-width table2d it) header)))
        (princ (xht--list-to-ssv-row header widths))
        (dolist (item (if reverse
                          rest
                        (reverse rest)))
          ;; That's right ^ so we only reverse it once; and if it came from a
          ;; lol or org table or ssv, it was originally read from bottom to
          ;; top, so it's already internally reversed.
          (princ (xht--list-to-ssv-row item widths))))
      (->> (xht--buff-str-no-prop)
           s-trim-right
           (format "%s\n")))))

(defun xht--2d-col-width (table2d field)
  "Maximum string length of column FIELD of 2D hash table TABLE2D.
For example, in the SSV below, resulting widths are, respectively:
2, 5, 3.

id  name   age
01  alice  42
02  bob    30

So passing \"name\" as FIELD for an equivalent 2D table returns 5."
  (declare (pure t) (side-effect-free t))
  (->> (h-2d-col table2d field)
       (--map (->> it  h-as-string  length))
       -max))

(defun xht--list-to-ssv-row (list &optional widths)
  "Convert a flat LIST to an SSV's row.
If WIDTHS is nil, just use two spaces as separator.
If however a list of WIDTHS is provided, fields will be padded to
these widths before adding the two spaces."
  (declare (pure t) (side-effect-free t))
  (->> (h-zip-lists (-map #'h-as-string list)
                    (or widths (-repeat (length list) 1)))
       (h--lmap (format (format "%%-%ds"
                                value)
                        key))
       (s-join "  ")  ;join with 2 spaces
       (s-trim-right) ;no need for trailing spaces in the row
       (s-append "\n")))

;; ‘h<-csv’: aliased to ‘xht--csv’.

(defun h->csv ()
  "Do the opposite of ‘h<-csv’, which see. Return csv.
NOTE: This function has not yet been implemented."
  (message "h->csv: not yet implemented"))


;;;;;;;;; Using org

(defun xht<--*sv (input arg &optional size test)
  "Read a tsv, csv, or ssv INPUT and convert it to a hash table.
INPUT can be a file or a buffer or a string.

For the meaning of ARG, see ‘org-table-convert-region’.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (if (xht--str-nw? input)
      (with-temp-buffer
        (autoload #'org-table-convert-region "org-table")
        (insert input)
        (org-table-convert-region (point-min) (point-max) arg)
        (h<-orgtbl (xht--buff-str-no-prop) size test))
    (h-new size test)))

(defun xht<--tsv (input &optional size test)
  "Create a hash table with initial values according to TSV.
INPUT can be a file or a buffer or a string.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (xht<--*sv input '(16) size test))

(defun xht<--csv (input &optional size test)
  "Create a hash table with initial values according to CSV.
INPUT can be a file or a buffer or a string.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (xht<--*sv input '(4) size test))

(defun xht<--ssv (input &optional size test)
  "Create a hash table with initial values according to SSV.
INPUT can be a file or a buffer or a string.

SSV here means a table using 2+ spaces, or a TAB, as field
separators.

SIZE is the nominal initial size of the table to be created.
If nil, it defaults to the result of ‘xht--init-size’, which see.

For the meaning of TEST, see ‘h-new’."
  (declare (pure t) (side-effect-free t))
  (xht<--*sv input 2 size test))


;;;;;;; Unknown thing

(xht--describe
  "Functions that read an unknown object, try to guess what it is, then
convert it to a hash table.")

(defun h<-it (thing &rest rest)
  "Convert THING to hash table. Try to guess what it is.

When in doubt, it prefers higher dimension. So if type is alist,it
uses ‘h<-alist*’, not ‘h<-alist’. Likewise ‘h<-plist*’ instead of
‘h-plist’. If you prefer the latter, be explicit, passing the exact
function instead.

And since it uses h-type for type-detection, it tries to match:
- lol, alist, and plist before list — so when you prefer plain
  lists, be explicit, passing ‘h<-list’ instead.

- *sv before lines — so when you prefer plain lines, be explicit,
  passing ‘h<-lines’ instead.

REST are arguments that might be taken by the respective conversion
functions."
  (declare (pure t) (side-effect-free t))
  (let ((type (h-type thing)))
    (pcase type
      (:ht         (apply #'h<-ht         thing  rest))
      (:lol        (apply #'h<-lol        thing  rest))
      (:tsv        (apply #'h<-tsv        thing  rest))
      (:csv        (apply #'h<-csv        thing  rest))
      (:ssv        (apply #'h<-ssv        thing  rest))
      (:kvl        (apply #'h<-kvl        thing  rest))
      (:null       (apply #'h<-list       thing  rest))
      (:list       (apply #'h<-list       thing  rest))
      (:json       (apply #'h<-json*      thing  rest))
      (:alist      (apply #'h<-alist*     thing  rest))
      (:plist      (apply #'h<-plist*     thing  rest))
      (:empty      (apply #'h<-tsv        thing  rest))
      (:lines      (apply #'h<-lines      thing  rest))
      (:orgtbl     (apply #'h<-orgtbl     thing  rest))
      (:vector     (apply #'h<-vector     thing  rest))
      (:cons-pair  (apply #'h<-cons-pair  thing  rest))
      (_ (user-error "Cannot convert %s to hash table" type)))))

;;;;;; Predicates

(xht--describe
  "Miscellaneous predicates, mostly about checking type for conversion
purposes.")

;;;;;;; Type

(xht--describe
  "For hash tables, use ‘h?’, which is the same as ‘ht?’: an alias to Emacs'
internal ‘hash-table-p’.")

(defun h-kvl? (str &optional obsolete)
  "Could string STR be a KVL?
Only if, after stripped of any comments and section headers, and
trimmed of any leading and trailing whitespace, every line has
exactly one common separator. Comments and section headers are
matched with ‘h-kvl-comment-re’ and ‘h-kvl-section-re’,
respectively, which see.

An OBSOLETE form is
  \(h-kvl? str &optional sep)
where the optional arg SEP specified the field delimiter. This use
is deprecated. The delimiter is now passed by the variable
‘h-kvl-sep-re’ (which see), which should be let-bound when the
default value is not expected."
  (declare (advertised-calling-convention (str) "2.0")
           (pure t) (side-effect-free t))
  (let ((strip-re (format "%s\\|%s" h-kvl-comment-re h-kvl-section-re))
        (delim-re (or obsolete h-kvl-sep-re))
        (lines    (s-lines str))
        (kvl?     t))
    (while (and lines kvl?)
      (--> (pop lines)
           (s-replace-regexp strip-re "" it)
           s-trim
           (or (s-blank-str? it)
               (= 1 (s-count-matches delim-re it))
               (setq kvl? nil))))
    kvl?))

(defun h-alist? (list)
  "Non-nil if and only if LIST is a non-nil alist with simple keys."
  ;; Adapted from json.el's ‘json-alist-p’.
  (declare (pure t) (side-effect-free t))
  (when list
    (let ((l list))
      (while (consp l)
        (setq l (if (and (consp (car l))
                         (atom (caar l)))
                    (cdr l)
                  'not-alist)))
      (null l))))

(defun h-plist? (list)
  "Non-nil if and only if LIST is a non-nil plist with non-nil atom keys."
  (declare (pure t) (side-effect-free t))
  (when list
    (let ((l list))
      (while (consp l)
        (setq l (if (consp (cdr l))
                    (cddr l)
                  'not-plist)))
      (null l))))

(defun h-lol? (obj)
  "Return t if OBJ is a list of lists."
  (declare (pure t) (side-effect-free t))
  (and obj
       (listp obj)
       ;; we don't want it if it's only header (more likely it'd be an alist)
       (> (length obj) 1)
       ;; then all items must be lists:
       (--all? (and (listp it)
                    ;; and none of them cons cells:
                    (listp (cdr it))
                    ;; for now we're assuming exactly 2D (not higher), so no
                    ;; item of the internal lists should itself be a list;
                    ;; this distinguishes it from 2D alists — but will break
                    ;; if we ever want to make it 3D, in which case we'd need
                    ;; better detection.
                    (-all? #'nlistp it))
               obj)
       ;; for our purposes, no item's length should be larger than the
       ;; header's (but ok if they're shorter: trailing nils)
       (--> (-map #'length obj)
            (= (car it) (-max it)))))

(defun h-orgtbl? (str)
  "Could string STR be an Org Table?"
  (autoload #'org-at-table-p "org")
  (with-temp-buffer
    (insert str)
    (goto-char 2)
    (org-at-table-p)))

(defun h-json? (str)
  "Could string STR be a JSON object?
Return nil if, after trimming, it isn't enclosed in {} or [].
Return t if inside is either empty or returns non-nil when
converted 
