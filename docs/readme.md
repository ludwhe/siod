# SIOD: Scheme in One Defun

**SIOD** is a _small-footprint_ implementation of the
Scheme programming language that is provided with some database,
unix programming and cgi scripting extensions.

This document is &copy; 1996-2007 by
[George J. Carrette](http://alum.mit.edu/www/gjc/), All Rights Reserved.

The reference home page for SIOD is this web page,
and the most recent release may be found on <http://www.codeplex.com/siod>.

Click
[here for a list of SIOD users](http://alum.mit.edu/www/gjc/siodusers.html).

| File           | Format           | Description                                  |
| -------------- | ---------------- | -------------------------------------------- |
| siod.tgz       | gzip tar         | Source code, all versions                    |
| siod.zip       | INFO-ZIP archive | Source code, all versions                    |
| <./winsiod.md> | document         | Windows binaries and unpacking instructions. |

## Table of Contents

- [Apology, discussion and motivation](#apology-discussion-and-motivation)
- [Building from Sources](#building-from-sources)
- [Release Notes](#release-notes)
- [What is Scheme?](#what-is-scheme)
- [Reference Section for built-in procedures](#reference-section-for-built-in-procedures)
- [Reference Section for extension-provided procedures](#reference-section-for-extension-provided-procedures)
- [Command interfaces and some scripts provided](#command-interfaces-and-some-scripts-provided)
- [Some scheme coded library modules](#some-scheme-coded-library-modules)
- [Garbage Collection](#garbage-collection)
- [Porting](#porting) to [tiny](#porting-to-tiny-machines) machines
- [Writing extensions in the C programming language](#writing-extensions-in-the-c-programming-language)
- [LIBSIOD use as an extension language for C programs](#libsiod-use-as-an-extension-language-for-c-programs)
- [Implementation of EVAL and environment representation](#implementation-of-eval-and-environment-representation)
- [Windows NT and Windows 95 Configuration](#windows-nt-and-windows-95-configuration)
- [Unix configuration](#unix-configuration)
- [References](#references)
- [Contributors](#contributors)
- [Acknowledgements](#acknowledgements)

## Apology, discussion and motivation

The genesis of this interpreter is a question posed by
students of a _LISP and Artificial Intelligence Programming Techniques_
course that I had developed and was teaching at Boston University in the
late 1980's:

> How can we possibly hope to make use of any of this stuff in work we are
> doing in other courses or in our jobs?

The problem being that both the commercial and non-commercial lisp
offerings at the time seemed to want to take over your entire
programming environment if you wanted to use lisp at all.
A completely satisfactory solution to this problem will not be fully
available again until a lisp system is written that uses
the same compiler back-end, debugging and runtime support as
all the other languages on a popular machine. But I digress.

Knowing that the use of lisp didn't have to be so intrusive,
but having very little
practical evidence at hand to actually prove the fact, I decided
one day while I was keeping the laboratory section room
open for course **EK201** to sit down and implement a demonstration
made up of a simple **cons**,
**arithmetic**, **read**, **eval**, and **print**
in straightforward **C**
programming style, where the garbage collector was stop-and-copy and
could only run in the context of the toplevel loop. Defining and executing
**fib** resulted in a code-coverage of over 95% of the lines
of the C program. Hence, SIOD, Scheme in One Day. Borrowing the name
of a previous Scheme interpreter I had done to test the Lispmachine
microcode compiler, Scheme in One Defun.

The motivation behind SIOD remains a small footprint, in every sense
of the word, at runtime, at compile time, and in **cognitive** attention
required to understand how the system works enough to be able to
extend it as well as the author would have done the work himself.

About eight years have passed since that initial release. It has been
possible to add a feature or two without contributing to the cause
of software bloat, with the code segment of the libsiod shared
library remaining under 75K bytes on a prototypical comparison machine
like a VAX. Furthermore, as the richness of the C runtime library
available on most systems has improved over time, **SIOD** remains
a useful kind of glue to have in a software engineers toolbox.

Please forgive the lack of full compliance with IEEE or R4RS
standards. Perhaps one of these days.

## Building from Sources

See the **README** file that comes with the distribution.
The following systems are known to be supported using supplied
build files.

| Make                          | OS                   |
| ----------------------------- | -------------------- |
| Digital Equipment Corporation | DIGITAL UNIX (OSF/1) |
| Linux                         | Linux                |
| Hewlett-Packard Company       | HP-UX                |
| Sun Microsystems              | Solaris              |
| Silicon Graphics              | IRIX                 |
| Digital Equipment Corporation | OpenVMS              |
| Microsoft                     | Windows 95           |

## Release Notes

- **1.0** April 1988. Initial release.
- **1.1** April 1988. Macros, predicates, load. Better number recognizer in
  read, provided siod.scm file.
- **1.2** April 1988. Name changes as requested by JAR@AI.AI.MIT.EDU, plus some
  bug fixes.
- **1.3** May 1988. Changed env to use frames instead of alist. define now
  works properly.
- **1.4** November 1989. This release is functionally the same as release 1.3
  but has been remodularized in response to people who have been encorporating
  SIOD as an interpreted extension language in other systems.
- **1.5** November 1989. Added the -g flag to enable mark-and-sweep garbage
  collection. The default is stop-and-copy. (Note: changed default to
  mark-and-sweep)
- **2.0** December 1989. Set_Repl_Hooks, catch & throw.
- **2.1** December 1989. Additions to SIOD.SCM: Backquote, cond.
- **2.2** December 1989. User Type extension. Read-Macros. (From C-programmer
  level).
- **2.3** December 1989. save-forms. load with argument t, comment character,
  faster intern. -o flag gives obarray size. default 100.
- **2.4** April 1990. speed up arithmetic and the evaluator. fixes to siod.scm.
  no_interrupt around calls to C I/O. gen_readr.
- **2.5** September 1990. numeric arrays in siod.c
- **2.6** March 1992. remodularize .h files, procedure prototypes. gc, eval,
  print hooks now table-driven.
- **2.7** March 1992. hash tables, fasload.
- **2.8** April 1992. bug fixes.
- **2.9** August 1992. added trace.c, fseek, ftell, some fixes.
- **3.0** May 1994. Windows NT port. some cleanups. SQL support, more string
  stuff. Heap management flexibility, default to mark-and-sweep, suggestions by
  some code reviewers for comp.sources.unix.
- **3.1x** June 1995. Verbose flag to supress file loading and error messages,
  along with enhanced command-line interface made siod useful for writing
  scripts. Support for more C library and unix programming functionality,
  including regular expressions and sockets. Debugging hooks _eval-history-ptr_.
- **3.2** June 1996. shared library modularization, dynamic linking interface
  for extensions. documentation in html.h Lexical closure support at
  c-programmer level. Arithmetic cleanup. parser:XXXXX extension. command
  "compiler" interface.
- **3.4** Feb 1997. Windows NT/95 cleanup.
- **3.5** 5-MAY-97 fixes, plus win95 "compiler" to create exe files.
- **3.6** 5-APR-2007. Upload to CodePlex.Com, port to Visual C++ 2005 Express
  Edition.

## What is Scheme?

Scheme is a programming language, a dialect of the Lisp (List
Processing) family of languages, generally utilizing a syntax based on
parenthetical expressions delimited by whitespace, although
[alternative](#example-helloscm) syntax capabilities are sometimes
available. An implementation
such as SIOD usually runs by default in an immediate execution mode,
where programs are parsed as they are entered, then executed with
the results being printed. Hence this set of examples with input
syntax suitable for either Microsoft QBASIC or SIOD:

| QBASIC.EXE input           | SIOD input                         | SIOD Result   |
| -------------------------- | ---------------------------------- | ------------- |
| `PRINT (1 + 2) \* (3 + 4)` | `(\* (+ 1 2) (+ 3 4))`             | 21            |
| `PRINT "HELLO-" + "BUDDY"` | `(string-append "HELLO-" "BUDDY")` | "HELLO-BUDDY" |
| FUNCTION f(x)              | (define (f x)                      | #\<CLOSURE>   |
| IF x < 2 THEN              | (if (< x 2) x                      |               |
| f = x                      | (+ (f (- x 1)) (f (- x 2)))))      |               |
| ELSE                       |                                    |               |
| f = f(x - 1) + f(x - 2)    |                                    |               |
| END IF                     |                                    |               |
| END FUNCTION               |                                    |               |
| PRINT f(20)                | (f 20)                             | 6765          |

## Reference Section for built-in procedures

Note that the arguments to built-in procedures are always optional
and default to **()**. Many of these procedures call a C library function
of the same or similar name, in the obvious way. Therefore you can
refer to the unix manual page for more detailed information about
function behavior. Such procedures are indicated with a bold **U**.

### (%%%memref address)

This is a lowlevel routine which should not be invoked in normal code.
References a byte of memory at address. Used mostly to cause
a core dump for debugging purposes by referencing address 0 or -1.

### (%%closure env code)

This is a lowlevel routine which should not be invoked in normal code.
If code is a cons of the form (arlist . body) then env is a list of frames,
and the application of the closure will invoke the interpreter.
Otherwise code should be of type tc_subr_X and the
application of the closure will pass the env as the
first argument to the C procedure implementing the subr.

### (%%closure-code closure)

This is a lowlevel routine which should not be invoked in normal code.
Returns the code passed to %%closure.

### (%%closure-env closure)

This is a lowlevel routine which should not be invoked in normal code.
Returns the env passed to %%closure.

### (%%stack-limit amount silent)

If amount is non-null it sets the runtime stack check pointer to
allow for that number of bytes. If silent is non-null the resulting
(or current) stack size is returned, otherwise a message is printed.

### (\* x1 x2 ...)

Returns the product of all its arguments, or 1 if no arguments.

### _after-gc_

A variable, the value is an express evaluated after the gc has done its
work. For example:

```scheme
(set! *after-gc* '(if (< (gc-info 4) 5000) (allocate-heap)))
```

### _args_

A variable, bound to the list of arguments passed to the main program siod.

### (\*catch tag body ...)

A special form. Tag is evaluated and kept in a special location while all
the forms in the body are evaluated. Normally returns the value of
the last form except if a \*throw is encountered within the dynamic
scope of the evaluations. Errors may be caught by using a tag of 'errorobj.

### _env_

A variable, bound to the list of environment values passed to the
main program siod.

### _eval-history-ptr_

A variable, default (), but if set to a list (possibly circular) then
each call to eval will cause the car of the list to receive a pointer
to the form being evaluated, and then the variable will be set to
the cdr of the list. Useful for writing a retrospective trace debugging
capability.

### _pi_

A variable, value 3.1416.

### _plists_

A variable, internal to the implementation of get and putprop.

### (\*throw tag value)

Locates an active \*catch for which the tag is identical and then
forces the \*catch form to return the value.

### _traced_

A variable, value is a list of procedures that have been traced.

### (+ x1 x2 ...)

Returns the sum of its arguments.

### (- x1 x2 ...)

With one argument returns the negation, returns the difference of
the first argument and the sum of the rest.

### (/ x1 x2 ...)

With one argument returns the inverse, otherwise returns the quotiont
of the first argument and the product of the rest.

### (< x y)

Returns true if x is numerically less than y.

### (<= x y)

Returns true if x is numerically less than or equal to y.

### (= x y)

Returns true if x is numerically equal to y.

### (> x y)

Returns true if x is numerically greater than y.

### (>= x y)

Returns true if x is numerically greater than or equal to y.

### (F_GETLK fd ltype whence start len)

The fd may be an integer or file. The function
**fcntl** (**U**) is called on the file
descriptor and an appropriate **struct flock**
constructed from the ltype, whence, start and len arguments, and the
lock operation F_GETLK. The ltype may be F_RDLCK,F_UNLCK, or F_WRLCK.
Whence may be SEEK_CUR, SEEK_END or SEEK_SET.

### (F_SETLK fd ltype whence start len)

Same as F_GETLCK but with lock operation F_SETLK. **U**.

### F_SETLKW fd ltype whence start len)

Same as F_GETLCK but with lock operation F_SETLKW. **U**.
For a good example see the command script
[cp-build](#command-interfaces-and-some-scripts-provided).

### (abs x)

Returns the absolute numerical value of x.

### (access-problem? filename method)

Invokes the access function (**U**) on
the filename and flags created from the method string which
should contain one or more of the characters "rwxf" returning non-null
if there is a problem with accessing the file in that way. For example:

```scheme
(if (access-problem? "x.y" "r") (error "can't read x.y"))
```

### (acos x)

Returns the inverse cosine of x.

### (alarm seconds flag)

Invokes the alarm function (**U**).
The handling of which will causes an error to be signaled in so many seconds.
But if flag is false then the error will not be signaled if the alarm took
place inside a system call or other critical code section.

### (allocate-heap)

Attempts to allocate (call the C library malloc procedure) to obtain
an additional heap. The size of the heap and the maximum number of heaps
are determined at startup time. Returns non-null if successful.

### (and form1 form2 form3 ...)

A special form which causes the evaluation of its subforms in order,
from left to right, continuing if and only if the subform returns
a non-null value.

### (append l1 l2 l3 l4 ...)

Returns a list which the result of appending all of its arguments.
Example:

```scheme
(append '(a b) '(c d)) => (a b c d)
```

### (apply function arglist)

Applies the function to the argument list arglist.

### (apropos substring)

Returns a list of all symbols containing the given substring.

### (aref array index)

Returns the element of the array at the given index.

### (array->hexstr string)

Takes a string or byte array and returns a string in representing the
values of the elements in hex.

### (aset array index value)

Stores the value at the given index in the array.

### (ash value bits)

Arithmetic shift of value a given number of bits to the left (positive)
or right (negative).

### (asin x)

Returns the inverse sin of x.

### (ass key alist function)

Returns the first element of the alist such that the function
applied to car of the element and the key returns a non-null value.
For example:

```scheme
(define (assq x alist) (ass x alist eq?))
```

### (assoc key alist)

Same as (ass key alist equal?).

### (assq key alist)

Same as (ass key alist eq?).

### (assv key alist)

Same as (ass key alist eql?).

### (atan x)

Returns the inverse tagent of x.

### (atan2 x y)

Returns the inverse tagent of x/y.

### (base64decode x)

Given a string X in base64 representation returns a string
with bytes computed using the base64 decoding algorithm.
See [rfc1521.txt](http://info.internet.isi.edu/in-notes/rfc/files/rfc1521.txt).

### (base64encode x)

Returns a string computed using the base64 encoding algorithm.

### (begin form1 form2 ...)

A special form which evaluates each of its subforms one after
another, returning the value of the last subform.

### (benchmark-eval nloops exp env)

A zero-overhead way of evaluating the exp n times.

### (benchmark-funcall1 nloops f arg1)

A zero-overhead way of calling the function f n times on arg1.

### (benchmark-funcall2 nloops f arg1 arg2)

A zero-overhead way of calling the function f n times on arg1 and arg2.

### (bit-and x y)

Returns the bitwise logical "and" (C language &amp; operator)
of numerical arguments x and y.

### (bit-not x)

Returns the bitwise logical complement (C language ~ operator)
of numerical argument x.

### (bit-or x y)

Returns the bitwise logical "or" (C language | operator)
of numerical arguments x and y.

### (bit-xor x y)

Returns the bitwise logical "xor" (C language ^ operator)
of numerical arguments x and y.

### (butlast x)

Returns a new list which has all the elements of the argument x except
for the last element.

### (bytes-append x1 x2 ...)

Returns a new byte array by appending its arguments which may be
strings or byte arrays.

### (caaar x)

Same as (car (car (car x))).

### (caadr x)

Same as (car (car (cdr x))).

### (caar x)

Same as (car (car x)).

### (cadar x)

Same as (car (cdr (car x))).

### (caddr x)

Same as (car (cdr (cdr x))).

### (cadr x)

Same as (car (cdr x)).

### (car x)

If x is the result of (cons a b) then (car x) is the same as a.

### (cdaar x)

Same as (cdr (car (car x))).

### (cdadr x)

Same as (cdr (car (cdr x))).

### (cdar x)

Same as (cdr (car x)).

### (cddar x)

Same as (cdr (cdr (car x))).

### (cdddr x)

Same as (cdr (cdr (cdr x))).

### (cddr x)

Same as (cdr (cdr x)).

### (cdr x)

If x is the result of (cons a b) then (cdr x) is the same as b.

### (chdir path)

Changes default directory to path. **U**.

### (chmod path mode)

Changes the file mode of path. **U**. For example, to add execute access
permission to the file f:

```scheme
(chmod f
       (encode-file-mode (append '(XUSR XGRP XOTH)
                                  (cdr (assq 'mode (stat f))))))
```

### (chown path uid gid)

Changes file ownership. **U**.

### (closedir stream)

Closes a directory stream. **U**.

### (cond clause1 clause2 ...)

A special form where each clause is processed until the predicate expression
of the clause evaluates true. Then each subform in the predicate
is evaluated with the value of the last one becoming the value of
the cond form:

```scheme
(**predicate-expression** form1 form2 ...)
```

### (cons x y)

Allocates a list object with x as the car and y as the cdr.
For example:

```scheme
(cons 1 (cons 2 (cons 3 ())))
```

evaluates to

```scheme
(1 2 3)
```

### (cons-array dimension kind)

Allocates an array (currently limited to one dimension). The kind
may be string, byte, double, or lisp (default).

### (copy-list x)

The toplevel cons objects of x are copied, returning a new list.

### (cos x)

Returns the cosine where x is in units of radians.

### (cpu-usage-limits soft-limit hard-limit)

Invokes getrlimit if the arguments are null or otherwise setrlimit.
**U**.

### (crypt key salt)

A form of string hash. **U**.

### (current-resource-usage kind)

Kind is the symbol SELF or CHILDREN, calls getrusage, **U**.

### (datlength data ctype)

Returns the dimension of the data as if viewed as an array by
the datref function.

### (datref data ctype index)

References the data as if it were an array of C data type ctype,
at the given index. The ctype may be CTYPE_CHAR, CTYPE_DOUBLE, CTYPE_FLOAT,
CTYPE_LONG, CTYPE_SHORT, CTYPE_UCHAR, CTYPE_ULONG, or CTYPE_USHORT.
The data may be a string or byte array.

### (decode-file-mode x)

Returns a list of symbols given a numerical file mode.

### (define subform1 subform2)

A special form used to assign a value to a variable in one of two ways:

```scheme
(define variable value)
```

or to create a procedure

```scheme
(define (procedure-name arg1 arg2 ...)
  form1
  form2
  ...)
```

### (delete-file path)

Deletes the file specified by path.

### (delq element list)

Deletes the elements of the list which are eq to its first argument.
Possibly modifying the list using the set-cdr! operation.

### (encode-file-mode list)

Takes a list of file mode symbols and returns the numerical value.
SUID, SGID, RUSR, WUSR, XUSR, RGRP, WGRP, XGRP, ROTH, WOTH, XOTH.

### (encode-open-flags list)

Takes a list of open (**U**)
flag symbols and returns a numerical value.
NONBLOCK, APPEND, RDONLY, WRONLY, RDWR, CREAT, TRUNC, EXCL.

### (endpwent)

See **U**.

### (env-lookup indentifier environment)

Returns an object such that the car is the location where
the value of identifier is stored.

### (eof-val)

Returns the object returned by read upon encountering and end of file
condition.

### (eq? x y)

Returns true if x and y are the same object.

### (equal? x y)

Returns true if x and y are equal objects.

### (eqv? x y)

Returns true if x and y are the same object or numerically equal.

### errobj

This variable is assigned to the offending object when the error
procedure has been invoked. Useful mainly during interactive debugging.

### (error message object)

Prints the error message then aborts the current execution
by invoking \*throw using the symbol errobj as the tag
and the cons of the message and the object as the value.
Equivalent to:

```scheme
(define (error message object)
  (if (> (verbose 0))
      (writes nil "ERROR: " message "\n"))
  (set! errobj object)
  (*throw 'errobj (cons message object)))
```

### (eval expression environment)

Evaluates the expression in the context of the environment. This
is **not** a special form. For example:

```scheme
(eval (read-from-string "(+ 1 2)"))
```

evaluates to 3.

### (exec path args env)

Calls execv or execve **U**.

### (exit status)

Calls exit **U**.

### (exp x)

Computes the exponential function of x.

### (fast-load path noeval-flag)

Loads a file of binary format expressions, if noeval-flag is true
returns a list of the forms instead of evaluating them.

### (fast-print object state)

Outputs a fast (binary) format representation of object,
where the state is a list of (file hash-array index).

### (fast-read state)

Inputs a form which had been output in fast (binary) format.

### (fast-save filename forms nohash-flag comment-string)

Creates a file by using fast-print to output each of the forms. A
true value for the nohash-flag will cause symbol names to be output
each time they are encountered. Otherwise a more optimal index
representation is used. The comment-string is used as the first line
of data in the file.

### (fchmod filedes mode)

The filedes may be an number or an open file object. **U**.

### (fclose stream)

Closes the open file stream. **U**.

### (fflush stream)

See **U**.

### (file-times path)

Returns a list of the st_ctime and the st_mtime returned
by the stat function. **U**.

### (first x)

Returns the first element (car) of the list x.

### (fmod x y)

Floating point mod. **U**.

### (fnmatch pattern string flags)

Returns true if the string matches the pattern.
**U**.

### (fopen path mode)

Opens the file and returns a file stream. **U**.

### (fork)

Create a child process. Returning a numerical pid in the parent,
() in the child, or call error if the child cannot be created.
**U**.

### (fread size-or-buffer stream)

Returns a new string of size bytes by calling
fread **U**. Or uses the buffer (a string
or a byte array) instead and returns the number of bytes read.
Returns () on end-of-file.

### (fseek file offset direction)

The direction is SEEK_CUR, SEEK_END or SEEK_SET.
**U**.

### (fstat stream)

Calls fstat **U** and returns
an alist with elements dev, ino, mode, nlink, uid, gid, rdev, size,
atime, mtime, ctime, blksize, blocks, flags, and gen.

### (ftell stream)

Calls ftell **U** to return the current
offset into a file.

### (fwrite data stream)

Write the data, a string or byte-array to the stream. Or
data can also be a list of a string or byte-array and a numerical
length.

### (gc)

Invokes the garbage collector.

### (gc-info item)

| Item | Value                                       |
| ---- | ------------------------------------------- |
| 0    | true if copying gc, false if mark and sweek |
| 1    | number of active heaps                      |
| 2    | maximum number of heaps                     |
| 3    | number of objects per heap                  |
| 4    | amount of consing of objects before next gc |

### (gc-status [flag])

If flag is not specified prints information about the gc.
Otherwise flag can be used to turn on or off gc messages
or turn on or off the gc itself when in stop and copy mode.

### (get object key)

Returns the key property of the object.

### (getc stream)

Reads a character from the stream, returns () for end of file.
**U**.

### (getcwd)

Returns the current working directory. **U**.

### (getenv name)

Returns the value of the environment variable named, or ().
**U**.

### (getgid)

Returns the group id of the process. **U**.

### (getgrgid gid)

Returns a list of members of the specified numerical group.
**U**.

### (getpass prompt)

Prompts the user and reads a line with echoing turned off.
**U**.

### (getpgrp)

Returns the process group ID of the calling process.
**U**.

### (getpid)

Returns the process ID of the calling process.
**U**.

### (getppid)

Returns the parent process ID of the calling process.
**U**.

### (getpwent)

Returns an alist representing the next item in the /etc/passwd file.
**U**.

### (getpwnam username)

Returns the /etc/passwd file entry for the given username.
**U**.

### (getpwuid)

Returns the /etc/passwd file entry fo the given user id.
**U**.

### (gets stream)

Reads a line from the stream, () on end-of-file.

### (getuid)

Returns the uid of the current process.
**U**.

### (gmtime value)

Decodes the value into an alist. The value defaults to the current time.
**U**.

### (hexstr->bytes str)

Decodes the hex representation into a byte array.

### (href table key)

The hash table is a one dimensional array of association lists.

```scheme
(define (href table key)
  (cdr (assoc key
    (aref table (sxhash key (length table))))))
```

### (hset table key value)

Stores the value into the hash table slot indicated by key.

### (html-encode str)

If str contains any special html characters (<>&amp;)
a new string is returned with these replaced by their cooresponding
representations &amp;lt; &amp;gt; &amp;amp;.

### (if predicate-form true-form false-form)

A special form that evaluates the true-form or the false-form
depending on the result of evaluating the predicate form.

### (intern str)

Looks up a string in the symbol table or enters a new symbol.

### (kill pid sig)

Calls the kill function **U**.
With sig defaulting to SIGKILL.

### (lambda (arg1 arg2 ...) form1 form2 ...)

Returns an applicable procedure object (CLOSURE) with the given
argument list and expression subforms. For example:

```scheme
(mapcar (lambda (x) (* x x)) '(1 2 3))
```

evaluates to:

```scheme
(1 4 9)
```

Also used by the define special form.

### (larg-default list index default-value)

Reference the list according to index, but skipping over
strings that begin with a colon or a dash. If the list is not long enough
it returns the default-value instead. Most useful when used
with the _args_ variable inside a main program.

### (last list)

Returns the last cons in a list.

### (last-c-error)

Returns the value of the C library strerror(errno)
**U** interned as a symbol.

### (lchown path owner group)

Changes the ownership of a symbolic link **U**.

### (length object)

Returns the length of an object which may be a string (acts like strlen)
or a list, or an array.

### (let (binding1 binding2 ...) form1 form2 ...)

A special form where each binding is a (variable value) pair.
It works by computing the values, establishing the bindings, and
then evaluating the forms, returning the value of the last one.
For example the following evaluates to 30:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

### (let\* (binding1 binding2 ...) form1 form2 ...)

A special form where each binding is a (variable value) pair.
It works by sequentially computing each value and then
establishing a binding. For example the following evaluates to 30:

```scheme
(let* ((x 10)
      (y (+ x 10)))
  (+ x y))
```

### (letrec (binding1 binding2 ...) form1 form2 ...)

Useful when the value forms of the bindings are lambda expressions
with which you desire to program mutually recursive calls.

### (link existing-file entry-to-create)

Creates a hard link **U**.

### (list item1 item2 ...)

Conses up its arguments into a list.

### (lkey-default list index default-value)

Returns the substring on the right hand size of the equal sign
of the first element of the list of the form **index=value**,
or the default-value if none are found. Useful when processing the
_args_ value inside a main program.

### (load fname noeval-flag search-flag)

If search-flag is true it looks for fname in the current directory
and then in the SIOD_LIB directory. The forms from the file are
parsed according to the "parser:xxx" directive at the begining of the file
(default "parser:read").
If the neval-flag is true then a list of the forms is returned
otherwise the forms are evaluated.

### (load-so fname init_fcn)

Loads the dynamic library specified by fname, invoking the init_fcn
if specified (default init_fname).

### (localtime value)

Returns an alist representing the value as a localtime.
**U**. Value defaults to the current time.

### (log x)

Computes the natural logarithm of x.

### (lref-default list index default-fcn)

Returns the index element of the list or the result of calling
the default-fcn if the list is not long enough.

### (lstat path)

Returns the stat information of a logical link.
**U**.

### (make-list length element)

Creates a list of the given length filled with the element specified.

### (mapcar fcn list1 list2 ...)

Returns a list which is the result of applying the fcn to the
elements of each of the lists specified.

### (max x1 x2 ...)

Returns the maximum of x1, x2, etc.

### (md5-final state)

Returns a byte array computed from the state,
derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm.
See [rfc1321.txt](http://info.internet.isi.edu/in-notes/rfc/files/rfc1321.txt).
Example:

```scheme
(define (md5 str)
  (let ((s (md5-init)))
    (md5-update s str)
    (array->hexstr (md5-final s))))
```

### (md5-init)

Returns an md5 algoritm state as a byte array.

### (md5-update state string length)

Performs the update step of the md5 algorithm using data from the
string up to length, or length can be an open file object, in which case
the data from the file is used to perform the update.

### (member key list)

Returns the portion of the list where the car is equal to the key, or ()
if none found.

### (memq key list)

Returns the portion of the list where the car is eq to the key, or ()
if none found.

### (memv key list)

Returns the portion of the list where the car is eqv? to the key, or ()
if none found.

### (min x1 x2 ...)

Returns the numerical minimum of its arguments.

### (mkdatref ctype ind)

Creates a closure functionally equivalent to (lambda (x) (datref x ctype ind)).

### (mkdir path mode)

Creates a directory with the specified numerical access mode.
**U**.

### (mktime alist)

Returns the numerical time value cooresponding to the alist
in the same format as returned by the localtime function.
**U**.

### (nconc l1 l2)

Makes the cdr of the last cons of l1 point to l2.

### (nice increment)

Changes the priority of the current process by the increment.
**U**.

### nil

Do not change the value of this variable which is bound to the empty list.

### (not x)

Returns the reverse truth sense of x.

### (nreverse list)

Destructive reversal of the elements of a list using set-cdr!.

### (nth index list)

Reference the list using index, with the first element being index 0.

### (null? x)

Returns true of x is the empty list.

### (number->string x base width precision)

Formats the number according to the base, which may be 8, 10, 16 or the symbol
e or f. The width and precision are both optional.

### (number? x)

Returns true of x is a number.

### (opendir path)

Returns a directory stream. Not that in unix path is the
name of a directory, but in WIN32 path is a wildcard pattern.
**U**.

### (or form1 form2 ...)

A special form which causes the evaluation of its subforms in order,
from left to right until a form evaluates to a non-null value.

#### (os-classification)

Returns unix, win32, vms.

### (pair? x)

Returns true if x is a pair (created by cons).

### (parse-number str)

Convers a string to a number.

### (pclose stream)

Used to close a stream opened using popen. Makes sure the
associated process is killed.
**U**.

### (popen command type)

Executes the command in a child process and opens a stream
connected to the process standard output if type is r, or
the standard input if type is w.
**U**.

### (pow x y)

Computes the result of x raised to the y power.

### (prin1 object stream)

Outputs the standard readable representation of the object to the stream,
which defaults to the standard output.

### (print object stream)

Same as prin1 followed by output of a newline.

### (print-to-string object string no-trunc-flag)

Puts the readable representation of the object into the string,
starting at the first character unless the no-trunc-flag is true,
in which case the representation starts at the current length of
the string.

### (prog1 form1 form2 form3 ...)

A special form which evaluates all its subforms but returns the value
of the first one. A useful shorthand to employ instead of using a let.

### (putc char stream)

Outputs the character to the stream.
**U**.

### (putenv setting)

With a setting is of the form "key=value" makes a new environment
binding available to the getenv function of the current and
subsequent child processes, or updates an old one.
**U**.

### (putprop object value key)

Not implemented.

### (puts string stream)

Outputs the string to the stream.
**U**.

### (qsort list predicate-fcn access-fcn)

Implements the recursive quicksort algorithm on elements
of the list compared by using the predicate-fcn on the results
of invoking the access-fcn.

| Example                        | Result        |
| ------------------------------ | ------------- |
| (qsort '(3 1 5 4 2) &lt)       | (1 2 3 4 5)   |
| (qsort '((3 a) (2 b)) &lt car) | ((2 b) (3 a)) |

### (quit)

Cause the read-eval-print loop to return, usually resulting in
an exit from the main program of siod, but may not when other
C programs are utilizing the libsiod functionality.

### (quote x)

A special form that returns x without evaluating it.
Commonly written in abbreviated format as 'x.

### (rand modulus)

Computes a random number from 0 to modulus-1. Uses C library rand.

### (random modulus)

Computes a random number from 0 to modulus-1. Uses C library random.

### (read stream)

Inputs characters from the stream returns the parsed standard expression,
or (eof-val).

### (read-from-string string)

Performs a read operation on the characters in the string.

### (readdir directory-stream)

Returns the name of the next entry in the directory stream
or () of none left.

### (readline stream)

Reads a line of characters from the stream, returning () on end of file.
The terminating newline is not included in the string, which is usually
more convenient. For example, this procedure for loading a tab-delimited
spreadsheet file:

```scheme
(define (load-spread-sheet filename)
  (if (>= (verbose) 2)
      (writes nil ";; loading spread sheet " filename "\n"))
  (let ((result nil)
  (line nil)
  (f (and (not (equal? filename "-")) (fopen filename "r"))))
    (while (set! line (readline f))
      (set! result (cons (strbreakup line "\t") result)))
    (and f (fclose f))
    (nreverse result)))
```

### (readlink path)

Returns the contents of the symbolic link at path.
**U**.

### (realtime)

Returns a double precision floating point value representation
of the current realtime number of seconds. Usually precise to about
a thousandth of a second.

### (rename from-path to-path)

Renames a directory or file within a file system.
**U**.

### (require path)

Computes a variable name by concatenating "_" + path + "-loaded_"
and then calling (load path nil t) if and only if the variable
is not bound to true. After the file is loaded the variable is
set to true. This is the correct way of making sure a file is only
loaded once.

### (require-so path)

Computes a variable name by concatenating "init\_" + path,
and calling (load-so path) if and only if the variable is not
bound to true. After the shared library has been loaded the variable
is set to true. The correct way of making sure a shared library is
only loaded once is:

```scheme
(require-so (so-ext 'name))
```

### (rest x)

Returns the rest of the list x, in other words the cdr.

### (reverse x)

Returns a new list which has elements in the reverse order of
the list x.

### (rld-pathnames)

Returns a list of the pathnames which represent shared libraries that
have been loaded by the the current process.

### (rmdir path)

Removes the directory entry specified by path.
**U**.

### (runtime)

Returns a list of containing the current cpu usage in seconds
and the subset amount of cpu time that was spent performing garbage
collection during the currently extant read-eval-print loop cycle.

### (save-forms filename forms how)

Prints the forms to the file, where how can be "w" (default) or "a"
to append to the file.

### (sdatref spec data)

Used as the %%closure-code by mkdatref.

### (set! variable value)

A special form that evaluates the value subform to get a value,
and then assigns the variable to the value.

### (set-car! cons-cell value)

Changes the car of the cons-cell object to point to the value.

### (set-cdr! cons-cell value)

Changes the cdr of the cons-cell object to point to the value.

### (set-eval-history length circular-flag)

Creates a list of the specified length and establishes bindings
for _eval-history-ptr_ and _eval-history_. The list is circular if
the flag is specified true. Try the following:

```scheme
(define (fib x) (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))
(set-eval-history 200)
(fib 10)
(mapcar (lambda (x) (if (pair? x) (car x) x)) *eval-history*)
```

### (set-symbol-value! symbol value env)

Finds the location of the value cell for the specified symbol
in the environment env and sets the value.

### (setprop obj key value)

Not implemented.

### (setpwent)

Resets the pointer into the /etc/passwd file.
**U**.

### (setuid x)

Sets the userid of the process.
**U**.

### (sin x)

Computes the sine function of the angle x in radians.

### (siod-lib)

Return the setting of the siod library directory.

### (sleep n)

Sleep for n seconds, where n may be fractional on some systems.

### (so-ext path)

Append the path with the file extension for shared libraries.

### (sqrt x)

Compute the square root of x.

### (srand seed)

Reset the algorithm seed for the rand function.
**U**.

### (srandom seed)

Reset the algorithm seed for the random function.
**U**.

### (stat path)

Return an alist describing file status information, or () if
the path cannot be accessed, (last-c-error) may be used to return
the reason.

### (strbreakup string sep)

Return a list of the portions of string indicated by the separator.

```scheme
(strbreakup "x=y&amp;z=3" "&amp;") => ("x=y" "z=3")
```

### (strcat str1 str2)

Copies the string str2 into str1 starting at the current active end
of str1, which is determined by the location of a 0 byte,
calling error if there is not enough room left in str1.
**U**.

### (strcmp str1 str2)

Returns 0 if str1 and str2 are equal, or -1 if str1 is alphabetically
less than str2 or 1 otherwise.
**U**.

### (strcpy str1 str2)

Copies str1 into str1 or calling error if there is not enough room.
**U**.

### (strcspn str indicators)

Returns the location of the first character in str which is
found in the indicators set, returns the length of the string if none
found.
**U**.

### (strftime format-string alist)

Uses the format-string to compute a string using broken-up time/data
information from the alist (defaults to the current time), for example:

```scheme
(strftime "%B" '((mon . 3))) => "April"
```

**U**.

### (string->number str radix)

Converts the string to a number assuming the specified radix.

### (string-append str1 str2 str3 ...)

Returns a new string which contains the concatenation of
all its string arguments.

### (string-dimension str)

Returns the maximum possible length of a string array.

### (string-downcase str)

Return a new string converting all the characters of str to lowercase.

### (string-length str)

Returns the active string length of str.

### (string-lessp str1 str2)

Return true if str1 is alphabetically less than str2.

### (string-search key str)

Locate the index of the key in the specified string. Returns () if
not found.

### (string-trim str)

Return a new string made by trimming whitespace from the left
and right of the specified string.

### (string-trim-left str)

Like string-trim but only the left hand side.

### (string-trim-right str)

Like string-trim but only the right hand side.

### (string-upcase str)

Returns a new string with all the lowercase characters converted
to uppercase.

### (string? x)

Returns true if x is a string.

### (strptime str format alist)

Parses str according to format and merges the values with the alist.

```scheme
(cdr (assq 'mon (strptime "March" "%B"))) => 2
```

**U**.

### (strspn str indicators)

Returns the location of the first character in str which is not
found in the indicators set, returns the length of the str if none found.
For example:

```scheme
(define (string-trim-left x)
  (substring x (strspn x " \t")))
```

**U**.

### (subset pred-fcn list)

Return the subset of the list such that the elements satisify
the pred-fcn. For example:

```scheme
(subset number? '(1 b 2 c)) => (1 2)
```

### (substring str start end)

Returns a new string made up of the part of str begining at start
and terminating at end. In other words, the new string has a length
of end - start.

### (substring-equal? str str2 start end)

An efficient way to determine if the substring of str2 specified
by start and end is equal to str1.

### (swrite stream table form)

This is the same as the write-smart-html procedure
described in
<ftp://ftp.std.com/pub/gjc/www95-paper.html>.

### (sxhash data modulus)

Computes a recursive hash of the data with respect to the specified
modulus.

### (symbol-bound? symbol env)

Returns true of the symbol is bound in the environment.

### (symbol-value symbol env)

Returns the value of the symbol in the environment.

### (symbol? x)

Returns true if x is a symbol.

### (symbolconc arg1 arg2 ...)

Slightly more efficient than calling intern on the result of
using string-append on the arguments. This procedure actually
predates the availability of the string data type in SIOD.

### (symlink contents-path link-path)

Creates a directory entry link-path pointing to the contents-path.
**U**.

### (system arg1 arg2 ...)

Appends the string arguments to form a command to be executed
by the operating system.
**U**.

### t

Please do not change the global value of this variable, bound to a
true value.

### (tan x)

Computes the tagent of the angle x specified in radians.

### (the-environment)

A special form which returns the interpreter environment structure
for the current lexical scope.

### (trace fcn1 fcn2 ...)

Traces the specified interpreted procedures by modifying the
closure objects.

### (trunc x)

Returns the integer portion of x.

### (typeof x)

Returns a symbol describing the type of the object x, or
the integer type code.

### (unbreakupstr list sep)

The reverse of strbreakup. The following example saves a list of
lists as a tab delimited spreadsheet:

```scheme
(define (save-spread-sheet filename data)
  (if (>= (verbose) 2)
      (writes nil ";; saving spread sheet " filename "\n"))
  (let ((result data)
  (f (and (not (equal? filename "-")) (fopen filename "w"))))
    (while result
      (writes f (unbreakupstr (car result) "\t") "\n")
      (set! result (cdr result)))
    (and f (fclose f))))
```

### (ungetc char stream)

Puts the char back into the stream for the next call to getc.

### (unix-ctime x)

Converts the integer time x into a string.
**U**

### (unix-time)

Returns the current number of seconds since 1-JAN-1970 GMT.
**U**

### (unix-time->strtime x)

Returns a string of the form "YYYYMMDDHHmmSSdd" which is useful
in some contexts. This predates the availability of the strftime procedure.

### (unlink path)

Deletes the specified entry from the directory stucture.
**U**

### (untrace fcn1 fcn2 ...)

Untraces the specified procedures.

### (url-decode str)

Performs the url decode operation on the str.
<!-- chtml.html is nowhere to be found -->
See <chtml.html> for example usage.

### (url-encode str)

Locates characters in the str which should not appear in a url,
and returns a new string where they have been converted to
the %NN hex representation. Spaces are converted to "+" signs.

### (utime path modification-time access-time)

Sets the file modification and access times.
**U**

### (verbose arg)

Sets the verbosity level of SIOD to the specified level or returns
the current level if not specified.

| Verbose Level | Effect on System                                              |
| ------------- | ------------------------------------------------------------- |
| 0             | No messages.                                                  |
| 1             | Error messages only.                                          |
| 2             | Startup messages, prompts, and evaluation timing.             |
| 3             | File loading and saving messages.                             |
| 4 (default)   | Garbage collection messages.                                  |
| 5             | display of data loaded from files and fetched from databases. |

### (wait pid options)

Waits on a child process by calling the waitpid function,
where options may be a list containing (WCONTINUED WNOWAIT WNOHANG WUNTRACED).
Returns a list of the process pid and final exit status. The
fork-test.scm and http-stress.scm modules provide example usage.
**U**

### (while pred-form form1 form2 ...)

If pred-form evaluates true it will evaluate all the other forms
and then loop.

### (writes stream data1 data2 data3 ...)

Outputs the data arguments to the stream without quoting strings
or special characters.

## Reference Section for extension-provided procedures

### Extension: acct

Interface to unix accounting records.

#### (decode_acct string)

Built-in subroutine of 1 argument.
Takes a string or byte array of length SIZEOF_ACCT and
returns an alist. You would get this data by using fread
on an appropriate accounting file. Currently an OSF/1 specific
implementation.

#### (decode_tacct string)

built-in subroutine of 1 argument. Takes a string of SIZEOF_TACCT.
Currently an OSF/1 specific implementation.

#### UTMP_FILE

variable, value data type tc_string

#### WTMP_FILE

variable, value data type tc_string

#### (endutent)

built-in subroutine of 0 arguments. **U**

#### (getutent)

built-in subroutine of 0 arguments. **U**

#### (setutent)

built-in subroutine of 0 arguments. **U**

#### (utmpname path)

built-in subroutine of 1 argument. **U**

### Extension: gd

Interface to libgd.a, for producing GIF data.
See <http://www.boutell.com/gd/>. The scheme functions all follow the C
functions in name and call interface. **U**.
The piechart.scm module is an example usage.

#### gdBrushed

variable, value data type tc_flonum

#### gdFont.h

built-in subroutine of 1 argument. Returns height of a font.

#### gdFont.w

built-in subroutine of 1 argument. Returns width of a font.

#### gdFontGiant

variable, contains a font.

#### gdFontLarge

variable, contains a font.

#### gdFontMediumBold

variable, contains a font.

#### gdFontSmall

variable, contains a font.

#### gdFontTiny

variable, contains a font.

#### gdImageArc

built-in subroutine 0 or more arguments.

#### gdImageChar

built-in subroutine 0 or more arguments.

#### gdImageCharUp

built-in subroutine 0 or more arguments.

#### gdImageColorAllocate

built-in subroutine 0 or more arguments.

#### gdImageColorClosest

built-in subroutine 0 or more arguments.

#### gdImageColorExact

built-in subroutine 0 or more arguments.

#### gdImageColorTransparent

built-in subroutine of 2 arguments.

#### gdImageCreate

built-in subroutine of 2 arguments.

#### gdImageCreateFromGif

built-in subroutine of 1 argument.

#### gdImageCreateFromXbm

built-in subroutine of 1 argument.

#### gdImageFill

built-in subroutine 0 or more arguments.

#### gdImageFillToBorder

built-in subroutine 0 or more arguments.

#### gdImageFilledPolygon

built-in subroutine of 3 arguments.

#### gdImageFilledRectangle

built-in subroutine 0 or more arguments.

#### gdImageGif

built-in subroutine of 2 arguments.

#### gdImageGifmem

built-in subroutine of 2 arguments.

#### gdImageInterlace

built-in subroutine of 2 arguments.

#### gdImageLine

built-in subroutine 0 or more arguments.

#### gdImagePolygon

built-in subroutine of 3 arguments.

#### gdImageRectangle

built-in subroutine 0 or more arguments.

#### gdImageSetPixel

built-in subroutine 0 or more arguments.

#### gdImageString

built-in subroutine 0 or more arguments.

#### gdImageStringUp

built-in subroutine 0 or more arguments.

#### gdPoint

built-in subroutine 0 or more arguments.

#### gdPoint.x

built-in subroutine of 3 arguments.

#### gdPoint.y

built-in subroutine of 3 arguments.

#### gdStyled

variable, value data type tc_flonum

#### gdStyledBrushed

variable, value data type tc_flonum

#### gdTiled

variable, value data type tc_flonum

#### gdTransparent

variable, value data type tc_flonum

### Extension: ndbm

These routines take string or byte
array arguments, and return byte arrays. The
lisp functions substring and datref can be useful to extract
information from a byte array. **U**.

#### DBLKSIZ

variable, value data type tc_flonum

#### DBM_INSERT

variable, value data type tc_flonum

#### DBM_REPLACE

variable, value data type tc_flonum

#### PBLKSIZ

variable, value data type tc_flonum

#### (dbm_close handle)

Closes the files associated with the database handle.

#### (dbm_delete handle key)

Deletes the key from the index of the database.

#### (dbm_dirfno handle)

Returns a file descriptor for useful for passing
to the F_SETLKW function.

#### (dbm_error handle)

built-in subroutine of 1 argument.

#### (dbm_fetch handle key)

built-in subroutine of 2 arguments.

#### (dbm_firstkey handle)

Returns the first key in the index.

#### (dbm_nextkey handle)

Returns the next key in the index.

#### (dbm_open path open-flags mode)

Returns a handle to the database, opening and creating the .dir
and .pag files if needed. Use encode-open-flags to compute the
open-flags argument and encode-file-mode to compute the mode
argument. The file access mode argument is used only if the file
needs to be created for the first time.

#### (dbm_pagfno handle)

Returns a file descriptor for useful for passing
to the F_SETLKW function.

#### (dbm_rdonly handle)

Returns true if the database is read only.

#### (dbm_store handle key data flag)

Stores the data index by key into the database.
The flag argument may be DBM_INSERT or DBM_REPLACE.

### Extension: parser_pratt

Alternative syntax interface. Just the tokenizer is included
here. The parser is provided by parser_pratt.scm and pratt.scm.

#### (pratt_read_token buffer token-table stream)

built-in subroutine of 3 arguments.

#### Example, hello.scm

```scheme
#!/usr/local/bin/siod -v01,-m2 -*-mode:text;parser:pratt-*-
main() :=
{
  writes(nil,"Hello Scheme World.\n");
  fflush(nil);
  writes(nil,"fib(20) = ",fib(20),"\n");
}
$

fib(x) := if x < 2 then x else fib(x-1) + fib(x-2)
$
```

### Extension: regex

Regular expression interface. **U**.
See smtp.scm for example usage. See [regex.md](./regex.md) for regex library
source if your C runtime does not have it available.

#### (regcomp pattern flags)

Returns a handle to a regular expression compiled from the
pattern and flags: REG_EXTENDED, REG_ICASE, REG_NEWLINE, REG_NOSUB.

#### (regerror code handle)

Returns a string describing the error code argumented by
error information from the compiled regular expression handle.

#### (regexec handle string flags)

If the string matches then a list of (start . end) pairs are returned
indicating the substrings involved in the match against the pattern
of the compiled regular expression. Otherwise an error code is
returned: REG_BADBR, REG_BADPAT, REG_BADRPT, REG_EBRACE,
REG_EBRACK, REG_ECHAR,
REG_ECOLLATE, REG_ECTYPE, REG_EESCAPE, REG_EPAREN, REG_ERANGE,
REG_ESPACE, REG_ESUBREG,
REG_NOMATCH, REG_NOTBOL, REG_NOTEOL.

### Extension: sql_oracle

Uses the OCI, Oracle Call Interface.
See the sql_oracle.scm module for examples.

### Extension: sql_rdb

Uses the RDB SQL SERVICES, sqlsrv_xxxx routines.
See the sql_rdb.scm module for examples.

### Extension: sql_sybase

Uses the Sybase Client ct*xxxx routines.
See the sql_sybase.scm module for examples.
All these procedures may print error and warning messages
depending on the current verbose level, and also
assign the variable \_sybase-message* with similar information.

#### (sybase-close [handle])

Closes the connection to the database handle which defaults
to the value of the variable _sybase_.

#### (sybase-execute [handle] string cmd-type key1 arg1 key2 arg2 ...)

Passes the string to the SQL server to be processed.
The cmd-type is one of the sybase command types, such as
CS*RPC_CMD or CS_LANG_CMD. The optional keys are procedure argument
names. Pass "" as the key for un-named positional arguments.
The result of sybase-execute is an list where the first
element represents the status and the rest is an association
list with CS_ROW_RESULT, CS_STATUS_RESULT, CS_PARAM_RESULT,
and CS_MSG_RESULT elements. Rows are returned as lists of arrays.
The handle defaults to the value of the variable \_sybase*.

#### (sybase-open key1 arg1 key2 arg2 ...)

Opens a connection to the database specified by the current
settings of the environment variables SYBASE and DSQUERY.
The possible argument keys are username, password, and appname.
A handle to the database connection structures is returned,
and also set as the value of the variable _sybase_ if it is currently ().

#### (sybase-status [handle])

Returns an list with information about the connection status,
servername, and hostname. Handle defaults to the setting of _sybase_.

### Extension: ss

Provides a simplified, stream-oriented interface to socket
functionality. **U**.
Example usages in smtp.scm and http-server.scm.
This doesn't provide all the functionality and options that
a straight socket interface would provide, but it has proven to
be highly convenient for most client and server applications.

#### (get-protocol-name number)

Calls getprotobynumber, returns a list of names.

#### (get-service-name port-number)

Calls getservbyport. Returns a list of the protocol and the service
names.

#### (gethostbyaddr x)

Calls gethostbyaddr, operating on the numerical address x.

#### (gethostbyname name)

Calls gethostbyname returning a list where the first element
is the canonical host name and the rest is an association list
containing aliases, addr_list and addrtype. For example to
obtain a list of numerical address for the host:

```scheme
(mapcar inet_addr
  (cdr (assq 'addr_list
    (gethostbyname (gethostname)))))
```

#### (gethostname)

Returns the configured name of the host.

#### (inet_addr str)

Converts a "x.x.x.x" dotted notation string or a byte
array into a number.

#### (s-accept stream)

Returns a new stream based on accepting a connection
from the socket assocated with the specified stream which must
be in a listen state.

#### (s-close stream)

Closes the socket assocated with the stream.

#### (s-drain stream)

Reads all input from the socket associated with the stream.

#### (s-force-output stream)

Makes sure all buffered output is sent to the socket associated
with the stream.

#### (s-getc stream)

Reads one character from the stream. () on eof.

#### (s-gets stream)

Reads a line from the stream.

#### (s-open address port listen-flag)

When listen-flag is () this procedure establishes a connection
to the remote address and port specified. If listen-flag is true
it creates a socket bound to the local address and port and initiates.
The address may be a host name, a number, or a dot-string notation.
The port may be the name of a protocol or a number.

#### (s-putc char stream)

Outputs the char to the socket stream.

#### (s-puts string stream)

Outputs the string to the socket stream.

#### (s-read size-or-buffer stream)

Reads the specified number of characters from the stream,
returning a string or () on end of file, or attempts to fill the buffer with
characters from the stream, returning the actual number read.

#### (s-read-sexp stream)

Reads a standard symbolic lisp expression from the stream.
If the SIOD I/O was better modularized this procedure would not
be needed.

#### (gethostid)

Unix only. Returns a 32 bit number.

#### (wsa-data)

WIN32 only. Returns information about the tcp-ip stack.

### Extension: tar

This is helpful for the efficient implementation of utilities
that need to deal with tar files, avoiding repeated use of
the substring and datref functions.

#### _tar-header-size_

Bound to the size of a tar header: 512.

#### (checksum-tar-header string)

Computes the standard tar header checksum algorithm.

#### (decode-tar-header string)

Returns an association list containing entries for all the
standard components of a tar header structure. This can be
done using substring and string->number but this procedure is
faster and more convenient.

## Command interfaces and some scripts provided

These commands all have unix manual pages, which in the interest of brevity
are not duplicated here.

| Command          | Purpose                                             |
| ---------------- | --------------------------------------------------- |
| siod             | the interpreter                                     |
| csiod            | command linker for siod scripts                     |
| cp-build         | a file copy command with versioning and audit trail |
| ftp-cp           | passive-mode ftp copy                               |
| ftp-put          | passive-mode ftp put with rename                    |
| http-get         | command-line http client                            |
| http-stress      | stress and http server                              |
| proxy-server     | serializing, logging, http proxy server             |
| snapshot-dir     | create a snapshot of a directory hierarchy.         |
| snapshot-compare | compare hierarchy snapshots, with options.          |

## Some scheme coded library modules

| Name             | Purpose                                                   |
| ---------------- | --------------------------------------------------------- |
| cgi-echo.scm     | example cgi script, echo the environment                  |
| find-files.scm   | works like the unix find command, but provides lisp data. |
| fork-test.scm    | example use of fork                                       |
| hello.scm        | an example command using infix syntax                     |
| http-server.scm  | useful as a socket example                                |
| http-stress.scm  | http client with stress features                          |
| http.scm         | more http client examples                                 |
| ftp.scm          | support for file transfer protocol                        |
| maze-support.scm | cgi script example, provides a run-maze subroutine        |
| parser_pratt.scm | interface to infix language parser                        |
| pop3.scm         | A pop3 client                                             |
| pratt.scm        | infix language parser                                     |
| selfdoc.scm      | create a table of built-in procedures                     |
| siod.scm         | mostly obsolete collection of utility subroutines         |
| smtp.scm         | smtp client subroutines                                   |
| sql_oracle.scm   | utilities for oracle database client                      |
| sql_rdb.scm      | utilities for rdb database client                         |
| sql_sybase.scm   | utilities for sybase database client                      |
| piechart.scm     | a CGI script that returns a piechart as a GIF             |

## Garbage Collection

When SIOD was first released in April of 1988, it had a stop-and-copy
garbage collector which could only run at the toplevel of the
read-eval-print loop because it had no certain way of determine which
objects on the stack were truly pointers; unless of course the runtime
system was modified to make use of information the compiler provided to
the debugger, in a way that a real lisp compiler would be required to
provide. About that same time Jim O'Dell related to me the debugging
heuristic he had used in his port of Franz Lisp to the Macintosh, by
scanning the stack for items which appeared to be pointers but which
had not been properly found by the mark phase of the gc. I suggested
at the time that he might as well get rid of the explicit book-keeping
code spread throughout Franz Lisp and stick with the heuristic. But since
he had already found all the bugs in the book-keeping in the runtime
and compiler output there was little benefit and great risk to doing it.

Then, by the time SIOD had been out for a year there had been
enough complaints about the lack of fully available GC that I was
motivated to utilize a stack heuristic, since I had no intention of
maintaining any explicit book-keeping code in the source. The published
arguments in favor of the conservative approach described by Hans
Boehm of Xerox Parc then reduced this design decision to a no-brainer,
and his implementation suggested the use of setjmp as a sufficiently
portable way for C code to get at the machine register set without
introducing assembling language. To SIOD then I added only about 300
bytes of VAX instructions to the size of the runtime system.

There are two storage management techniques which may be chosen at runtime
by specifying the -g argument flag.

- -g1 is stop-and-copy. This is the simplest and most portable implementation.
  GC is only done at toplevel.
- -g0 (the default) is mark-and-sweep. GC is done at any time, required or
  requested. However, the implementation is not as portable.

If you get strange errors on a machine architecture not listed
then you may be forced to use -g1 until you investigate and contact
the author for advise.

### Stop and Copy

As one can see from the source, garbage collection is really quite an easy
thing. The procedure gc_relocate is about 25 lines of code, and
scan_newspace is about 15.

The real tricks in handling garbage collection are (in a copying gc):

1. keeping track of locations containing objects
2. parsing the heap (in the space scanning)

The procedure gc_protect is called once (e.g. at startup) on each
**global** location which will contain a lisp object.

That leaves the stack. The beleive is that if we had chosen not to
use the argument
and return-value passing mechanism provided by the C-language
implementation, (also known as the "machine stack" and "machine
procedure calling mechanism) this lisp would be larger, slower, and
rather more difficult to read and understand. Furthermore it would be
considerably more painful to _add_ functionality in the way of SUBR's
to the implementation.

Aside from writing a very machine and compiler specific assembling language
routine for each C-language implementation, embodying assumptions about
the placement choices for arguments and local values, etc, we
are left with the following limitation:

**YOU CAN GC ONLY AT TOP-LEVEL**  
However, this fits in perfectly with the programming style imposed in
many user interface implementations including the MIT X-Window Toolkit.
In the X Toolkit, a callback or work procedure is not supposed to spend
much time implementing the action. Therefore it cannot have allocated
much storage, and the callback trampoline mechanism can post a work
procedure to call the garbage collector when needed.

Our simple object format makes parsing the heap rather trivial.
In more complex situations one ends up requiring object headers or markers
of some kind to keep track of the actual storage lengths of objects
and what components of objects are lisp pointers.

Because of the usefulness of strings, they were added by default into
SIOD 2.6. The implementation requires a hook that calls the C library
memory free procedure when an object is in oldspace and never
got relocated to newspace. Obviously this slows down the stop-and-sweep
GC, and removes one of the usual advantages it has over mark-and-sweep.

### Mark and Sweep

In a mark-and-sweep GC the objects are not relocated. Instead
one only has to LOOK at objects which are referenced by the argument
frames and local variables of the underlying (in this case C-coded)
implementation procedures. If a pointer "LOOKS" like it is a valid
lisp object (see the procedure mark_locations_array) then it may be marked,
and the objects it points to may be marked, as being in-use storage which
is not linked into the freelist in the gc_sweep phase.

Another advantage of the mark_and_sweep storage management technique is
that only one heap is required.

The main disadvantages are:

1. start-up cost to initially link freelist. (can be avoided by more general
   but slower NEWCELL code).
2. does not COMPACT or LOCALIZE the use of storage. This is poor engineering
   practice in a virtual memory environment.
3. the entire heap must be looked at, not just the parts with useful storage.

In general, mark-and-sweep is slower in that it has to look at more
memory locations for a given heap size, however the heap size can
be smaller for a given problem being solved. More complex analysis
is required when READ-ONLY, STATIC, storage spaces are used
(which we do not support, currently).
Additionally the most sophisticated stop-and-copy storage management
techniques take into account considerations of object usage temporality.

The technique assumes that all machine registers the GC needs to
look at will be saved by a setjmp call into the save_regs_gc_mark data,
and that every thing else is on the C runtime stack. Hence we
have some assumptions that impact portability.

## Porting

The most heavily ifdef'd module is slibu.c because it utilizes "unix"
runtime functionality. You may want to start out by porting
the "sample" main program along with the modules needed to be static
linked with it.

If your system or C runtime needs to poll for the interrupt signal
mechanism to work, then define INTERRUPT_CHECK to be something
useful.

The STACK_LIMIT and STACK_CHECK macros may need to be conditionized.
They currently assume stack growth downward in virtual address.
The subr (%%stack-limit setting non-verbose) may be used to change the
limits at runtime.

The stack and register marking code used in the mark-and-sweep GC
is unlikely to work on machines that do not keep the procedure call
stack in main memory at all times. It is assumed that setjmp saves
all registers into the jmp_buff data structure. If your target machine
architecture is radically different, such as using linked procedure
call frames of some kind, not organized as a stack, then it would be
best if you could find vendor-supported routines for walking these
frames, such as would be utilized by a debugger. The mark_locations
procedure can then be invoked multiple times with the proper start
and end addresses.

If the stack is not always aligned (in LISP-PTR sense) then the
gc_mark_and_sweep procedure will not work properly unless steps
are taken to work around the problem.

Example, assuming a byte addressed 32-bit pointer machine:

```assembly
stack_start_ptr: [LISP-PTR(4)]
                 [LISP-PTR(4)]
                 [RANDOM(4)]
                 [RANDOM(2)]
                 [LISP-PTR(4)]
                 [LISP-PTR(4)]
                 [RANDOM(2)]
                 [LISP-PTR(4)]
                 [LISP-PTR(4)]
stack_end:       [LISP-PTR(4)]
```

As mark_locations goes from start to end it will get off proper alignment
somewhere in the middle, and therefore the stack marking operation will
not properly identify some valid lisp pointers.

Fortunately there is an easy fix to this. A more aggressive use of
our mark_locations procedure will suffice.

For example, say that there might be 2-byte quantities inserted into
the stack. Then use two calls to mark_locations, as as in THINK_C
on the Macintosh:

```c
mark_locations(((char *)stack_start_ptr) + 0,((char *)&amp;stack_end) + 0);
mark_locations(((char *)stack_start_ptr) + 2,((char *)&amp;stack_end) + 2);
```

If we think there might be 1-byte quantities, then 4 calls are required:

```c
mark_locations(((char *)stack_start_ptr) + 0,((char *)&amp;stack_end) + 0);
mark_locations(((char *)stack_start_ptr) + 1,((char *)&amp;stack_end) + 1);
mark_locations(((char *)stack_start_ptr) + 2,((char *)&amp;stack_end) + 2);
mark_locations(((char *)stack_start_ptr) + 3,((char *)&amp;stack_end) + 3);
```

### Porting to tiny machines

SIOD has been run on 16 bit machines, such as MS-DOS tiny memory
model and in palm tops.

Some things to consider:

- changing the double data type in struct obj into a float.
- changing mark and type to bytes, or bitfields in a byte.
- reducing default sizes of heap, interned numbers, obarray_dim.
- excluding modules such as slibu.c and md5.c
- making sure constant strings are allocated by the linker into read-only data
  sections.

On larger machines the structure member alignment requirements
cause an array of struct obj to waste space. For example, on an ALPHA
with 64 bit pointers each cell takes up 24 bytes, even though
ideally only 18 would be required. Is it worth it to pay a price of
having a 33% overhead in order to buy simplicity? The most common
way of dealing with the alignment problem is to store the object
type and gc_mark some place else. Usually this means making assumptions
about memory addressing because it is important to be able to get the TYPE
as quickly as possible.

There are certainly other was to organize data, avoiding the
use of the C programming _struct_ support, and utilizing
carefully contrived macro definitions instead.

## Writing extensions in the C programming language

The file **siod.h** provides public declarations of C functions
that can be called by extensions. Although some things are missing,
having been put into the **siodp.h** file accidently, and some
may have been left out altogether. Any function available to be called
from Scheme interpreted programs can also be called from C programs,
using the same arguments and with the same return value. Hence
the claim for a small _cognitive-footprint_ imposed on extension
authors.

There are three common reasons for wanting to write an extension to the
system using the C programming language:

1. For runtime efficiency.
2. To take advantage of operating system, or other runtime library provided
   functionality.
3. To play games with evaluator semantics.

Some examples of the first class are the functions memq, and nth,
study them. These extensions are straightforward, and easy to debug from the C
language debugger, with the functions **err0**, **pr**, and
**prp** being provided to call back into the lisp runtime system
from the C debugger.

Some examples from the second class are the **ndbm**
and **regex** modules, and the support for commercial database
client interfaces. In many cases it is convenient to define
new scheme data types to encapsulate the complex state of
an API. Study how to utilize allocate_user_tc,
set_gc_hooks and set_print_hooks. Careful ordering of
storage allocation and interrupt management are important.
Also don't forget that most C programming API functions do not
handle being longjump'd through very well, so beware of how
you handle callbacks and SIGINT.

Functions such as get_c_string, get_c_string_dim, get_c_long,
get_c_double, and get_c_file are usually all you need to get at the
data you require to get the job done. But beware of spoofing the
garbage collector. For example, never do something equivalent to this:

```c
{
  LISP x;
  char *z;
  x = strcons(100,NULL);
  z = get_c_string(z);
  /* no further references to x, but z is used */
 }
```

Because there are no further references to **x**, the C compiler
might very well reuse the location on the stack in which **x** resided.
If there is any other consing then the garbage collector will go off
at some point in the future inside this function, and it will free
the memory pointed to by **z**. A potential example of this
sort of thing is the built-in procedure **lexec**. In theory
a C compiler might store envp and gcsafe in the same memory
location. But of course for other reasons it is impossible for that
to cause problems unless get_c_string was extended to
invoke the evaluator in some cases.

If you want to play with evaluator semantics you need to study
the **leval** function and perhaps the **lapply** function too.
The **tc_fsubr** object is the conventional way to extend an
evaluator, but the **tc_msubr** is more powerfull and allows
for a modular tail recursion. The set_eval_hooks function allows for
arbitrary evalution semantics when the first element of a form
evaluates to a new datatype.

### User Type Extension

If you use them then you must provide some information
to the garbage collector.
To do this you can supply 4 functions,

1. a user_relocate, takes an object and returns a new copy.
2. a user_scan, takes an object and calls relocate on its subparts.
3. a user_mark, takes an object and calls gc_mark on its subparts or
   it may return one of these to avoid stack growth.
4. a user_free, takes an object to hack before it gets onto the freelist.

```c
set_gc_hooks(type,
             user_relocate_fcn,
             user_scan_fcn,
             user_mark_fcn,
             user_free_fcn,
             &amp;kind_of_gc);
```

The variable kind_of_gc should be a long.
It will receive 0 for mark-and-sweep, 1 for
stop-and-copy. Therefore set_gc_hooks should be called AFTER process_cla.
You must specify a relocate function with stop-and-copy. The scan
function may be NULL if your user types will not have lisp objects in them.
Under mark-and-sweep the mark function is required but the free function
may be NULL.

You might also want to extend the printer. This is optional.

```c
set_print_hooks(type,fcn);
```

The fcn receives the object which should be printed to its second
argument, a struct gen_printio \*, using procedures such as gput_st.

## LIBSIOD use as an extension language for C programs

See the modules **sample.c** and **siod.c** for example main
program usage. Once the storage system is initialized the
most simple and useful interface into the interpreter is probably
the **repl_c_string** function. Sample called with argument "xx"
illustrates the most general case of string->string transformation
using repl_c_string. Here is an even smaller example, with only two
procedure calls into the siod shared library, marked in bold font.
The return value of repl_c_string is zero unless an error took
place in either the read, eval, or print portions of execution.
This example wraps a call to \*catch 'errobj around the expression, which
will cause evaluation errors to return a pair of error message string
and the offending object.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "siod.h"

int main(int argc, char **argv)
{
    int j, retval = 0;
    long iobufflen = 1000;
    char *iobuff, *sargv[4];
    sargv[0] = argv[0];
    sargv[1] = "-v0";
    sargv[2] = "-g0";
    sargv[3] = "-h10000:10";

    siod_init(4,sargv);

    iobuff = (char *) malloc(iobufflen);
    retval = 0;
    for (j = 1; j < argc; ++j) {
        sprintf(iobuff, "(*catch 'errobj (begin %s))", argv[j]);
        printf("Evaluating %s, ",argv[j]);
        retval = **repl_c_string\*\*(iobuff,0,0,iobufflen);
        printf("retval = %d\n%s\n",retval,iobuff);
    }

    return(retval);
}
```

## Implementation of EVAL and environment representation

Some procedures take an optional (last) argument which is a pointer to
an environment. Here is a table giving the scheme and C names of
the most important ones:

| C Name        | arguments       | Scheme Name       |
| ------------- | --------------- | ----------------- |
| leval         | form, env       | eval              |
| symbol_value  | sym, env        | symbol-value      |
| symbol_boundp | sym, env        | symbol-bound?     |
| setvar        | sym, value, env | set-symbol-value! |
| envlookup     | sym, env        | env-lookup        |

These procedures are not to be confused with special forms of the
proper Scheme language, which can be effectively compiled and
translated into arbitrary other languages, including but not limited
to machine language. Instead these procedures should be thought of as
hooks into the underlying interpreter implementation.

The most common value to pass for env, especially when used from C
programs is the value NIL, the empty list. This NIL represents the
global, or toplevel environment.

If you go beyond considering the NIL environment then you can
get into areas of the system which are subject to change.
Although not often. With release 1.0 of SIOD in April of 1988
the environment was a pure association list. But with release 1.3
in May of 1988 it was changed to the list of frames as described
in "The Art of the Interpreter." Over the last 9 years that hasn't
changed. For example:

```scheme
> **(%%closure-env (let ((x 3)) (lambda ())))**
(((x) 3))
> **(let ((x 3)) (the-environment))**
(((x) 3))
```

In an environment frame the car is a list of symbols
and the cdr is a list of values. The env-lookup procedure returns
a list such that car can be used to obtain the value, and
set-car! can be used to assign the value.

```scheme
> **(env-lookup 'x '( (  (a b x c d)  1 2 3 4 5 ) ))**
(3 4 5)
```

The env-lookup does not work on the global environment. This could
be considered an architecture bug. The global environment is represented
by actual slots in the symbol structure, rather then as entries in
some general frame representation. If SIOD had a "locative" data
type then env-lookup might well return that. But either way there is
a dicotomy between local and global environment representation which is
usually considered to be a bad thing, even though it is a classic
implementation technique.

Possibly just-as-good in practice would be to allow an environment
frame to be an efficient **test=EQ?** hash-table.

A future direction to take in SIOD is most likely to involve
embracing operating-system-specific environment representations,
when appropriate, especially those having to do with underlying
library and dynamic linking implementation.

## Windows NT and Windows 95 Configuration

Files written in scheme may use the SIOD.EXE command/console module
application by creating appropriate registry entries.
For example, say that the file extension is **.SMD**:

To enable usage from within Microsoft Internet Information Server,
the registry key is HKEY_LOCAL_MACHINE, SYSTEM, CurrentControlSet,
Services, W3SVC, Parameters, Script Map. Create a new string
value:

- name: .smd
- data: c:\siod\siod.exe -v0,-m3 %s

To enable usage from the Command Prompt (Windows NT only)
or the Windows GUI, it is easiest to use the **File Types** tab
you get by viewing options of **My Computer**. You will
want to create a new type with associated file extension **SMD**:

- action: open
- application: c:\siod\siod.exe -v01,-m2

Note the different level of main program and verbosity between
web server and command usage. This is recommended.

The siod.mak file is used with Microsoft Visual C++ 4.0
development environment.

Executable files may also be created. See the winsiod.html
support document.

## Unix configuration

- In all versions beware that LD_LIBRARY_PATH must
  be set to include the current directory "." first
  if the development libsiod is to be found first. Otherwise
  rebuilding it will have no effect at runtime.
- In OSF1 everything works without a glitch when
  the default installation targets are chosen.
- In Solaris I found that I had to make a soft link
  from /usr/lib/libsiod.so to /usr/local/lib/libsiod.so.
  The diagnostic ldd -s /usr/local/bin/siod, shows that
  the default lib is only /usr/lib. Make a note to look into
  setting RPATH in the LD. Setting flag -R /usr/local/lib/siod,
  would also help remove a kludge from load_so in the slibu.c file.
- In Linux you must run the ldconfig command after
  installing siod. Try **ldconfig -v**.

## References

- _Structure and Interpretation of Computer Programs_, by
  [Abelson](http://www-swiss.ai.mit.edu/~hal/hal.html) and
  [Sussman](http://www-swiss.ai.mit.edu/~gjs/gjs.html),
  [MIT Press](http://www-mitpress.mit.edu/).
- [scheme repository at indiana.edu](http://www.cs.indiana.edu/scheme-repository/).
- [scheme repository at cmu.edu](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/scheme/0.html).

## Contributors

The following people have contributed bug fixes or other additions
to siod.

- Paul Stodghill
- Bob Bane
- Barak Pearlmutter
- Craig Denson
- Philip G Wilson
- Leo Harten
- Philippe Laliberte
- andreasg

## Acknowledgements

This software contains code derived from the RSA Data Security Inc. MD5
Message-Digest Algorithm.

This winsiod precompiled version of SIOD
package contains software written and copyrighted by Henry Spencer.
See [hs_regex.html](http://alum.mit.edu/www/gjc/hs_regex.html).
