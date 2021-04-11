# Windows NT/95 Binary Distribution of SIOD

**NEW!**
A wicked good port of siod to the Windows CE environment.
See Ben Goetter's <http://www.angrygraycat.com/scheme/pscheme.htm">, which goes
by the name of _pocket scheme_.
Other mind-blowing Windows work, Brian Beckman's
full-metal-jacket windows native function interface.
See <http://www.angelfire.com/wa/brianbec/siodffi.html>.
At the end of this file, my notes on building crude
[windows-installers](<(#making-a-crude-setupexe)>).

See also [SIOD for MS-DOS](http://alum.mit.edu/www/gjc/minisiod.html).

Skip to: [installation](#installation),
[support](#support), or [sources](#sources).

This distribution of the [SIOD](http://alum.mit.edu/www/gjc/siod.html) Scheme
dialect of lisp language interpreter you may find useful for a number
of reasons, including:

- The CSIOD command, used to convert scheme script files into stand-alone
  executables.
- Writing WEB/CGI and other scripts, either directly or through the
  [Chunks of HTML](http://alum.mit.edu/www/gjc/chtml.html) technique.
- The binaries are currently available for the INTEL platform of Windows,
  compiled using the Microsoft C++ 2005 Express Edition.
- The highly-compact [sources](#sources) are freely
  [available](./readme.md), and can be
  rebuilt in a few dozen seconds using off-the-shelf development tools.
- Small runtime footprint. SIOD.EXE is 16k bytes, LIBSIOD.DLL
  is 184k bytes. If you need binaries for other platforms or
  optimized binaries, or if you have produced them, please contact
  the author. [Note: the previous figures must have been for
  Windows NT/95. Since then SIOD has not changed much, but
  compiler output has changed considerably. The DLL is now about 300k,
  and the SIOD.EXE is 49k. There is now a lot more padding and
  other information, even in a release executable].
- Documentation in html format.
- Efficient file I/O and string operations, including
  [regular expression](http://alum.mit.edu/www/gjc/winregex.html) procedures.
- tcp socket interface, including sample http server,
  and useful ftp, http, and email client commands.
- MD5 checksum procedure, for digital signature work,
  and filesystem snapshot and comparision
  utilities.
- dynamic-linked extensions.
- LIBSIOD.DLL entry points using WINAPI can be called from Winbatch,
  Visual Basic, and other environments.
- Algebraic/infix syntax parser for user-visible interface work.
- fast binary object save/restore procedures.
- Optional (compile them yourself) interfaces to Sybase, Oracle, RDB, mSQL,
  relational databases. (No ODBC yet, contact me for info).
- The entire distribution fits on a WIN95 boot floppy with plenty of room to
  spare. (Still true, even in 2007).

## Installation

1. Download a release, e.g. SIOD-5-6.MSI from <http://www.codeplex.com/siod>
2. The MSI is not digitally signed, so it may warn you about the "publisher"
   being unknown.
3. Install a typical release brings in binaries and some documentation into the
   %PROGRAMFILES%\SIOD folder. Installing everything gives you sources that you
   can rebuild everything from, including the installer.

## Getting Started

At this point you may make use of your new tool. The file siod.txt
has information about the command lines switches. Use Wordpad to
view the file, because it still has the unix-style end-of-line convention.
Other txt files contain documentation on particular commands (smd) files.
Use your web browser to view the documentation in siod.html.

Note the m and v flags to csiod. If you were going to create a cgi
script, then you would use a different default "main" and "verbosity"
level, such as:

```shell
> csiod :o=cgi-echo.exe :m=3 :v=0 cgi-echo.scm
```

For examples of CGI scripts see [chtml.html](http://alum.mit.edu/www/gjc/chtml.html).

Enjoy!

--- George J. Carrette.

## Support

The internet news group [comp.lang.scheme](news:comp.lang.scheme)
is a good source of expert advise on many scheme related topics.
In addition to responding to email the author answers questions posted
in the news group. Any expert C programmer, even if they have no knowledge
of scheme in particular, should be able to help out at the interpreter
and system-interface levels of SIOD after reading the sections on
storage management and extensibility in the online manual.

See [siodusers](http://alum.mit.edu/www/gjc/siodusers.html) for info on the siod
mailing list and some network references to users of siod. (About 10 years old).

See
[Scheme FAQ's](http://www.cs.cmu.edu/Groups/AI/html/faqs/lang/scheme/top.html)
for more reference material.

## Standard Documentation

SIOD doesn't adhere to all of what has become to be
considered the standard functions of Scheme. If it did, or when it does,
these standard manual references would be even more useful.
The Revised Report in
[html format](http://swissnet.ai.mit.edu/~jaffer/r4rs_2.html)
or [Postscript](http://www-swiss.ai.mit.edu/ftpdir/scheme-reports/r4rs.ps).

From time to time I push the thing to include more standard functions
without increasing the runtime footprint very much.
Beware that a high quality implementation of the full numerical
tower of types and functions would double the size of the base
system. Until some standard C or C++ library can be utilized
instead of course.

## Copyright

The copyright notice in the source file slib.c is very generous in
what it grants to those who have obtained the source code.
Commercial products have been introduced using SIOD without any
need to contact the author. For any application that has a **help**
command all I ask is that the **Help/About** command or menu item
display this phrase along with your own notices:

> Portions of this software are based in part on the
> <http://alum.mit.edu/www/gjc/siod.html> work of George J. Carrette.

Command line utilities with a -help option of some kind should
also display the above phrase. Online and printed documentation
should also contain that phrase in the obvious place along with
similar notices.

The short notification phrase is sufficient and better than
the longer notice suggested in the source code itself.

## Differences from the unix version of siod

1. (so-ext) is .dll of course. For portability between operating systems in
   command files always say (require-so (so-ext "file")).
2. The ss.dll includes (wsa-data) procedure, but no (gethostid).
3. The (alarm) is not available. A simple version could use a thread to set the
   CONTROL-C interrupt flag after sleeping. But the main alarm feature in unix
   is to interrupt system calls, something which is done quite differently in
   Windows NT (or just about any other O/S besides Unix, for that matter).
4. CONTROL-C won't interrupt console mode input, but will interrupt output and
   running programs.
5. LIBSIOD.DLL is not thread safe. It isn't in Unix either, but threads are
   rarely used in unix applications up until very recently, so it hardly
   mattered in that environment.
6. (siod-lib) defaults to the directory containing siod.exe, or the CSIOD
   compiled executable, or registry defined location, unless the SIOD_LIB
   environment value was defined, or is over-ridden by the -i command line
   argument to siod.exe or encorporated in executables produced using csiod
   with the :i argument. Is that clear? The best thing to do is to set the
   registry when you install siod on your system.

## Sources

You can download either a source bundle from the
[codeplex](http://www.codeplex.com/siod) site,
or download a release installer MSI and use the sources included
with the release.

To compile you can use a full Visual Studio 2005 edition, or
[Visual C++ 2005 Express Edition](http://msdn.microsoft.com/vstudio/express/visualc/).
To build the installer you will need
[Windows Installer XML](http://wix.sourceforge.net/), version 2.0

The file BUILD.BAT invokes the VCBUILD program and also builds
the installer, assuming all the tools have been installed in the standard
places.

## Making a crude setup.exe

This example is obviously quite obsolete given the
power and ease of use of Windows Installer XML. But recall that it
was based on technology for windows NT version 4.

This example provides a subset of the standard siod distribution
files, if needed, establishes the required windows registry entries
for SIOD, and creates a new file type SSF, which with a OPEN action to
invoke siod, and also an EDIT action. It provides some additional
DLL's and a sample script and readme file.

If SIOD is already installed then it skips the siod registration.

You should be able to use this as a template for your own applications.

```scheme
;;-*-mode:lisp-*-

;; name:    setup.scm
;; purpose: source for SETUP.EXE for installation of SIOD subset
;;          along with ECI script extension file.
;; created: 28-JUL-1999 George J. Carrette LITTON/TASC
;;
;; note:    The commercial product installshield can do this much prettier.
;;          As can a custom setup.exe created using visual C++ or
;;          visual basic.

(define *siod-dist-files*
  '("siod.exe"
    "csiod.exe"
    "wcsiod.exe"
    "slibw32.dll"
    "libsiod.dll"))

(define *eci-dist-files*
  '("epshr02.dll"
    "eciclt32.dll"))

(define *siod-eci-dist-files*
  '("siod_eci.dll"
    "ecisample.ssf"
    "readme.txt"))

(define (fatal-setup-error . rest)
  (apply writes (cons nil rest))
  (error "fatal setup error"))

(define (main)
  (let ((result (*catch 'errobj (cons 1 (main-1)) nil)))
    (writes nil "Press Enter to exit ...")
    (readline)))

(define *key* "SOFTWARE\\George Carrette\\SIOD")
(define *dir* "C:\\SIOD")
(define *ftype* "SSF")

(define (main-1)
  (writes nil
      "*****************************************\n"
      "* ECI SIOD scripting language installer *\n"
      "* See README.TXT for more information   *\n"
      "*****************************************\n"
      "\n")
  (require-so (so-ext 'slibw32))
  (let ((inplace-siod-lib (siod-lib))
    (key *key*)
    (root "HKEY_LOCAL_MACHINE")
    (value "SIOD_LIB")
    (registered-siod-lib nil)
    (new-siod-lib nil))
    (*catch 'errobj
        (set! registered-siod-lib (registry-ref root key value)))
    (writes nil "\n")
    (cond ((equal? inplace-siod-lib registered-siod-lib)
       ;; if this setup.scm was compiled with :b=siod.exe
       ;; then this can happen.
       (fatal-setup-error "Improperly built SETUP.EXE\n")))
    (writes nil
        "SETUP DIR              = " inplace-siod-lib "\n")
    (cond (registered-siod-lib
       (writes nil
           "Installed SIOD LIB DIR = " registered-siod-lib "\n\n")
       (cond ((y-or-n-p
           (string-append "copy ECI files to "
                  registered-siod-lib " directory?"))
          (install-files (append *eci-dist-files*
                     *siod-eci-dist-files*)
                 inplace-siod-lib
                 registered-siod-lib))
         ('else
          (fatal-setup-error "user chose not to install files\n"))))
      ((set! new-siod-lib
         (get-user-input "Installation Destination directory"
                 *dir*))
       (if (not (substring-equal? "\\"
                      new-siod-lib
                      (- (length new-siod-lib) 1)
                      (length new-siod-lib)))
           (set! new-siod-lib (string-append new-siod-lib
                         "\\")))
       (or (y-or-n-p
        (string-append "copy ECI files to "
                   new-siod-lib " directory?"))
           (fatal-setup-error "user chose not to install files\n"))
       (create-directory new-siod-lib)
       (install-files (append *eci-dist-files*
                  *siod-eci-dist-files*
                  *siod-dist-files*)
              inplace-siod-lib
              new-siod-lib)
       (registry-set root key value new-siod-lib))
      ('else
       (fatal-setup-error "not implemented")))
    (cond ((y-or-n-p (string-append
              "Register file extension "
              *ftype* " ? "))
       (register-siod-ftype (registry-ref root key value)))))
  (writes nil
      "********\n"
      "* DONE *\n"
      "********\n"
      "\n"))

(define (register-siod-ftype dir)
  (registry-set "HKEY_CLASSES_ROOT"
        (string-append "." *ftype*)
        ""
        (string-append *ftype* "_FILE"))
  (registry-set "HKEY_CLASSES_ROOT"
        (string-append *ftype* "_FILE")
        ""
        "SIOD script file")
  (registry-set "HKEY_CLASSES_ROOT"
        (string-append *ftype* "_FILE"
                   "\\Shell\\open\\command")
        ""
        (string-append "\""
                   dir
                   "siod.exe\""
                   "-v01,-m2 \"%1\" %*"))
  (registry-set "HKEY_CLASSES_ROOT"
        (string-append *ftype* "_FILE"
                   "\\Shell\\Edit\\command")
        ""
    ;; we should actually get this value from the key
    ;; WordPad.Document.1\\Shell\Open\command.
      ;; or wrifile. (for .wri).
        (string-append "\""
                   (or (getenv "sysmtedrive") "C:")
                   "\\Program Files\\Windows NT\\Accessories\\"
                   "WORDPAD.EXE"
                   "\""
                   " \"%1\"")))

(define (y-or-n-p . msg)
  (let ((result nil)
    (return nil)
    (in nil))
    (while (not return)
      (apply writes (cons nil msg))
      (set! in (readline))
      (cond ((not in) (fatal-setup-error "no input\n"))
        ((member (set! in (string-downcase (string-trim in)))
             '("y" "yes"))
         (set! return t)
         (set! result t))
        ((member in '("n" "no"))
         (set! return t)
         (set! result nil))
        ('else
         (writes nil "Please reply yes or no.\n"))))
    result))


(define (get-user-input msg default)
  (let ((result nil)
    (return nil)
    (in nil))
    (while (not return)
      (writes nil msg " [" default "]: ")
      (set! in (readline))
      (cond ((not in) (fatal-setup-error "no input\n"))
        ((equal? in "")
         (set! return t)
         (set! result default))
        ('else
         (set! return in)
         (set! result nil))))
    result))



(define (install-files l from into)
  (mapcar (lambda (x)
        (copy-one-file (string-append from x)
               (string-append into x)))
      l))

(define (copy-one-file from to)
  (let ((buff (cons-array 4096 'byte))
    (in (fopen from "rb"))
    (out (fopen to "wb"))
    (nbytes nil))
    (writes nil from " -&gt; " to "\n")
    (while (set! nbytes (fread buff in))
      (fwrite (if (= nbytes (length buff)) buff (list buff nbytes)) out))
    (fclose in)
    (fclose out)))

(define (create-directory name)
  (let ((old (getcwd)))
    (chdir (getenv "WINDIR"))
    ;; chdir prevents warning if cwd is an UNC
    (system (string-append "mkdir \"" name "\""))
    (chdir old)))
```
