;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;
;;; Some support stuff for compiling and loading PCL.  It would be nice if
;;; there was some portable make-system we could all agree to share for a
;;; while.  At least until people really get databases and stuff.
;;;
;;; ***                                                               ***
;;; ***        DIRECTIONS FOR INSTALLING PCL AT YOUR SITE             ***
;;; ***                                                               ***
;;;
;;; To get PCL working at your site you should:
;;; 
;;;  - Get all the PCL source files from Xerox.  The complete list of source
;;;    file names can be found in the defsystem for PCL which appears towards
;;;    the end of this file.
;;; 
;;;  - Edit the variable *pcl-directory* below to specify the directory at
;;;    your site where the pcl sources and binaries will be.  This variable
;;;    can be found by searching from this point for the string "***" in
;;;    this file.
;;; 
;;;  - Use the function (pcl::compile-pcl) to compile PCL for your site.
;;; 
;;;  - Once PCL has been compiled it can be loaded with (pcl::load-pcl).
;;;    Note that PCL cannot be loaded on top of itself, nor can it be
;;;    loaded into the same world it was compiled in.
;;;

(in-package :user)

#+excl
(eval-when (compile load eval)
  (when (eq (find-package :lisp) (find-package :common-lisp))
    (let ((excl::*enable-package-locked-errors* nil))
      (rename-package :common-lisp :common-lisp '(:cl)))
    (make-package :lisp :use nil)
    ((lambda (symbols)
       (import symbols :lisp)
       (export symbols :lisp))
     '(&allow-other-keys &aux &body &environment &key &optional &rest &whole
       * ** *** *applyhook* *break-on-warnings* *debug-io*
       *default-pathname-defaults* *error-output* *evalhook* *features*
       *load-verbose* *macroexpand-hook* *modules* *package* *print-array*
       *print-base* *print-case* *print-circle* *print-escape*
       *print-gensym* *print-length* *print-level* *print-pretty*
       *print-radix* *query-io* *random-state* *read-base*
       *read-default-float-format* *read-suppress* *readtable*
       *standard-input* *standard-output* *terminal-io* *trace-output* +
       ++ +++ - / // /// /= 1+ 1- < <= = > >= abs acons acos acosh adjoin
       adjust-array adjustable-array-p akcl alpha-char-p alphanumericp and
       append apply applyhook apropos apropos-list aref array
       array-dimension array-dimension-limit array-dimensions
       array-element-type array-has-fill-pointer-p array-in-bounds-p
       array-rank array-rank-limit array-row-major-index array-total-size
       array-total-size-limit arrayp ash asin asinh assert assoc assoc-if
       assoc-if-not atan atanh atom bignum bit bit-and bit-andc1 bit-andc2
       bit-eqv bit-ior bit-nand bit-nor bit-not bit-orc1 bit-orc2
       bit-vector bit-vector-p bit-xor block boole boole-1 boole-2
       boole-and boole-andc1 boole-andc2 boole-c1 boole-c2 boole-clr
       boole-eqv boole-ior boole-nand boole-nor boole-orc1 boole-orc2
       boole-set boole-xor both-case-p boundp break butlast byte
       byte-position byte-size caaaar caaadr caaar caadar caaddr caadr
       caar cadaar cadadr cadar caddar cadddr caddr cadr
       call-arguments-limit car case catch ccase cdaaar cdaadr cdaar
       cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr
       cddr cdr ceiling cerror char char-bit char-bits char-bits-limit
       char-code char-code-limit char-control-bit char-downcase char-equal
       char-font char-font-limit char-greaterp char-hyper-bit char-int
       char-lessp char-meta-bit char-name char-not-equal char-not-greaterp
       char-not-lessp char-super-bit char-upcase char/= char< char<= char=
       char> char>= character characterp check-type cis clear-input
       clear-output close clrhash code-char coerce common commonp
       compilation-speed compile compile-file compiled-function
       compiled-function-p compiler-let complex complexp concatenate cond
       conjugate cons consp constantp copy-alist copy-list copy-readtable
       copy-seq copy-symbol copy-tree cos cosh count count-if count-if-not
       ctypecase decf declaration declare decode-float
       decode-universal-time defconstant define-modify-macro
       define-setf-method defmacro defparameter defsetf defstruct deftype
       defun defvar delete delete-duplicates delete-file delete-if
       delete-if-not denominator deposit-field describe digit-char
       digit-char-p directory directory-namestring disassemble do do*
       do-all-symbols do-external-symbols do-symbols documentation dolist
       dotimes double-float double-float-epsilon
       double-float-negative-epsilon dpb dribble ecase ed eighth elt
       encode-universal-time endp enough-namestring eq eql equal equalp
       error etypecase eval eval-when evalhook evenp every exp export expt
       fboundp fceiling ffloor fifth file-author file-length
       file-namestring file-position file-write-date fill fill-pointer
       find find-all-symbols find-if find-if-not find-package find-symbol
       finish-output first fixnum flet float float-digits float-precision
       float-radix float-sign floatp floor fmakunbound force-output format
       fourth fresh-line fround ftruncate ftype funcall function functionp
       gcd gensym gentemp get get-decoded-time
       get-dispatch-macro-character get-internal-real-time
       get-internal-run-time get-macro-character get-output-stream-string
       get-properties get-setf-method get-setf-method-multiple-value
       get-universal-time getf gethash gfun go graphic-char-p hash-table
       hash-table-count hash-table-p host-namestring identity if ignore
       imagpart import in-package incf inline input-stream-p inspect
       int-char integer integer-decode-float integer-length integerp
       intern internal-time-units-per-second intersection isqrt keyword
       keywordp labels lambda lambda-list-keywords lambda-parameters-limit
       last lcm ldb ldb-test ldiff least-negative-double-float
       least-negative-long-float least-negative-short-float
       least-negative-single-float least-positive-double-float
       least-positive-long-float least-positive-short-float
       least-positive-single-float length let let*
       lisp-implementation-type lisp-implementation-version list list*
       list-all-packages list-length listen listp load locally log logand
       logandc1 logandc2 logbitp logcount logeqv logior lognand lognor
       lognot logorc1 logorc2 logtest logxor long-float long-float-epsilon
       long-float-negative-epsilon long-site-name loop lower-case-p
       machine-instance machine-type machine-version macro-function
       macroexpand macroexpand-1 macrolet make-array make-broadcast-stream
       make-char make-concatenated-stream make-dispatch-macro-character
       make-echo-stream make-hash-table make-list make-package
       make-pathname make-random-state make-sequence make-string
       make-string-input-stream make-string-output-stream make-symbol
       make-synonym-stream make-two-way-stream makunbound map mapc mapcan
       mapcar mapcon maphash mapl maplist mask-field max member member-if
       member-if-not merge merge-pathnames min minusp mips mismatch mod
       most-negative-double-float most-negative-fixnum
       most-negative-long-float most-negative-short-float
       most-negative-single-float most-positive-double-float
       most-positive-fixnum most-positive-long-float
       most-positive-short-float most-positive-single-float
       multiple-value-bind multiple-value-call multiple-value-list
       multiple-value-prog1 multiple-value-setq multiple-values-limit
       name-char namestring nbutlast nconc nil nintersection ninth not
       notany notevery notinline nreconc nreverse nset-difference
       nset-exclusive-or nstring-capitalize nstring-downcase
       nstring-upcase nsublis nsubst nsubst-if nsubst-if-not nsubstitute
       nsubstitute-if nsubstitute-if-not nth nthcdr null number numberp
       numerator nunion oddp open optimize or otherwise output-stream-p
       package package-name package-nicknames package-shadowing-symbols
       package-use-list package-used-by-list packagep pairlis
       parse-integer parse-namestring pathname pathname-device
       pathname-directory pathname-host pathname-name pathname-type
       pathname-version pathnamep peek-char phase pi plusp pop position
       position-if position-if-not pprint prin1 prin1-to-string princ
       princ-to-string print probe-file proclaim prog prog* prog1 prog2
       progn progv provide psetf psetq push pushnew quote random
       random-state random-state-p rassoc rassoc-if rassoc-if-not ratio
       rational rationalize rationalp read read-byte read-char
       read-char-no-hang read-delimited-list read-from-string read-line
       read-preserving-whitespace readtable readtablep realpart reduce rem
       remf remhash remove remove-duplicates remove-if remove-if-not
       remprop rename-file rename-package replace require rest return
       return-from revappend reverse room rotatef round rplaca rplacd
       safety satisfies sbit scale-float schar search second sequence set
       set-char-bit set-difference set-dispatch-macro-character
       set-exclusive-or set-macro-character set-syntax-from-char setf setq
       seventh sfun shadow shadowing-import shiftf short-float
       short-float-epsilon short-float-negative-epsilon short-site-name
       signed-byte signum simple-array simple-bit-vector
       simple-bit-vector-p simple-string simple-string-p simple-vector
       simple-vector-p sin single-float single-float-epsilon
       single-float-negative-epsilon sinh sixth sleep software-type
       software-version some sort space special special-form-p speed sqrt
       stable-sort standard-char standard-char-p step stream
       stream-element-type streamp string string-capitalize string-char
       string-char-p string-downcase string-equal string-greaterp
       string-left-trim string-lessp string-not-equal string-not-greaterp
       string-not-lessp string-right-trim string-trim string-upcase
       string/= string< string<= string= string> string>= stringp
       structure sublis subseq subsetp subst subst-if subst-if-not
       substitute substitute-if substitute-if-not subtypep svref sxhash
       symbol symbol-function symbol-name symbol-package symbol-plist
       symbol-value symbolp t tagbody tailp tan tanh tenth terpri the
       third throw time trace tree-equal truename truncate type type-of
       typecase typep unexport unintern union unless unread-char
       unsigned-byte untrace unuse-package unwind-protect upper-case-p
       use-package user-homedir-pathname values values-list variable
       vector vector-pop vector-push vector-push-extend vectorp warn when
       with-input-from-string with-open-file with-open-stream
       with-output-to-string write write-byte write-char write-line
       write-string write-to-string y-or-n-p yes-or-no-p zerop))))

(in-package :walker :use '(:lisp))
(in-package :iterate :use '(:lisp :walker))
(in-package :pcl :use '(:walker :iterate :lisp))

(export (intern (symbol-name :iterate)		;Have to do this here,
		(find-package :iterate))	;because in the defsystem
	(find-package :iterate))		;(later in this file)
						;we use the symbol iterate
						;to name the file

;;;
;;; Sure, its weird for this to be here, but in order to follow the rules
;;; about order of export and all that stuff, we can't put it in PKG before
;;; we want to use it.
;;; 
(defvar *the-pcl-package* (find-package :pcl))

(defvar *pcl-system-date* "September 16 92 PCL (f)")

#+cmu
(setf (getf ext:*herald-items* :pcl)
      `("    CLOS based on PCL version:  " ,*pcl-system-date*))

;;;
;;; Various hacks to get people's *features* into better shape.
;;; 
(eval-when (compile load eval)
  #+(and Symbolics Lispm)
  (multiple-value-bind (major minor) (sct:get-release-version)
    (etypecase minor
      (integer)
      (string (setf minor (parse-integer minor :junk-allowed t))))
    (pushnew :genera *features*)
    (ecase major
      ((6)
       (pushnew :genera-release-6 *features*))
      ((7)
       (pushnew :genera-release-7 *features*)
       (pushnew :copy-&rest-arg *features*)
       (ecase minor
	 ((0 1) (pushnew :genera-release-7-1 *features*))
	 ((2)   (pushnew :genera-release-7-2  *features*))
	 ((3)   (pushnew :genera-release-7-3  *features*))
	 ((4)   (pushnew :genera-release-7-4  *features*))))
      ((8)
       (pushnew :genera-release-8 *features*)
       (ecase minor
	 ((0) (pushnew :genera-release-8-0 *features*))
	 ((1) (pushnew :genera-release-8-1 *features*))))))
  
  #+CLOE-Runtime
  (let ((version (lisp-implementation-version)))
    (when (string-equal version "2.0" :end1 (min 3 (length version)))
      (pushnew :cloe-release-2 *features*)))

  (dolist (feature *features*)
    (when (and (symbolp feature)                ;3600!!
               (equal (symbol-name feature) "CMU"))
      (pushnew :CMU *features*)))
  
  #+TI
  (if (eq (si:local-binary-file-type) :xld)
      (pushnew ':ti-release-3 *features*)
      (pushnew ':ti-release-2 *features*))

  #+Lucid
  (when (search "IBM RT PC" (machine-type))
    (pushnew :ibm-rt-pc *features*))

  #+ExCL
  (cond ((search "sun3" (lisp-implementation-version))
	 (push :sun3 *features*))
	((search "sun4" (lisp-implementation-version))
	 (push :sun4 *features*)))

  #+(and HP Lucid)
  (push :HP-Lucid *features*)
  #+(and HP (not Lucid) (not excl))
  (push :HP-HPLabs *features*)

  #+Xerox
  (case il:makesysname
    (:lyric (push :Xerox-Lyric *features*))
    (otherwise (push :Xerox-Medley *features*)))
;;;
;;; For KCL and IBCL, push the symbol :turbo-closure on the list *features*
;;; if you have installed turbo-closure patch.  See the file kcl-mods.text
;;; for details.
;;;
;;; The xkcl version of KCL has this fixed already.
;;; 

  #+xkcl(pushnew :turbo-closure *features*)

  )

#+(and excl sun4)
(eval-when (eval compile load)
  (pushnew :excl-sun4 *features*))

#+cmu
(eval-when (compile load eval)
  (let ((vs (lisp-implementation-version)))
    (cond ((and (<= 2 (length vs))
		(eql #\1 (aref vs 0))
		(let ((d (digit-char-p (aref vs 1))))
		  (and d (<= 6 d))))
	   (pushnew :cmu16 *features*))
	  ((string= "8-Dec-1992" vs)
	   (pushnew :cmu16 *features*)
	   (pushnew :cmu17 *features*)
	   (when (fboundp 'pcl::original-defstruct)
	     (setf (macro-function 'defstruct)
		   (macro-function 'pcl::original-defstruct))))
	  (t
	   (error "what version of cmucl is this?")))))

;;; Yet Another Sort Of General System Facility and friends.
;;;
;;; The entry points are defsystem and operate-on-system.  defsystem is used
;;; to define a new system and the files with their load/compile constraints.
;;; Operate-on-system is used to operate on a system defined that has been
;;; defined by defsystem.  For example:
#||

(defsystem my-very-own-system
	   "/usr/myname/lisp/"
  ((classes   (precom)           ()                ())
   (methods   (precom classes)   (classes)         ())
   (precom    ()                 (classes methods) (classes methods))))

This defsystem should be read as follows:

* Define a system named MY-VERY-OWN-SYSTEM, the sources and binaries
  should be in the directory "/usr/me/lisp/".  There are three files
  in the system, there are named classes, methods and precom.  (The
  extension the filenames have depends on the lisp you are running in.)
  
* For the first file, classes, the (precom) in the line means that
  the file precom should be loaded before this file is loaded.  The
  first () means that no other files need to be loaded before this
  file is compiled.  The second () means that changes in other files
  don't force this file to be recompiled.

* For the second file, methods, the (precom classes) means that both
  of the files precom and classes must be loaded before this file
  can be loaded.  The (classes) means that the file classes must be
  loaded before this file can be compiled.  The () means that changes
  in other files don't force this file to be recompiled.

* For the third file, precom, the first () means that no other files
  need to be loaded before this file is loaded.  The first use of
  (classes methods)  means that both classes and methods must be
  loaded before this file can be compiled.  The second use of (classes
  methods) mean that whenever either classes or methods changes precom
  must be recompiled.

Then you can compile your system with:

 (operate-on-system 'my-very-own-system :compile)

and load your system with:

 (operate-on-system 'my-very-own-system :load)

||#

;;; 
(defvar *system-directory*)

;;;
;;; *port* is a list of symbols (in the PCL package) which represent the
;;; Common Lisp in which we are now running.  Many of the facilities in
;;; defsys use the value of *port* rather than #+ and #- to conditionalize
;;; the way they work.
;;; 
(defvar *port*
        '(#+Genera               Genera
;         #+Genera-Release-6     Rel-6
;         #+Genera-Release-7-1   Rel-7
          #+Genera-Release-7-2   Rel-7
	  #+Genera-Release-7-3   Rel-7
          #+Genera-Release-7-1   Rel-7-1
          #+Genera-Release-7-2   Rel-7-2
	  #+Genera-Release-7-3   Rel-7-2	;OK for now
	  #+Genera-Release-7-4   Rel-7-2	;OK for now
	  #+Genera-Release-8	 Rel-8
	  #+imach                Ivory
	  #+Cloe-Runtime	 Cloe
          #+Lucid                Lucid
          #+Xerox                Xerox
	  #+Xerox-Lyric          Xerox-Lyric
	  #+Xerox-Medley         Xerox-Medley
          #+TI                   TI
          #+(and dec vax common) Vaxlisp
          #+KCL                  KCL
          #+IBCL                 IBCL
          #+excl                 excl
	  #+(and excl sun4)      excl-sun4
          #+:CMU                 CMU
          #+HP-HPLabs            HP-HPLabs
          #+:gclisp              gclisp
          #+pyramid              pyramid
          #+:coral               coral))

;;;
;;; When you get a copy of PCL (by tape or by FTP), the sources files will
;;; have extensions of ".lisp" in particular, this file will be defsys.lisp.
;;; The preferred way to install pcl is to rename these files to have the
;;; extension which your lisp likes to use for its files.  Alternately, it
;;; is possible not to rename the files.  If the files are not renamed to
;;; the proper convention, the second line of the following defvar should
;;; be changed to:
;;;     (let ((files-renamed-p nil)
;;;
;;; Note: Something people installing PCL on a machine running Unix
;;;       might find useful.  If you want to change the extensions
;;;       of the source files from ".lisp" to ".lsp", *all* you have
;;;       to do is the following:
;;;
;;;       % foreach i (*.lisp)
;;;       ? mv $i $i:r.lsp
;;;       ? end
;;;       %
;;;
;;;       I am sure that a lot of people already know that, and some
;;;       Unix hackers may say, "jeez who doesn't know that".  Those
;;;       same Unix hackers are invited to fix mv so that I can type
;;;       "mv *.lisp *.lsp".
;;;
(defvar *default-pathname-extensions*
  (car '(#+(and (not imach) genera)          ("lisp"  . "bin")
	 #+(and imach genera)                ("lisp"  . "ibin")
	 #+Cloe-Runtime                      ("l"     . "fasl")
	 #+(and dec common vax (not ultrix)) ("LSP"   . "FAS")
	 #+(and dec common vax ultrix)       ("lsp"   . "fas")
	 #+KCL                               ("lsp"   . "o")
	 #+IBCL                              ("lsp"   . "o")
	 #+Xerox                             ("lisp"  . "dfasl")
	 #+(and Lucid MC68000)               ("lisp"  . "lbin")
	 #+(and Lucid VAX)                   ("lisp"  . "vbin")
	 #+(and Lucid Prime)                 ("lisp"  . "pbin")
	 #+(and Lucid SUNRise)               ("lisp"  . "sbin")
	 #+(and Lucid SPARC)                 ("lisp"  . "sbin")
	 #+(and Lucid IBM-RT-PC)             ("lisp"  . "bbin")
	 #+(and Lucid MIPS)                  ("lisp"  . "mbin")
	 #+(and Lucid PRISM)                 ("lisp"  . "abin")
	 #+(and Lucid PA)                    ("lisp"  . "hbin")
	 #+(and excl SPARC)                  ("cl"    . "sparc")
	 #+(and excl m68k)                   ("cl"    . "m68k")
	 #+excl                              ("cl"    . "fasl")
         #+cmu ("lisp" . #.(c:backend-fasl-file-type c:*backend*))
	 #+HP-HPLabs                         ("l"     . "b")
	 #+TI ("lisp" . #.(string (si::local-binary-file-type)))
	 #+:gclisp                           ("LSP"   . "F2S")
	 #+pyramid                           ("clisp" . "o")
	 #+:coral                            ("lisp"  . "fasl")
	 #-(or symbolics (and dec common vax) KCL IBCL Xerox 
	       lucid excl :CMU HP TI :gclisp pyramid coral)
	                                     ("lisp"  . "lbin"))))

(defvar *pathname-extensions*
  (let* ((files-renamed-p t)
	 (proper-extensions *default-pathname-extensions*))
    (cond ((null proper-extensions) '("l" . "lbin"))
          ((null files-renamed-p) (cons "lisp" (cdr proper-extensions)))
          (t proper-extensions))))

(eval-when (compile load eval)

(defun get-system (name)
  (get name 'system-definition))

(defun set-system (name new-value)
  (setf (get name 'system-definition) new-value))

(defmacro defsystem (name directory files)
  `(set-system ',name (list #'(lambda () ,directory)
			    (make-modules ',files)
			    ',(mapcar #'car files))))

)


;;;
;;; The internal datastructure used when operating on a system.
;;; 
(defstruct (module (:constructor make-module (name))
                   (:print-function
                     (lambda (m s d)
                       (declare (ignore d))
                       (format s "#<Module ~A>" (module-name m)))))
  name
  load-env
  comp-env
  recomp-reasons)

(defun make-modules (system-description)
  (let ((modules ()))
    (labels ((get-module (name)
               (or (find name modules :key #'module-name)
                   (progn (setq modules (cons (make-module name) modules))
                          (car modules))))
             (parse-spec (spec)
               (if (eq spec 't)
                   (reverse (cdr modules))
		   (case (car spec)
		     (+ (append (reverse (cdr modules))
				(mapcar #'get-module (cdr spec))))
		     (- (let ((rem (mapcar #'get-module (cdr spec))))
			  (remove-if #'(lambda (m) (member m rem))
				     (reverse (cdr modules)))))
		     (otherwise (mapcar #'get-module spec))))))
      (dolist (file system-description)
        (let* ((name (car file))
               (port (car (cddddr file)))
               (module nil))
          (when (or (null port)
                    (member port *port*))
            (setq module (get-module name))
            (setf (module-load-env module) (parse-spec (cadr file))
                  (module-comp-env module) (parse-spec (caddr file))
                  (module-recomp-reasons module) (parse-spec (cadddr file))))))
      (let ((filenames (mapcar #'car system-description)))
	(sort modules #'(lambda (name1 name2)
			  (member name2 (member name1 filenames)))
	      :key #'module-name)))))


(defun make-transformations (modules filter make-transform)
  (declare (type function filter make-transform))
  (let ((transforms (list nil)))
    (dolist (m modules)
      (when (funcall filter m transforms) (funcall make-transform m transforms)))
    (reverse (cdr transforms))))

(defun make-compile-transformation (module transforms)
  (unless (dolist (trans transforms)
	    (and (eq (car trans) ':compile)
		 (eq (cadr trans) module)
		 (return t)))
    (dolist (c (module-comp-env module)) (make-load-transformation c transforms))
    (setf (cdr transforms)
	  (remove-if #'(lambda (trans) (and (eq (car trans) :load)
					    (eq (cadr trans) module)))
		     (cdr transforms)))
    (push `(:compile ,module) (cdr transforms))))

(defvar *being-loaded* ())

(defun make-load-transformation (module transforms)
  (if (assoc module *being-loaded*)
      (throw module (setf (cdr transforms) (cdr (assoc module *being-loaded*))))
      (let ((*being-loaded* (cons (cons module (cdr transforms)) *being-loaded*)))
	(catch module
	  (unless (dolist (trans transforms)
		    (when (and (eq (car trans) ':load)
			       (eq (cadr trans) module))
		      (return t)))
	    (dolist (l (module-load-env module))
	      (make-load-transformation l transforms))
	    (push `(:load ,module) (cdr transforms)))))))

(defun make-load-without-dependencies-transformation (module transforms)
  (unless (dolist (trans transforms)
            (and (eq (car trans) ':load)
                 (eq (cadr trans) module)
                 (return trans)))
    (push `(:load ,module) (cdr transforms))))

(defun compile-filter (module transforms)
  (or (dolist (r (module-recomp-reasons module))
        (when (dolist (transform transforms)
                (when (and (eq (car transform) ':compile)
                           (eq (cadr transform) r))
                  (return t)))
          (return t)))
      (null (probe-file (make-binary-pathname (module-name module))))
      (> (file-write-date (make-source-pathname (module-name module)))
         (file-write-date (make-binary-pathname (module-name module))))))

(defun operation-transformations (name mode &optional arg)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let ((*system-directory* (funcall (the function (car system))))
	  (modules (cadr system)))
      (ecase mode
	(:compile
	  ;; Compile any files that have changed and any other files
	  ;; that require recompilation when another file has been
	  ;; recompiled.
	  (make-transformations
	   modules
	   #'compile-filter
	   #'make-compile-transformation))
	(:recompile
	  ;; Force recompilation of all files.
	  (make-transformations
	   modules
	   #'true
	   #'make-compile-transformation))
	(:recompile-some
	  ;; Force recompilation of some files.  Also compile the
	  ;; files that require recompilation when another file has
	  ;; been recompiled.
	  (make-transformations
	   modules
	   #'(lambda (m transforms)
	       (or (member (module-name m) arg)
		   (compile-filter m transforms)))
	   #'make-compile-transformation))
	(:query-compile
	  ;; Ask the user which files to compile.  Compile those
	  ;; and any other files which must be recompiled when
	  ;; another file has been recompiled.
	  (make-transformations
	   modules
	   #'(lambda (m transforms)
	       (or (compile-filter m transforms)
		   (y-or-n-p "Compile ~A?"
			     (module-name m))))
	   #'make-compile-transformation))
	(:confirm-compile
	  ;; Offer the user a chance to prevent a file from being
	  ;; recompiled.
	  (make-transformations
	   modules
	   #'(lambda (m transforms)
	       (and (compile-filter m transforms)
		    (y-or-n-p "Go ahead and compile ~A?"
			      (module-name m))))
	   #'make-compile-transformation))
	(:load
	  ;; Load the whole system.
	  (make-transformations
	   modules
	   #'true
	   #'make-load-transformation))
	(:query-load
	  ;; Load only those files the user says to load.
	  (make-transformations
	   modules
	   #'(lambda (m transforms)
	       (declare (ignore transforms))
	       (y-or-n-p "Load ~A?" (module-name m)))
	   #'make-load-without-dependencies-transformation))))))

(defun true (&rest ignore)
  (declare (ignore ignore))
  't)

(defun operate-on-system (name mode &optional arg print-only)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let* ((*system-directory* (funcall (the function (car system))))
	   (transformations (operation-transformations name mode arg)))
      (labels ((load-binary (name pathname)
		 (format t "~&Loading binary of ~A...~%" name)
		 (or print-only (load pathname)))	       
	       (load-module (m)
		 (let* ((name (module-name m))
			(*load-verbose* nil)
			(binary (make-binary-pathname name)))
		   (load-binary name binary)))
	       (compile-module (m)
		 (format t "~&Compiling ~A...~%" (module-name m))
		 (unless print-only
		   (let  ((name (module-name m)))
		     (compile-file (make-source-pathname name)
				   :output-file
				   (make-pathname :defaults
						  (make-binary-pathname name)
						  :version :newest))))))
	(#+Genera
	 compiler:compiler-warnings-context-bind
	 #+TI
	 COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
	 #+:LCL3.0
	 lucid-common-lisp:with-deferred-warnings
	 #+cmu
	 with-compilation-unit #+cmu ()
	 #-(or Genera TI :LCL3.0 cmu)
	 progn
           (loop (when (null transformations) (return t))
		 (let ((transform (pop transformations)))
		   (ecase (car transform)
		     (:compile (compile-module (cadr transform)))
		     (:load (load-module (cadr transform)))))))))))


(defun make-source-pathname (name) (make-pathname-internal name :source))
(defun make-binary-pathname (name) (make-pathname-internal name :binary))

(defun make-pathname-internal (name type)
  (let* ((extension (ecase type
                      (:source (car *pathname-extensions*))
                      (:binary (cdr *pathname-extensions*))))
         (directory (pathname
		      (etypecase *system-directory*
			(string *system-directory*)
			(pathname *system-directory*)
			(cons (ecase type
				(:source (car *system-directory*))
				(:binary (cdr *system-directory*)))))))
         (pathname
           (make-pathname
             :name (string-downcase (string name))
             :type extension
             :defaults directory)))

    #+Genera
    (setq pathname (zl:send pathname :new-raw-name (pathname-name pathname))
          pathname (zl:send pathname :new-raw-type (pathname-type pathname)))

    pathname))

(defun system-source-files (name)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let ((*system-directory* (funcall (the function (car system))))
	  (modules (cadr system)))
      (mapcar #'(lambda (module)
		  (make-source-pathname (module-name module)))
	      modules))))

(defun system-binary-files (name)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let ((*system-directory* (funcall (the function (car system))))
	  (modules (cadr system)))
      (mapcar #'(lambda (module)
		  (make-binary-pathname (module-name module)))
	      modules))))

;;; ***                SITE SPECIFIC PCL DIRECTORY                        ***
;;;
;;; *pcl-directory* is a variable which specifies the directory pcl is stored
;;; in at your site.  If the value of the variable is a single pathname, the
;;; sources and binaries should be stored in that directory.  If the value of
;;; that directory is a cons, the CAR should be the source directory and the
;;; CDR should be the binary directory.
;;;
;;; By default, the value of *pcl-directory* is set to the directory that
;;; this file is loaded from.  This makes it simple to keep multiple copies
;;; of PCL in different places, just load defsys from the same directory as
;;; the copy of PCL you want to use.
;;;
;;; Note that the value of *PCL-DIRECTORY* is set using a DEFVAR.  This is
;;; done to make it possible for users to set it in their init file and then
;;; load this file.  The value set in the init file will override the value
;;; here.
;;;
;;; ***                                                                   ***

(defun load-truename (&optional (errorp nil))
  (flet ((bad-time ()
	   (when errorp
	     (error "LOAD-TRUENAME called but a file isn't being loaded."))))
    #+Lispm  (or sys:fdefine-file-pathname (bad-time))
    #+excl   excl::*source-pathname*
    #+Xerox  (pathname (or (il:fullname *standard-input*) (bad-time)))
    #+(and dec vax common) (truename (sys::source-file #'load-truename))
    ;;
    ;; The following use of  `lucid::' is a kludge for 2.1 and 3.0
    ;; compatibility.  In 2.1 it was in the SYSTEM package, and i
    ;; 3.0 it's in the LUCID-COMMON-LISP package.
    ;;
    #+LUCID (or lucid::*source-pathname* (bad-time))
    #+akcl   si:*load-pathname*
    #+cmu17 *load-truename*
    #-(or Lispm excl Xerox (and dec vax common) LUCID akcl cmu17) nil))

#-(or (and cmu (not cmu17)) Symbolics)
(defvar *pcl-directory*
	(or (load-truename t)
	    (error "Because load-truename is not implemented in this port~%~
                    of PCL, you must manually edit the definition of the~%~
                    variable *pcl-directory* in the file defsys.lisp.")))

#+(and cmu (not cmu17))
(defvar *pcl-directory* (pathname "pcl:"))

#+Genera
(defvar *pcl-directory*
	(let ((source (load-truename t)))
	  (flet ((subdir (name)
		   (scl:send source :new-pathname :raw-directory
			     (append (scl:send source :raw-directory)
				     (list name)))))
	    (cons source
		  #+genera-release-7-2       (subdir "rel-7-2")
		  #+genera-release-7-3       (subdir "rel-7-3") 
		  #+genera-release-7-4       (subdir "rel-7-4")
		  #+genera-release-8-0       (subdir "rel-8-0")
		  #+genera-release-8-1       (subdir "rel-8-1")
		  ))))

#+Cloe-Runtime
(defvar *pcl-directory* (pathname "/usr3/hornig/pcl/"))

(defsystem pcl	   
           *pcl-directory*
  ;;
  ;; file         load           compile      files which       port
  ;;              environment    environment  force the of
  ;;                                          recompilation
  ;;                                          of this file
  ;;                                          
  (
;  (rel-6-patches   t            t            ()                rel-6)
;  (rel-7-1-patches t            t            ()                rel-7-1)
   (rel-7-2-patches t            t            ()                rel-7-2)
   (rel-8-patches   t            t            ()                rel-8)
   (ti-patches      t            t            ()                ti)
   (pyr-patches     t            t            ()                pyramid)
   (xerox-patches   t            t            ()                xerox)
   (kcl-patches     t            t            ()                kcl)
   (ibcl-patches    t            t            ()                ibcl)
   (gcl-patches     t            t            ()                gclisp)
   
   (pkg             t            t            ())
   (sys-proclaim    t            t            ()                kcl)
   (walk            (pkg)        (pkg)        ())
   (iterate         t            t            ())
   (macros          t            t            ())
   (low             (pkg macros) t            (macros))
   
   
   (genera-low     (low)         (low)        (low)            Genera)
   (cloe-low	   (low)	 (low)	      (low)            Cloe)
   (lucid-low      (low)         (low)        (low)            Lucid)
   (Xerox-low      (low)         (low)        (low)            Xerox)
   (ti-low         (low)         (low)        (low)            TI)
   (vaxl-low       (low)         (low)        (low)            vaxlisp)
   (kcl-low        (low)         (low)        (low)            KCL)
   (ibcl-low       (low)         (low)        (low)            IBCL)
   (excl-low       (low)         (low)        (low)            excl)
   (cmu-low        (low)         (low)        (low)            CMU)
   (hp-low         (low)         (low)        (low)            HP-HPLabs)
   (gold-low       (low)         (low)        (low)            gclisp) 
   (pyr-low        (low)         (low)        (low)            pyramid) 
   (coral-low      (low)         (low)        (low)            coral)
   
   (fin         t                                   t (low))
   (defclass    t                                   t (low))
   (defs        t                                   t (defclass macros iterate))
   (fngen       t                                   t (low))
   (cache       t                                   t (low defs))
   ;;(lap         t                                   t (low)    excl-sun4)
   ;;(plap        t                                   t (low)    excl-sun4)
   ;;(cpatch      t                                   t (low)    excl-sun4)
   ;;(quadlap     t                                   t (low)    excl-sun4)
   ;;(dlap        t                                   t (defs low fin cache lap) 
   ;;	                                                         excl-sun4)
   ;;#-excl-sun4
   (dlisp       t                                   t (defs low fin cache))
   (dlisp2      t                                   t (low fin cache dlisp))
   (boot        t                                   t (defs fin))
   (vector      t                                   t (boot defs cache fin))
   (slots-boot  t                                   t (vector boot defs cache fin))
   (combin      t                                   t (boot defs))
   (dfun        t                                   t (boot low cache))
   (fast-init   t                                   t (boot low))
   (braid       (+ precom1 precom2)                 t (boot defs low fin cache))
   (generic-functions t                             t (boot))
   (slots       t                                   t (vector boot defs low cache fin))
   (init        t                                   t (vector boot defs low fast-init))
   (std-class   t                                   t (vector boot defs low cache fin slots))
   (cpl         t                                   t (vector boot defs low cache fin slots))
   (fsc         t                                   t (defclass boot defs low fin cache))
   (methods     t                                   t (defclass boot defs low fin cache))
   (fixup       t                                   t (boot defs low fin))
   (defcombin   t                                   t (defclass boot defs low fin))
   (ctypes      t                                   t (defclass defcombin))
   #+ignore
   (construct   t                                   t (defclass boot defs low))
   (env         t                                   t (defclass boot defs low fin))
   (compat      t                                   t ())
   (precom1     (dlisp)                             t (defs low cache fin dfun))
   (precom2     (dlisp)                             t (defs low cache fin dfun))
   ))

(defun compile-pcl (&optional m)
  (let (#+:coral(ccl::*warn-if-redefine-kernel* nil)
	#+Lucid (lcl:*redefinition-action* nil)
	#+excl  (excl::*redefinition-warnings* nil)
	#+Genera (sys:inhibit-fdefine-warnings t)
	)
    (cond ((null m)        (operate-on-system 'pcl :compile))
	  ((eq m :print)   (operate-on-system 'pcl :compile () t))
	  ((eq m :query)   (operate-on-system 'pcl :query-compile))
	  ((eq m :confirm) (operate-on-system 'pcl :confirm-compile))
	  ((eq m 't)       (operate-on-system 'pcl :recompile))        
	  ((listp m)       (operate-on-system 'pcl :compile-from m))
	  ((symbolp m)     (operate-on-system 'pcl :recompile-some `(,m))))))

(defun load-pcl (&optional m)
  (let (#+:coral(ccl::*warn-if-redefine-kernel* nil)
	#+Lucid (lcl:*redefinition-action* nil)
	#+excl  (excl::*redefinition-warnings* nil)
	#+Genera (sys:inhibit-fdefine-warnings t)
	)
    (cond ((null m)      (operate-on-system 'pcl :load))
	  ((eq m :query) (operate-on-system 'pcl :query-load)))))

#+Genera
;;; Make sure Genera bug mail contains the PCL bug data.  A little
;;; kludgy, but what the heck.  If they didn't mean for people to do
;;; this, they wouldn't have made private patch notes be flavored
;;; objects, right?  Right.
(progn
  (scl:defflavor pcl-private-patch-info ((description)) ())
  (scl:defmethod (sct::private-patch-info-description pcl-private-patch-info) ()
    (or description
	(setf description (string-append "PCL version: " *pcl-system-date*))))
  (scl:defmethod (sct::private-patch-info-pathname pcl-private-patch-info) ()
    *pcl-directory*)
  (unless (find-if #'(lambda (x) (typep x 'pcl-private-patch-info))
		   sct::*private-patch-info*)
    (push (scl:make-instance 'pcl-private-patch-info)
	  sct::*private-patch-info*)))

(defun bug-report-info (&optional (stream *standard-output*))
  (format stream "~&PCL system date: ~A~
                  ~&Lisp Implementation type: ~A~
                  ~&Lisp Implementation version: ~A~
                  ~&*features*: ~S"
	  *pcl-system-date*
	  (lisp-implementation-type)
	  (lisp-implementation-version)
	  *features*))



;;;;
;;;
;;; This stuff is not intended for external use.
;;; 
(defun rename-pcl ()
  (dolist (f (cadr (get-system 'pcl)))
    (let ((old nil)
          (new nil))
      (let ((*system-directory* *default-pathname-defaults*))
        (setq old (make-source-pathname (car f))))
      (setq new  (make-source-pathname (car f)))
      (rename-file old new))))

#+Genera
(defun edit-pcl ()
  (dolist (f (cadr (get-system 'pcl)))
    (let ((*system-directory* *pcl-directory*))
      (zwei:find-file (make-source-pathname (car f))))))

#+Genera
(defun hardcopy-pcl (&optional query-p)
  (let ((files (mapcar #'(lambda (f)
                           (setq f (car f))
                           (and (or (not query-p)
                                    (y-or-n-p "~A? " f))
                                f))
		       (cadr (get-system 'pcl))))
        (b zwei:*interval*))
    (unwind-protect
        (dolist (f files)
          (when f
            (multiple-value-bind (ignore b)
                (zwei:find-file (make-source-pathname f))
              (zwei:hardcopy-buffer b))))
      (zwei:make-buffer-current b))))


;;;
;;; unido!ztivax!dae@seismo.css.gov
;;; z30083%tansei.cc.u-tokyo.junet@utokyo-relay.csnet
;;; Victor@carmen.uu.se
;;; mcvax!harlqn.co.uk!chris@uunet.UU.NET
;;; 
#+Genera
(defun mail-pcl (to)
  (let* ((original-buffer zwei:*interval*)
	 (*system-directory* (pathname "vaxc:/user/ftp/pub/pcl/")
			    ;(funcall (car (get-system 'pcl)))
			     )
         (files (list* 'defsys
			'test
			(caddr (get-system 'pcl))))
         (total-number (length files))
         (file nil)
	 (number-of-lines 0)
         (i 0)
         (mail-buffer nil))
    (unwind-protect
        (loop
           (when (null files) (return nil))
           (setq file (pop files))
           (incf i)
           (multiple-value-bind (ignore b)
               (zwei:find-file (make-source-pathname file))
	     (setq number-of-lines (zwei:count-lines b))
             (zwei:com-mail-internal t
                                     :initial-to to
                                     :initial-body b
				     :initial-subject
                                     (format nil
				       "PCL file   ~A   (~A of ~A) ~D lines"
				       file i total-number number-of-lines))
             (setq mail-buffer zwei:*interval*)
             (zwei:com-exit-com-mail)
             (format t "~&Just sent ~A  (~A of ~A)." b i total-number)
             (zwei:kill-buffer mail-buffer)))
      (zwei:make-buffer-current original-buffer))))

(defun reset-pcl-package ()		; Try to do this safely
  (let* ((vars '(*pcl-directory* 
		 *default-pathname-extensions* 
		 *pathname-extensions*
		 *redefined-functions*))
	 (names (mapcar #'symbol-name vars))
	 (values (mapcar #'symbol-value vars)))
    (declare (special *redefined-functions*))
    (reset-package "PCL")
    (let ((pkg (find-package "SLOT-ACCESSOR-NAME")))
      (when pkg
	(do-symbols (sym pkg)
	  (makunbound sym)
	  (fmakunbound sym)
	  (setf (symbol-plist sym) nil))))
    (let ((pcl (find-package "PCL")))
      (mapcar #'(lambda (name value)
		  (let ((var (intern name pcl)))
		    (proclaim `(special ,var))
		    (set var value)))
	      names values))      
    (dolist (sym *redefined-functions*)
      (setf (symbol-function sym) (get sym ':definition-before-pcl)))
    nil))

(defun reset-package (&optional (package-name "PCL"))
  (let ((pkg (find-package package-name)))
    (do-symbols (sym pkg)
      (when (eq pkg (symbol-package sym))
	(if (or (constantp sym)
		#-cmu (member sym '(wrapper cache arg-info pv-table))
		#+cmu
		(or (c::info setf inverse sym)
		    (c::info setf expander sym)
		    (c::info type kind sym)
		    (c::info type structure-info sym)
		    (c::info type defined-structure-info sym)
		    (c::info function macro-function sym)
		    (c::info function compiler-macro-function sym)))
	    (unintern sym pkg)
	    (progn
	      (makunbound sym)
	      (unless (or (eq sym 'reset-pcl-package)
			  (eq sym 'reset-package))
		(fmakunbound sym)
		#+cmu
		(fmakunbound `(setf ,sym)))
	      (setf (symbol-plist sym) nil)))))))
