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


(in-package "COMPILER")

;; do evaluation of top level forms at compile time.
(eval-when (compile eval load)
(setq  *EVAL-WHEN-COMPILE* t)
)

(pushnew :turbo-closure *features*)
(pushnew :turbo-closure-env-size *features*)
;; patch around compiler bug.


(let ((rset "int Rset;
"))
  (unless (search rset compiler::*cmpinclude-string*)
      (setq compiler::*cmpinclude-string*
	    (concatenate 'string rset compiler::*cmpinclude-string*))))

(when (get 'si::basic-wrapper 'si::s-data)
  (pushnew :new-kcl-wrapper *features*)
  (pushnew :structure-wrapper *features*))
  



#+akcl
(progn

(unless (fboundp 'real-c2lambda-expr-with-key)
  (setf (symbol-function 'real-c2lambda-expr-with-key)
	(symbol-function 'c2lambda-expr-with-key)))

(defun c2lambda-expr-with-key (lambda-list body)
  (declare (special *sup-used*))
  (setq *sup-used* t)
  (real-c2lambda-expr-with-key lambda-list body))


;There is a bug in the implementation of *print-circle* that
;causes some akcl debugging commands (including :bt and :bl)
;to cause the following error when PCL is being used:
;Unrecoverable error: value stack overflow.

;When a CLOS object is printed, travel_push_object ends up
;traversing almost the whole class structure, thereby overflowing
;the value-stack.

;from lsp/debug.lsp.
;*print-circle* is badly implemented in kcl.
;it has two separate problems that should be fixed:
;  1. it traverses the printed object putting all objects found
;     on the value stack (rather than in a hash table or some
;     other structure; this is a problem because the size of the value stack
;     is fixed, and a potentially unbounded number of objects
;     need to be traversed), and
;  2. it blindly traverses all slots of any
;     kind of structure including std-object structures.
;     This is safe, but not always necessary, and is very time-consuming
;     for CLOS objects (because it will always traverse every class).

;For now, avoid using *print-circle* T when it will cause problems.



(eval-when (compile eval )
(defmacro si::f (op &rest args)
    `(the fixnum (,op ,@ (mapcar #'(lambda (x) `(the fixnum ,x)) args) )))

(defmacro si::fb (op &rest args)
    `(,op ,@ (mapcar #'(lambda (x) `(the fixnum ,x)) args) ))
)

(defun si::display-env (n env)
  (do ((v (reverse env) (cdr v)))
      ((or (not (consp v)) (si::fb > (fill-pointer si::*display-string*) n)))
    (or (and (consp (car v))
	     (listp (cdar v)))
	(return))
    (let ((*print-circle* (can-use-print-circle-p (cadar v))))
      (format si::*display-string* "~s=~s~@[,~]" (caar v) (cadar v) (cdr v)))))

(defun si::display-compiled-env ( plength ihs &aux
				      (base (si::ihs-vs ihs))
				      (end (min (si::ihs-vs (1+ ihs)) (si::vs-top))))
  (format si::*display-string* "")
  (do ((i base )
       (v (get (si::ihs-fname ihs) 'si::debug) (cdr v)))
      ((or (si::fb >= i end)(si::fb > (fill-pointer si::*display-string*) plength)))
    (let ((*print-circle* (can-use-print-circle-p (si::vs i))))
    (format si::*display-string* "~a~@[~d~]=~s~@[,~]"
	    (or (car v)  'si::loc) (if (not (car v)) (si::f - i base)) (si::vs i)
	    (si::fb < (setq i (si::f + i 1)) end)))))

(clines "#define objnull_p(x) ((x==OBJNULL)?Ct:Cnil)")
(defentry objnull-p (object) (object "objnull_p"))

(defun can-use-print-circle-p (x)
  (catch 'can-use-print-circle-p
    (can-use-print-circle-p1 x nil)))

(defun can-use-print-circle-p1 (x so-far)
  (and (not (objnull-p x)) ; because of deficiencies in the compiler, maybe?
       (if (member x so-far)
	   (throw 'can-use-print-circle-p t)
	   (let ((so-far (cons x so-far)))
	     (flet ((can-use-print-circle-p (x)
		      (can-use-print-circle-p1 x so-far)))
	       (typecase x
		 (vector  (or (not (eq 't (array-element-type x)))
			      (every #'can-use-print-circle-p x)))
		 (cons    (and (can-use-print-circle-p (car x))
			       (can-use-print-circle-p (cdr x))))
		 (array   (or (not (eq 't (array-element-type x)))
			      (let* ((rank (array-rank x))
				     (dimensions (make-list rank)))
				(dotimes (i rank)
				  (setf (nth i dimensions) (array-dimension x i)))
				(or (member 0 dimensions)
				    (do ((cursor (make-list rank :initial-element 0)))
					(nil)
				      (declare (:dynamic-extent cursor))
				      (unless (can-use-print-circle-p
					       (apply #'aref x cursor))
					(return nil))
				      (when (si::increment-cursor cursor dimensions)
					(return t)))))))
		 (t (or (not (si:structurep x))
			(let* ((def (si:structure-def x))
			       (name (si::s-data-name def))
			       (len (si::s-data-length def))
			       (pfun (si::s-data-print-function def)))
			  (and (null pfun)
			       (dotimes (i len t)
				 (unless (can-use-print-circle-p
					  (si:structure-ref x name i))
				   (return nil)))))))))))))

(defun si::apply-display-fun (display-fun  n lis)  
  (let ((*print-length* si::*debug-print-level*)
	(*print-level* si::*debug-print-level*)
	(*print-pretty* nil)
	(*PRINT-CASE* :downcase)
	(*print-circle* nil)
	)
    (setf (fill-pointer si::*display-string*) 0)
    (format si::*display-string* "{")
    (funcall display-fun n lis)
    (when (si::fb > (fill-pointer si::*display-string*) n)
      (setf (fill-pointer si::*display-string*) n)
      (format si::*display-string* "..."))

    (format si::*display-string* "}")
    )
  si::*display-string*
  )

;The old definition of this had a bug:
;sometimes it returned without calling mv-values.
(defun si::next-stack-frame (ihs &aux line-info li i k na)
  (cond ((si::fb < ihs si::*ihs-base*)
	 (si::mv-values nil nil nil nil nil))
	((let (fun)
	   ;; next lower visible ihs
	   (si::mv-setq (fun i) (si::get-next-visible-fun ihs))
	   (setq na fun)
	   (cond ((and (setq line-info (get fun 'si::line-info))
		       (do ((j (si::f + ihs 1) (si::f - j 1))
			    (form ))
			   ((<= j i) nil)
			 (setq form (si::ihs-fun j))
			 (cond ((setq li (si::get-line-of-form form line-info))
				(return-from si::next-stack-frame 
				  (si::mv-values
				   i fun li
				   ;; filename
				   (car (aref line-info 0))
				   ;;environment
				   (list (si::vs (setq k (si::ihs-vs j)))
					 (si::vs (1+ k))
					 (si::vs (+ k 2)))))))))))))
	((and (not (special-form-p na))
	      (not (get na 'si::dbl-invisible))
	      (fboundp na))
	 (si::mv-values i na nil nil
		    (if (si::ihs-not-interpreted-env i)
			nil
			(let ((i (si::ihs-vs i)))
			  (list (si::vs i) (si::vs (1+ i)) (si::vs (si::f + i 2)))))))
	(t (si::mv-values nil nil nil nil nil))))
)







