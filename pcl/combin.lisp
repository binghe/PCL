;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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

(in-package :pcl)

(defun get-method-function (method &optional method-alist wrappers)
  (let ((fn (cadr (assoc method method-alist))))
    (if fn
	(values fn nil nil nil)
	(multiple-value-bind (mf fmf)
	    (if (listp method)
		(early-method-function method)
		(values nil (method-fast-function method)))
	  (let* ((pv-table (and fmf (method-function-pv-table fmf))))
	    (if (and fmf (or (null pv-table) wrappers))
		(let* ((pv-wrappers (when pv-table 
				      (pv-wrappers-from-all-wrappers
				       pv-table wrappers)))
		       (pv-cell (when (and pv-table pv-wrappers)
				  (pv-table-lookup pv-table pv-wrappers))))
		  (values mf t fmf pv-cell))
		(values 
		 (or mf (if (listp method)
			    (setf (cadr method)
				  (method-function-from-fast-function fmf))
			    (method-function method)))
		 t nil nil)))))))

(defun make-effective-method-function (generic-function form &optional 
				       method-alist wrappers)
  (funcall (make-effective-method-function1 generic-function form
					    (not (null method-alist))
					    (not (null wrappers)))
	   method-alist wrappers))

(defun make-effective-method-function1 (generic-function form 
					method-alist-p wrappers-p)
  (if (and (listp form)
	   (eq (car form) 'call-method))
      (make-effective-method-function-simple generic-function form)
      ;;
      ;; We have some sort of `real' effective method.  Go off and get a
      ;; compiled function for it.  Most of the real hair here is done by
      ;; the GET-FUNCTION mechanism.
      ;; 
      (make-effective-method-function-internal generic-function form
					       method-alist-p wrappers-p)))

(defun make-effective-method-function-type (generic-function form
					    method-alist-p wrappers-p)
  (if (and (listp form)
	   (eq (car form) 'call-method))
      (let* ((cm-args (cdr form))
	     (method (car cm-args)))
	(when method
	  (if (if (listp method)
		  (eq (car method) ':early-method)
		  (method-p method))
	      (if method-alist-p
		  't
		  (multiple-value-bind (mf fmf)
		      (if (listp method)
			  (early-method-function method)
			  (values nil (method-fast-function method)))
		    (declare (ignore mf))
		    (let* ((pv-table (and fmf (method-function-pv-table fmf))))
		      (if (and fmf (or (null pv-table) wrappers-p))
			  'fast-method-call
			  'method-call))))
	      (if (and (consp method) (eq (car method) 'make-method))
		  (make-effective-method-function-type 
		   generic-function (cadr method) method-alist-p wrappers-p)
		  (type-of method)))))
      'fast-method-call))

(defun make-effective-method-function-simple (generic-function form
							       &optional no-fmf-p)
  ;;
  ;; The effective method is just a call to call-method.  This opens up
  ;; the possibility of just using the method function of the method as
  ;; the effective method function.
  ;;
  ;; But we have to be careful.  If that method function will ask for
  ;; the next methods we have to provide them.  We do not look to see
  ;; if there are next methods, we look at whether the method function
  ;; asks about them.  If it does, we must tell it whether there are
  ;; or aren't to prevent the leaky next methods bug.
  ;; 
  (let* ((cm-args (cdr form))
	 (fmf-p (and (null no-fmf-p)
		     (or (not (eq *boot-state* 'complete))
			 (gf-fast-method-function-p generic-function))
		     (null (cddr cm-args))))
	 (method (car cm-args))
	 (cm-args1 (cdr cm-args)))
    #'(lambda (method-alist wrappers)
	(make-effective-method-function-simple1 generic-function method cm-args1 fmf-p
						method-alist wrappers))))

(defun make-emf-from-method (method cm-args &optional gf fmf-p method-alist wrappers)
  (multiple-value-bind (mf real-mf-p fmf pv-cell)
      (get-method-function method method-alist wrappers)
    (if fmf
	(let* ((next-methods (car cm-args))
	       (next (make-effective-method-function-simple1
		      gf (car next-methods)
		      (list* (cdr next-methods) (cdr cm-args))
		      fmf-p method-alist wrappers))
	       (arg-info (method-function-get fmf ':arg-info)))
	  (make-fast-method-call :function fmf
				 :pv-cell pv-cell
				 :next-method-call next
				 :arg-info arg-info))
	(if real-mf-p
	    (make-method-call :function mf
			      :call-method-args cm-args)
	    mf))))

(defun make-effective-method-function-simple1 (gf method cm-args fmf-p
						  &optional method-alist wrappers)
  (when method
    (if (if (listp method)
	    (eq (car method) ':early-method)
	    (method-p method))
	(make-emf-from-method method cm-args gf fmf-p method-alist wrappers)
	(if (and (consp method) (eq (car method) 'make-method))
	    (make-effective-method-function gf (cadr method) method-alist wrappers)
	    method))))

(defvar *global-effective-method-gensyms* ())
(defvar *rebound-effective-method-gensyms*)

(defun get-effective-method-gensym ()
  (or (pop *rebound-effective-method-gensyms*)
      (let ((new (intern (format nil "EFFECTIVE-METHOD-GENSYM-~D" 
				 (length *global-effective-method-gensyms*))
			 "PCL")))
	(setq *global-effective-method-gensyms*
	      (append *global-effective-method-gensyms* (list new)))
	new)))

(let ((*rebound-effective-method-gensyms* ()))
  (dotimes (i 10) (get-effective-method-gensym)))

(defun expand-effective-method-function (gf effective-method &optional env)
  (declare (ignore env))
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-function-info gf)
    (declare (ignore nreq nkeys arg-info))
    `(lambda ,(make-fast-method-call-lambda-list metatypes applyp)
       (declare (ignore .pv-cell. .next-method-call.))
       ,effective-method)))

(defun expand-emf-call-method (gf form metatypes applyp env)
  (declare (ignore gf metatypes applyp env))
  `(call-method ,(cdr form)))

(defmacro call-method (&rest args)
  (declare (ignore args))
  `(error "~S outsize of a effective method form" 'call-method))

(defun memf-test-converter (form generic-function method-alist-p wrappers-p)
  (cond ((and (consp form) (eq (car form) 'call-method))
	 (case (make-effective-method-function-type 
		generic-function form method-alist-p wrappers-p)
	   (fast-method-call
	    '.fast-call-method.)
	   (t
	    '.call-method.)))
	((and (consp form) (eq (car form) 'call-method-list))
	 (case (if (every #'(lambda (form)
			      (eq 'fast-method-call
				  (make-effective-method-function-type 
				   generic-function form 
				   method-alist-p wrappers-p)))
			  (cdr form))
		   'fast-method-call
		   't)
	   (fast-method-call
	    '.fast-call-method-list.)
	   (t
	    '.call-method-list.)))
	(t
	 (default-test-converter form))))

(defun memf-code-converter (form generic-function 
				 metatypes applyp method-alist-p wrappers-p)
  (cond ((and (consp form) (eq (car form) 'call-method))
	 (let ((gensym (get-effective-method-gensym)))
	   (values (make-emf-call metatypes applyp gensym
				  (make-effective-method-function-type 
				   generic-function form method-alist-p wrappers-p))
		   (list gensym))))
	((and (consp form) (eq (car form) 'call-method-list))
	 (let ((gensym (get-effective-method-gensym))
	       (type (if (every #'(lambda (form)
				    (eq 'fast-method-call
					(make-effective-method-function-type 
					 generic-function form 
					 method-alist-p wrappers-p)))
				(cdr form))
			 'fast-method-call
			 't)))
	   (values `(dolist (emf ,gensym nil)
		      ,(make-emf-call metatypes applyp 'emf type))
		   (list gensym))))		     
	(t
	 (default-code-converter form))))

(defun memf-constant-converter (form generic-function)
  (cond ((and (consp form) (eq (car form) 'call-method))
	 (list (cons '.meth.
		     (make-effective-method-function-simple
		      generic-function form))))
	((and (consp form) (eq (car form) 'call-method-list))
	 (list (cons '.meth-list.
		     (mapcar #'(lambda (form)
				 (make-effective-method-function-simple
				  generic-function form))
			     (cdr form)))))
	(t
	 (default-constant-converter form))))

(defun make-effective-method-function-internal (generic-function effective-method
					        method-alist-p wrappers-p)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-function-info generic-function)
    (declare (ignore nkeys arg-info))
    (let* ((*rebound-effective-method-gensyms* *global-effective-method-gensyms*)
	   (name (if (early-gf-p generic-function)
		     (early-gf-name generic-function)
		     (generic-function-name generic-function)))
	   (arg-info (cons nreq applyp))
	   (effective-method-lambda (expand-effective-method-function
				     generic-function effective-method)))
      (multiple-value-bind (cfunction constants)
	  (get-function1 effective-method-lambda
			 #'(lambda (form)
			     (memf-test-converter form generic-function
						  method-alist-p wrappers-p))
			 #'(lambda (form)
			     (memf-code-converter form generic-function
						  metatypes applyp
						  method-alist-p wrappers-p))
			 #'(lambda (form)
			     (memf-constant-converter form generic-function)))
	#'(lambda (method-alist wrappers)
	    (let* ((constants 
		    (mapcar #'(lambda (constant)
				(if (consp constant)
				    (case (car constant)
				      (.meth.
				       (funcall (cdr constant)
						method-alist wrappers))
				      (.meth-list.
				       (mapcar #'(lambda (fn)
						   (funcall fn method-alist wrappers))
					       (cdr constant)))
				      (t constant))
				    constant))
			    constants))
		   (function (set-function-name
			      (apply cfunction constants)
			      `(combined-method ,name))))
	      (make-fast-method-call :function function
				     :arg-info arg-info)))))))

(defmacro call-method-list (&rest calls)
  `(progn ,@calls))

(defun make-call-methods (methods)
  `(call-method-list
    ,@(mapcar #'(lambda (method) `(call-method ,method ())) methods)))

(defun standard-compute-effective-method (generic-function combin applicable-methods)
  (declare (ignore combin))
  (let ((before ())
	(primary ())
	(after ())
	(around ()))
    (dolist (m applicable-methods)
      (let ((qualifiers (if (listp m)
			    (early-method-qualifiers m)
			    (method-qualifiers m))))			    
	(cond ((member ':before qualifiers)  (push m before))
	      ((member ':after  qualifiers)  (push m after))
	      ((member ':around  qualifiers) (push m around))
	      (t
	       (push m primary)))))
    (setq before  (reverse before)
	  after   (reverse after)
	  primary (reverse primary)
	  around  (reverse around))
    (cond ((null primary)
	   `(error "No primary method for the generic function ~S." ',generic-function))
	  ((and (null before) (null after) (null around))
	   ;;
	   ;; By returning a single call-method `form' here we enable an important
	   ;; implementation-specific optimization.
	   ;; 
	   `(call-method ,(first primary) ,(rest primary)))
	  (t
	   (let ((main-effective-method
		   (if (or before after)
		       `(multiple-value-prog1
			  (progn ,(make-call-methods before)
				 (call-method ,(first primary) ,(rest primary)))
			  ,(make-call-methods (reverse after)))
		       `(call-method ,(first primary) ,(rest primary)))))
	     (if around
		 `(call-method ,(first around)
			       (,@(rest around) (make-method ,main-effective-method)))
		 main-effective-method))))))

;;;
;;; The STANDARD method combination type.  This is coded by hand (rather than
;;; with define-method-combination) for bootstrapping and efficiency reasons.
;;; Note that the definition of the find-method-combination-method appears in
;;; the file defcombin.lisp, this is because EQL methods can't appear in the
;;; bootstrap.
;;;
;;; The defclass for the METHOD-COMBINATION and STANDARD-METHOD-COMBINATION
;;; classes has to appear here for this reason.  This code must conform to
;;; the code in the file defcombin, look there for more details.
;;;

(defun compute-effective-method (generic-function combin applicable-methods)
  (standard-compute-effective-method generic-function combin applicable-methods))

(defvar *invalid-method-error*
	#'(lambda (&rest args)
	    (declare (ignore args))
	    (error
	      "INVALID-METHOD-ERROR was called outside the dynamic scope~%~
               of a method combination function (inside the body of~%~
               DEFINE-METHOD-COMBINATION or a method on the generic~%~
               function COMPUTE-EFFECTIVE-METHOD).")))

(defvar *method-combination-error*
	#'(lambda (&rest args)
	    (declare (ignore args))
	    (error
	      "METHOD-COMBINATION-ERROR was called outside the dynamic scope~%~
               of a method combination function (inside the body of~%~
               DEFINE-METHOD-COMBINATION or a method on the generic~%~
               function COMPUTE-EFFECTIVE-METHOD).")))

;(defmethod compute-effective-method :around        ;issue with magic
;	   ((generic-function generic-function)     ;generic functions
;	    (method-combination method-combination)
;	    applicable-methods)
;  (declare (ignore applicable-methods))
;  (flet ((real-invalid-method-error (method format-string &rest args)
;	   (declare (ignore method))
;	   (apply #'error format-string args))
;	 (real-method-combination-error (format-string &rest args)
;	   (apply #'error format-string args)))
;    (let ((*invalid-method-error* #'real-invalid-method-error)
;	  (*method-combination-error* #'real-method-combination-error))
;      (call-next-method))))

(defun invalid-method-error (&rest args)
  (declare (arglist method format-string &rest format-arguments))
  (apply *invalid-method-error* args))

(defun method-combination-error (&rest args)
  (declare (arglist format-string &rest format-arguments))
  (apply *method-combination-error* args))

;This definition appears in defcombin.lisp.
;
;(defmethod find-method-combination ((generic-function generic-function)
;				     (type (eql 'standard))
;				     options)
;  (when options
;    (method-combination-error
;      "The method combination type STANDARD accepts no options."))
;  *standard-method-combination*)

