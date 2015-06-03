;;; -*- Mode:LISP; Package:PCL; Base:10; Syntax:Common-Lisp -*-
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

#|

This implementation of method lookup was redone in early August of 89.

It has the following properties:

 - It's modularity makes it easy to modify the actual caching algorithm.
   The caching algorithm is almost completely separated into the files
   cache.lisp and dlap.lisp.  This file just contains the various uses
   of it. There will be more tuning as we get more results from Luis'
   measurements of caching behavior.

 - The metacircularity issues have been dealt with properly.  All of
   PCL now grounds out properly.  Moreover, it is now possible to have
   metaobject classes which are themselves not instances of standard
   metaobject classes.

** Modularity of the code **

The actual caching algorithm is isolated in a modest number of functions.
The code which generates cache lookup code is all found in cache.lisp and
dlap.lisp.  Certain non-wrapper-caching special cases are in this file.


** Handling the metacircularity **

In CLOS, method lookup is the potential source of infinite metacircular
regress.  The metaobject protocol specification gives us wide flexibility
in how to address this problem.  PCL uses a technique which handles the
problem not only for the metacircular language described in Chapter 3, but
also for the PCL protocol which includes additional generic functions
which control more aspects of the CLOS implementation.

The source of the metacircular regress can be seen in a number of ways.
One is that the specified method lookup protocol must, as part of doing
the method lookup (or at least the cache miss case), itself call generic
functions.  It is easy to see that if the method lookup for a generic
function ends up calling that same generic function there can be trouble.

Fortunately, there is an easy solution at hand.  The solution is based on 
the restriction that portable code cannot change the class of a specified
metaobject.  This restriction implies that for specified generic functions,
the method lookup protocol they follow is fixed.  

More precisely, for such specified generic functions, most generic functions
that are called during their own method lookup will not run portable methods. 
This allows the implementation to usurp the actual generic function call in
this case.  In short, method lookup of a standard generic function, in the
case where the only applicable methods are themselves standard doesn't
have to do any method lookup to implement itself.

And so, we are saved.

|#


;An alist in which each entry is of the form :
;  (<generator> . (<subentry> ...))
;Each subentry is of the form:
;  (<args> <constructor> <system>)
(defvar *dfun-constructors* ())			

;If this is NIL, then the whole mechanism
;for caching dfun constructors is turned
;off.  The only time that makes sense is
;when debugging LAP code. 
(defvar *enable-dfun-constructor-caching* t)	

(defun show-dfun-constructors ()
  (format t "~&DFUN constructor caching is ~A." 
	  (if *enable-dfun-constructor-caching*
	      "enabled" "disabled"))
  (dolist (generator-entry *dfun-constructors*)
    (dolist (args-entry (cdr generator-entry))
      (format t "~&~S ~S"
	      (cons (car generator-entry) (caar args-entry))
	      (caddr args-entry)))))

(defvar *raise-metatypes-to-class-p* t)

(defun get-dfun-constructor (generator &rest args)
  (when (and *raise-metatypes-to-class-p*
	     (member generator '(emit-checking emit-caching
				 emit-in-checking-cache-p emit-constant-value)))
    (setq args (cons (mapcar #'(lambda (mt)
				 (if (eq mt 't)
				     mt
				     'class))
			     (car args))
		     (cdr args))))						  
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (if (null *enable-dfun-constructor-caching*)
	(apply (symbol-function generator) args)
	(or (cadr args-entry)
	    (multiple-value-bind (new not-best-p)
		(apply (symbol-function generator) args)
	      (let ((entry (list (copy-list args) new (unless not-best-p 'pcl)
				 not-best-p)))
		(if generator-entry
		    (push entry (cdr generator-entry))
		    (push (list generator entry)
			  *dfun-constructors*)))
	      (values new not-best-p))))))

(defun load-precompiled-dfun-constructor (generator args system constructor)
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (if args-entry
	(when (fourth args-entry)
	  (let* ((dfun-type (case generator
			      (emit-checking 'checking)
			      (emit-caching 'caching)
			      (emit-constant-value 'constant-value)
			      (emit-default-only 'default-method-only)))
		 (metatypes (car args))
		 (gfs (when dfun-type (gfs-of-type dfun-type))))
	    (dolist (gf gfs)
	      (when (and (equal metatypes (arg-info-metatypes (gf-arg-info gf)))
			 (let ((gf-name (generic-function-name gf)))
			   (and (not (eq gf-name 'slot-value-using-class))
				(not (equal gf-name '(setf slot-value-using-class)))
				(not (eq gf-name 'slot-boundp-using-class)))))
		(update-dfun gf)))
	    (setf (second args-entry) constructor)
	    (setf (third args-entry) system)
	    (setf (fourth args-entry) nil)))
	(let ((entry (list args constructor system nil)))
	  (if generator-entry
	      (push entry (cdr generator-entry))
	      (push (list generator entry) *dfun-constructors*))))))

(defmacro precompile-dfun-constructors (&optional system)
  (let ((*precompiling-lap* t))
    `(progn
       ,@(gathering1 (collecting)
	   (dolist (generator-entry *dfun-constructors*)
	     (dolist (args-entry (cdr generator-entry))
	       (when (or (null (caddr args-entry))
			 (eq (caddr args-entry) system))
		 (when system (setf (caddr args-entry) system))
		 (gather1
		   (make-top-level-form `(precompile-dfun-constructor 
					  ,(car generator-entry))
					'(load)
		     `(load-precompiled-dfun-constructor
		       ',(car generator-entry)
		       ',(car args-entry)
		       ',system
		       ,(apply (symbol-function (car generator-entry))
			       (car args-entry))))))))))))


;;;
;;; When all the methods of a generic function are automatically generated
;;; reader or writer methods a number of special optimizations are possible.
;;; These are important because of the large number of generic functions of
;;; this type.
;;;
;;; There are a number of cases:
;;;
;;;   ONE-CLASS-ACCESSOR
;;;     In this case, the accessor generic function has only been called
;;;     with one class of argument.  There is no cache vector, the wrapper
;;;     of the one class, and the slot index are stored directly as closure
;;;     variables of the discriminating function.  This case can convert to
;;;     either of the next kind.
;;;
;;;   TWO-CLASS-ACCESSOR
;;;     Like above, but two classes.  This is common enough to do specially.
;;;     There is no cache vector.  The two classes are stored a separate
;;;     closure variables.
;;;
;;;   ONE-INDEX-ACCESSOR
;;;     In this case, the accessor generic function has seen more than one
;;;     class of argument, but the index of the slot is the same for all
;;;     the classes that have been seen.  A cache vector is used to store
;;;     the wrappers that have been seen, the slot index is stored directly
;;;     as a closure variable of the discriminating function.  This case
;;;     can convert to the next kind.
;;;
;;;   N-N-ACCESSOR
;;;     This is the most general case.  In this case, the accessor generic
;;;     function has seen more than one class of argument and more than one
;;;     slot index.  A cache vector stores the wrappers and corresponding
;;;     slot indexes.  Because each cache line is more than one element
;;;     long, a cache lock count is used.
;;;
(defstruct (dfun-info
	     (:constructor nil)
	     (:print-function print-dfun-info))
  (cache nil))

(defun print-dfun-info (dfun-info stream depth)
  (declare (ignore depth) (stream stream))
  (printing-random-thing (dfun-info stream)
    (format stream "~A" (type-of dfun-info))))

(defstruct (no-methods
	     (:constructor no-methods-dfun-info ())
	     (:include dfun-info)))

(defstruct (initial
	     (:constructor initial-dfun-info ())
	     (:include dfun-info)))

(defstruct (initial-dispatch
	     (:constructor initial-dispatch-dfun-info ())
	     (:include dfun-info)))

(defstruct (dispatch
	     (:constructor dispatch-dfun-info ())
	     (:include dfun-info)))

(defstruct (default-method-only
	     (:constructor default-method-only-dfun-info ())
	     (:include dfun-info)))

;without caching:
;  dispatch one-class two-class default-method-only

;with caching:
;  one-index n-n checking caching

;accessor:
;  one-class two-class one-index n-n
(defstruct (accessor-dfun-info
	     (:constructor nil)
	     (:include dfun-info))
  accessor-type) ; (member reader writer)

(defmacro dfun-info-accessor-type (di)
  `(accessor-dfun-info-accessor-type ,di))

(defstruct (one-index-dfun-info
	     (:constructor nil)
	     (:include accessor-dfun-info))
  index)

(defmacro dfun-info-index (di)
  `(one-index-dfun-info-index ,di))

(defstruct (n-n
	     (:constructor n-n-dfun-info (accessor-type cache))
	     (:include accessor-dfun-info)))

(defstruct (one-class
	     (:constructor one-class-dfun-info (accessor-type index wrapper0))
	     (:include one-index-dfun-info))
  wrapper0)

(defmacro dfun-info-wrapper0 (di)
  `(one-class-wrapper0 ,di))

(defstruct (two-class
	     (:constructor two-class-dfun-info (accessor-type index wrapper0 wrapper1))
	     (:include one-class))
  wrapper1)

(defmacro dfun-info-wrapper1 (di)
  `(two-class-wrapper1 ,di))

(defstruct (one-index
	     (:constructor one-index-dfun-info
			   (accessor-type index cache))
	     (:include one-index-dfun-info)))	     

(defstruct (checking
	     (:constructor checking-dfun-info (function cache))
	     (:include dfun-info))
  function)

(defmacro dfun-info-function (di)
  `(checking-function ,di))

(defstruct (caching
	     (:constructor caching-dfun-info (cache))
	     (:include dfun-info)))

(defstruct (constant-value
	     (:constructor constant-value-dfun-info (cache))
	     (:include dfun-info)))

(defmacro dfun-update (generic-function function &rest args)
  `(multiple-value-bind (dfun cache info)
       (funcall ,function ,generic-function ,@args)
     (update-dfun ,generic-function dfun cache info)))

(defun accessor-miss-function (gf dfun-info)
  (ecase (dfun-info-accessor-type dfun-info)
    (reader
      #'(lambda (arg)
	   (declare (pcl-fast-call))
	   (accessor-miss gf nil arg dfun-info)))
    (writer
     #'(lambda (new arg)
	 (declare (pcl-fast-call))
	 (accessor-miss gf new arg dfun-info)))))

;;;
;;; ONE-CLASS-ACCESSOR
;;;
(defun make-one-class-accessor-dfun (gf type wrapper index)
  (let ((emit (if (eq type 'reader) 'emit-one-class-reader 'emit-one-class-writer))
	(dfun-info (one-class-dfun-info type index wrapper)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      wrapper index
	      (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

;;;
;;; TWO-CLASS-ACCESSOR
;;;
(defun make-two-class-accessor-dfun (gf type w0 w1 index)
  (let ((emit (if (eq type 'reader) 'emit-two-class-reader 'emit-two-class-writer))
	(dfun-info (two-class-dfun-info type index w0 w1)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      w0 w1 index
	      (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

;;;
;;; std accessors same index dfun
;;;
(defun make-one-index-accessor-dfun (gf type index &optional cache)
  (let* ((emit (if (eq type 'reader) 'emit-one-index-readers 'emit-one-index-writers))
	 (cache (or cache (get-cache 1 nil #'one-index-limit-fn 4)))
	 (dfun-info (one-index-dfun-info type index cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      cache
	      index
	      (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(defun make-final-one-index-accessor-dfun (gf type index table)
  (let ((cache (fill-dfun-cache table nil 1 #'one-index-limit-fn)))
    (make-one-index-accessor-dfun gf type index cache)))

(defun one-index-limit-fn (nlines)
  (default-limit-fn nlines))


(defun make-n-n-accessor-dfun (gf type &optional cache)
  (let* ((emit (if (eq type 'reader) 'emit-n-n-readers 'emit-n-n-writers))
	 (cache (or cache (get-cache 1 t #'n-n-accessors-limit-fn 2)))
	 (dfun-info (n-n-dfun-info type cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit)
	      cache
	      (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(defun make-final-n-n-accessor-dfun (gf type table)
  (let ((cache (fill-dfun-cache table t 1 #'n-n-accessors-limit-fn)))
    (make-n-n-accessor-dfun gf type cache)))

(defun n-n-accessors-limit-fn (nlines)
  (default-limit-fn nlines))

(defun make-checking-dfun (generic-function function &optional cache)
  (unless cache
    (when (use-caching-dfun-p generic-function)
      (return-from make-checking-dfun (make-caching-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-checking-dfun (make-dispatch-dfun generic-function))))
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info generic-function)
    (declare (ignore nreq))
    (if (every #'(lambda (mt) (eq mt 't)) metatypes)
	(let ((dfun-info (default-method-only-dfun-info)))
	  (values 
	   (funcall (get-dfun-constructor 'emit-default-only metatypes applyp)
		    function)
	   nil
	   dfun-info))
	(let* ((cache (or cache (get-cache nkeys nil #'checking-limit-fn 2)))
	       (dfun-info (checking-dfun-info function cache)))
	  (values
	   (funcall (get-dfun-constructor 'emit-checking metatypes applyp)
		    cache
		    function 
		    #'(lambda (&rest args)
			(declare (pcl-fast-call))
			#+copy-&rest-arg (setq args (copy-list args))
			(checking-miss generic-function args dfun-info)))
	   cache
	   dfun-info)))))

(defun make-final-checking-dfun (generic-function function
						  classes-list new-class)
  (let ((metatypes (arg-info-metatypes (gf-arg-info generic-function))))
    (if (every #'(lambda (mt) (eq mt 't)) metatypes)
	(values #'(lambda (&rest args)
		    #+copy-&rest-arg (setq args (copy-list args))
		    (invoke-emf function args))
		nil (default-method-only-dfun-info))
	(let ((cache (make-final-ordinary-dfun-internal 
		      generic-function nil #'checking-limit-fn 
		      classes-list new-class)))
	  (make-checking-dfun generic-function function cache)))))

(defun use-default-method-only-dfun-p (generic-function)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info generic-function)
    (declare (ignore nreq applyp nkeys))
    (every #'(lambda (mt) (eq mt 't)) metatypes)))

(defun use-caching-dfun-p (generic-function)
  (some #'(lambda (method)
	    (let ((fmf (if (listp method)
			   (third method)
			   (method-fast-function method))))
	      (method-function-get fmf ':slot-name-lists)))
	(if (early-gf-p generic-function)
	    (early-gf-methods generic-function)
	    (generic-function-methods generic-function))))

(defun checking-limit-fn (nlines)
  (default-limit-fn nlines))


;;;
;;;
;;;
(defun make-caching-dfun (generic-function &optional cache)
  (unless cache
    (when (use-constant-value-dfun-p generic-function)
      (return-from make-caching-dfun (make-constant-value-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-caching-dfun (make-dispatch-dfun generic-function))))
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info generic-function)
    (declare (ignore nreq))
    (let* ((cache (or cache (get-cache nkeys t #'caching-limit-fn 2)))
	   (dfun-info (caching-dfun-info cache)))
      (values
       (funcall (get-dfun-constructor 'emit-caching metatypes applyp)
		cache
		#'(lambda (&rest args)
		    (declare (pcl-fast-call))
		    #+copy-&rest-arg (setq args (copy-list args))
		    (caching-miss generic-function args dfun-info)))
       cache
       dfun-info))))

(defun make-final-caching-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-internal 
		generic-function t #'caching-limit-fn
		classes-list new-class)))
    (make-caching-dfun generic-function cache)))

(defun caching-limit-fn (nlines)
  (default-limit-fn nlines))

(defun insure-caching-dfun (gf)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info gf)
    (declare (ignore nreq nkeys))
    (when (and metatypes
	       (not (null (car metatypes)))
	       (dolist (mt metatypes nil)
		 (unless (eq mt 't) (return t))))
      (get-dfun-constructor 'emit-caching metatypes applyp))))

(defun use-constant-value-dfun-p (gf &optional boolean-values-p)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info gf)
    (declare (ignore nreq metatypes nkeys))
    (let* ((early-p (early-gf-p gf))
	   (methods (if early-p
			(early-gf-methods gf)
			(generic-function-methods gf)))
	   (default '(unknown)))
      (and (null applyp)
	   (or (not (eq *boot-state* 'complete))
	       (compute-applicable-methods-emf-std-p gf))
	   (notany #'(lambda (method)
		       (or (and (eq *boot-state* 'complete)
				(some #'eql-specializer-p
				      (method-specializers method)))
			   (let ((value (method-function-get 
					 (if early-p
					     (or (third method) (second method))
					     (or (method-fast-function method)
						 (method-function method)))
					 :constant-value default)))
			     (if boolean-values-p
				 (not (or (eq value 't) (eq value nil)))
				 (eq value default)))))
		   methods)))))

(defun make-constant-value-dfun (generic-function &optional cache)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info generic-function)
    (declare (ignore nreq applyp))
    (let* ((cache (or cache (get-cache nkeys t #'caching-limit-fn 2)))
	   (dfun-info (constant-value-dfun-info cache)))
      (values
       (funcall (get-dfun-constructor 'emit-constant-value metatypes)
		cache
		#'(lambda (&rest args)
		    (declare (pcl-fast-call))
		    #+copy-&rest-arg (setq args (copy-list args))
		    (constant-value-miss generic-function args dfun-info)))
       cache
       dfun-info))))

(defun make-final-constant-value-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-internal 
		generic-function :constant-value #'caching-limit-fn
		classes-list new-class)))
    (make-constant-value-dfun generic-function cache)))

(defun use-dispatch-dfun-p (gf &optional (caching-p (use-caching-dfun-p gf)))
  (when (eq *boot-state* 'complete)
    (unless caching-p
      (let* ((methods (generic-function-methods gf))
	     (arg-info (gf-arg-info gf))
	     (mt (arg-info-metatypes arg-info))
	     (nreq (length mt)))
	;;Is there a position at which every specializer is eql or non-standard?
	(dotimes (i nreq nil)
	  (when (not (eq 't (nth i mt)))
	    (let ((some-std-class-specl-p nil))
	      (dolist (method methods)
		(let ((specl (nth i (method-specializers method))))
		  (when (and (not (eql-specializer-p specl))
			     (let ((sclass (specializer-class specl)))
			       (or (null (class-finalized-p sclass))
				   (member *the-class-standard-object*
					   (class-precedence-list sclass)))))
		    (setq some-std-class-specl-p t))))
	      (unless some-std-class-specl-p
		(return-from use-dispatch-dfun-p t)))))))))

(defun make-dispatch-dfun (gf)
  (values (get-dispatch-function gf) nil (dispatch-dfun-info)))

(defun get-dispatch-function (gf)
  (let ((methods (generic-function-methods gf)))
    (function-funcall (get-secondary-dispatch-function1 gf methods nil nil nil 
							nil nil t)
		      nil nil)))

(defun make-final-dispatch-dfun (gf)
  (make-dispatch-dfun gf))

(defun update-dispatch-dfuns ()
  (dolist (gf (gfs-of-type '(dispatch initial-dispatch)))
    (dfun-update gf #'make-dispatch-dfun)))

(defun fill-dfun-cache (table valuep nkeys limit-fn &optional cache)
  (let ((cache (or cache (get-cache nkeys valuep limit-fn
				    (+ (hash-table-count table) 3)))))
    (maphash #'(lambda (classes value)
		 (setq cache (fill-cache cache
					 (class-wrapper classes)
					 value
					 t)))
	     table)
    cache))

(defun make-final-ordinary-dfun-internal (generic-function valuep limit-fn
							   classes-list new-class)
  (let* ((arg-info (gf-arg-info generic-function))
	 (nkeys (arg-info-nkeys arg-info))
	 (new-class (and new-class
			 (equal (type-of (gf-dfun-info generic-function))
				(cond ((eq valuep t) 'caching)
				      ((eq valuep :constant-value) 'constant-value)
				      ((null valuep) 'checking)))
			 new-class))
	 (cache (if new-class
		    (copy-cache (gf-dfun-cache generic-function))
		    (get-cache nkeys (not (null valuep)) limit-fn 4))))
      (make-emf-cache generic-function valuep cache classes-list new-class)))

(defvar *dfun-miss-gfs-on-stack* ())

(defmacro dfun-miss ((gf args wrappers invalidp nemf
		      &optional type index caching-p applicable)
		     &body body)
  (unless applicable (setq applicable (gensym)))
  `(multiple-value-bind (,nemf ,applicable ,wrappers ,invalidp 
			 ,@(when type `(,type ,index)))
       (cache-miss-values ,gf ,args ',(cond (caching-p 'caching)
					    (type 'accessor)
					    (t 'checking)))
     (when (and ,applicable (not (memq ,gf *dfun-miss-gfs-on-stack*)))
       (let ((*dfun-miss-gfs-on-stack* (cons ,gf *dfun-miss-gfs-on-stack*)))
	 ,@body))
     (invoke-emf ,nemf ,args)))

;;;
;;; The dynamically adaptive method lookup algorithm is implemented is
;;; implemented as a kind of state machine.  The kinds of discriminating
;;; function is the state, the various kinds of reasons for a cache miss
;;; are the state transitions.
;;;
;;; The code which implements the transitions is all in the miss handlers
;;; for each kind of dfun.  Those appear here.
;;;
;;; Note that within the states that cache, there are dfun updates which
;;; simply select a new cache or cache field.  Those are not considered
;;; as state transitions.
;;; 
(defvar *lazy-dfun-compute-p* t)
(defvar *early-p* nil)

(defun make-initial-dfun (gf)
  (let ((initial-dfun 
	 #'(lambda (&rest args)
	     #+copy-&rest-arg (setq args (copy-list args))
	     (initial-dfun gf args))))
    (multiple-value-bind (dfun cache info)
	(if (and (eq *boot-state* 'complete)
		 (compute-applicable-methods-emf-std-p gf))
	    (let* ((caching-p (use-caching-dfun-p gf))
		   (classes-list (precompute-effective-methods 
				  gf caching-p
				  (not *lazy-dfun-compute-p*))))
	      (if *lazy-dfun-compute-p*
		  (cond ((use-dispatch-dfun-p gf caching-p)
			 (values initial-dfun nil (initial-dispatch-dfun-info)))
			(caching-p
			 (insure-caching-dfun gf)
			 (values initial-dfun nil (initial-dfun-info)))
			(t
			 (values initial-dfun nil (initial-dfun-info))))
		  (make-final-dfun-internal gf classes-list)))
	    (let ((arg-info (if (early-gf-p gf)
				(early-gf-arg-info gf)
				(gf-arg-info gf)))
		  (type nil))
	      (if (and (gf-precompute-dfun-and-emf-p arg-info)
		       (setq type (final-accessor-dfun-type gf)))
		  (if *early-p*
		      (values (make-early-accessor gf type) nil nil)
		      (make-final-accessor-dfun gf type))
		  (values initial-dfun nil (initial-dfun-info)))))
      (set-dfun gf dfun cache info))))

(defun make-early-accessor (gf type)
  (let* ((methods (early-gf-methods gf))
	 (slot-name (early-method-standard-accessor-slot-name (car methods))))
    (ecase type
      (reader #'(lambda (instance)
		  (let* ((class (class-of instance))
			 (class-name (bootstrap-get-slot 'class class 'name)))
		    (bootstrap-get-slot class-name instance slot-name))))
      (writer #'(lambda (new-value instance)
		  (let* ((class (class-of instance))
			 (class-name (bootstrap-get-slot 'class class 'name)))
		    (bootstrap-set-slot class-name instance slot-name new-value)))))))

(defun initial-dfun (gf args)
  (dfun-miss (gf args wrappers invalidp nemf ntype nindex)
    (cond (invalidp)
	  ((and ntype nindex)
	   (dfun-update 
	    gf #'make-one-class-accessor-dfun ntype wrappers nindex))
	  ((use-caching-dfun-p gf)
	   (dfun-update gf #'make-caching-dfun))
	  (t
	   (dfun-update 
	    gf #'make-checking-dfun
	    ;; nemf is suitable only for caching, have to do this:
	    (cache-miss-values gf args 'checking))))))

(defun make-final-dfun (gf &optional classes-list)
  (multiple-value-bind (dfun cache info)
      (make-final-dfun-internal gf classes-list)
    (set-dfun gf dfun cache info)))

(defvar *new-class* nil)

(defvar *free-hash-tables* (mapcar #'list '(eq equal eql)))

(defmacro with-hash-table ((table test) &body forms)
  `(let* ((.free. (assoc ',test *free-hash-tables*))
	  (,table (if (cdr .free.)
		      (pop (cdr .free.))
		      (make-hash-table :test ',test))))
     (multiple-value-prog1
	 (progn ,@forms)
       (clrhash ,table)
       (push ,table (cdr .free.)))))

(defmacro with-eq-hash-table ((table) &body forms)
  `(with-hash-table (,table eq) ,@forms))

(defun final-accessor-dfun-type (gf)
  (let ((methods (if (early-gf-p gf)
		     (early-gf-methods gf)
		     (generic-function-methods gf))))
    (cond ((every #'(lambda (method) 
		      (if (consp method)
			  (eq *the-class-standard-reader-method*
			      (early-method-class method))
			  (standard-reader-method-p method)))
		  methods)
	   'reader)
	  ((every #'(lambda (method) 
		      (if (consp method)
			  (eq *the-class-standard-writer-method*
			      (early-method-class method))
			  (standard-writer-method-p method)))
		  methods)
	   'writer))))

(defun make-final-accessor-dfun (gf type &optional classes-list new-class)
  (with-eq-hash-table (table)
    (multiple-value-bind (table all-index first second size no-class-slots-p)
	(make-accessor-table gf type table)
      (if table
	  (cond ((= size 1)
		 (let ((w (class-wrapper first)))
		   (make-one-class-accessor-dfun gf type w all-index)))
		((and (= size 2) (or (integerp all-index) (consp all-index)))
		 (let ((w0 (class-wrapper first))
		       (w1 (class-wrapper second)))
		   (make-two-class-accessor-dfun gf type w0 w1 all-index)))
		((or (integerp all-index) (consp all-index))
		 (make-final-one-index-accessor-dfun 
		  gf type all-index table))
		(no-class-slots-p
		 (make-final-n-n-accessor-dfun gf type table))
		(t
		 (make-final-caching-dfun gf classes-list new-class)))
	  (make-final-caching-dfun gf classes-list new-class)))))

(defun make-final-dfun-internal (gf &optional classes-list)
  (let ((methods (generic-function-methods gf)) type
	(new-class *new-class*) (*new-class* nil)
	specls all-same-p)
    (cond ((null methods)
	   (values
	    #'(lambda (&rest args)
		(apply #'no-applicable-method gf args))
	    nil
	    (no-methods-dfun-info)))
	  ((setq type (final-accessor-dfun-type gf))
	   (make-final-accessor-dfun gf type classes-list new-class))
	  ((and (not (and (every #'(lambda (specl) (eq specl *the-class-t*))
				 (setq specls (method-specializers (car methods))))
			  (setq all-same-p
				(every #'(lambda (method)
					   (and (equal specls
						       (method-specializers method))))
				       methods))))
		(use-constant-value-dfun-p gf))
	   (make-final-constant-value-dfun gf classes-list new-class))
	  ((use-dispatch-dfun-p gf)
	   (make-final-dispatch-dfun gf))
	  ((and all-same-p (not (use-caching-dfun-p gf)))
	   (let ((emf (get-secondary-dispatch-function gf methods nil)))
	     (make-final-checking-dfun gf emf classes-list new-class)))
	  (t
	   (make-final-caching-dfun gf classes-list new-class)))))

(defun accessor-miss (gf new object dfun-info)
  (let* ((ostate (type-of dfun-info))
	 (otype (dfun-info-accessor-type dfun-info))
	 oindex ow0 ow1 cache
	 (args (ecase otype			;The congruence rules assure
		(reader (list object))		;us that this is safe despite
		(writer (list new object)))))	;not knowing the new type yet.
    (dfun-miss (gf args wrappers invalidp nemf ntype nindex)
      ;;
      ;; The following lexical functions change the state of the
      ;; dfun to that which is their name.  They accept arguments
      ;; which are the parameters of the new state, and get other
      ;; information from the lexical variables bound above.
      ;; 
      (flet ((two-class (index w0 w1)
	       (when (zerop (random 2)) (psetf w0 w1 w1 w0))
	       (dfun-update gf #'make-two-class-accessor-dfun ntype w0 w1 index))
	     (one-index (index &optional cache)
	       (dfun-update gf #'make-one-index-accessor-dfun ntype index cache))
	     (n-n (&optional cache)
	       (if (consp nindex)
		   (dfun-update gf #'make-checking-dfun nemf)
		   (dfun-update gf #'make-n-n-accessor-dfun ntype cache)))
	     (caching () ; because cached accessor emfs are much faster for accessors
	       (dfun-update gf #'make-caching-dfun))
	     ;;
	     (do-fill (update-fn)
	       (let ((ncache (fill-cache cache wrappers nindex)))
		 (unless (eq ncache cache)
		   (funcall update-fn ncache)))))
	(cond ((null ntype)
	       (caching))
	      ((or invalidp
		   (null nindex)))
	      ((not (or (std-instance-p object)
			(fsc-instance-p object)))
	       (caching))
	      ((or (neq ntype otype) (listp wrappers))
	       (caching))
	      (t
	       (ecase ostate
		 (one-class
		  (setq oindex (dfun-info-index dfun-info))
		  (setq ow0 (dfun-info-wrapper0 dfun-info))
		  (unless (eq ow0 wrappers)
		    (if (eql nindex oindex)
			(two-class nindex ow0 wrappers)
			(n-n))))
		 (two-class
		  (setq oindex (dfun-info-index dfun-info))
		  (setq ow0 (dfun-info-wrapper0 dfun-info))
		  (setq ow1 (dfun-info-wrapper1 dfun-info))
		  (unless (or (eq ow0 wrappers) (eq ow1 wrappers))
		    (if (eql nindex oindex)
			(one-index nindex)
			(n-n))))
		 (one-index
		  (setq oindex (dfun-info-index dfun-info))
		  (setq cache (dfun-info-cache dfun-info))
		  (if (eql nindex oindex)
		      (do-fill #'(lambda (ncache)
				   (one-index nindex ncache)))
		      (n-n)))
		 (n-n
		  (setq cache (dfun-info-cache dfun-info))
		  (if (consp nindex)
		      (caching)
		      (do-fill #'n-n))))))))))

(defun checking-miss (generic-function args dfun-info)
  (let ((oemf (dfun-info-function dfun-info))
	(cache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp nemf)
      (cond (invalidp)
	    ((eq oemf nemf)
	     (let ((ncache (fill-cache cache wrappers nil)))
	       (unless (eq ncache cache)
		 (dfun-update generic-function #'make-checking-dfun 
			      nemf ncache))))
	    (t
	     (dfun-update generic-function #'make-caching-dfun))))))

(defun caching-miss (generic-function args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp emf nil nil t)
      (cond (invalidp)
	    (t
	     (let ((ncache (fill-cache ocache wrappers emf)))
	       (unless (eq ncache ocache)
		 (dfun-update generic-function 
			      #'make-caching-dfun ncache))))))))

(defun constant-value-miss (generic-function args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp emf nil nil t)
      (cond (invalidp)
	    (t
	     (let* ((function (typecase emf
				(fast-method-call (fast-method-call-function emf))
				(method-call (method-call-function emf))))
		    (value (method-function-get function :constant-value))
		    (ncache (fill-cache ocache wrappers value)))
	       (unless (eq ncache ocache)
		 (dfun-update generic-function
			      #'make-constant-value-dfun ncache))))))))

;;; Given a generic function and a set of arguments to that generic function,
;;; returns a mess of values.
;;;
;;;  <function>   The compiled effective method function for this set of
;;;               arguments. 
;;;
;;;  <applicable> Sorted list of applicable methods. 
;;;
;;;  <wrappers>   Is a single wrapper if the generic function has only
;;;               one key, that is arg-info-nkeys of the arg-info is 1.
;;;               Otherwise a list of the wrappers of the specialized
;;;               arguments to the generic function.
;;;
;;;               Note that all these wrappers are valid.  This function
;;;               does invalid wrapper traps when it finds an invalid
;;;               wrapper and then returns the new, valid wrapper.
;;;
;;;  <invalidp>   True if any of the specialized arguments had an invalid
;;;               wrapper, false otherwise.
;;;
;;;  <type>       READER or WRITER when the only method that would be run
;;;               is a standard reader or writer method.  To be specific,
;;;               the value is READER when the method combination is eq to
;;;               *standard-method-combination*; there are no applicable
;;;               :before, :after or :around methods; and the most specific
;;;               primary method is a standard reader method.
;;;
;;;  <index>      If <type> is READER or WRITER, and the slot accessed is
;;;               an :instance slot, this is the index number of that slot
;;;               in the object argument.
;;;
(defun cache-miss-values (gf args state)
  (if (null (if (early-gf-p gf)
		(early-gf-methods gf)
		(generic-function-methods gf)))
      (apply #'no-applicable-method gf args)
      (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
	  (get-generic-function-info gf)
	(declare (ignore nreq applyp nkeys))
	(with-dfun-wrappers (args metatypes)
	  (dfun-wrappers invalid-wrapper-p wrappers classes types)
	  (error "The function ~S requires at least ~D arguments"
		 gf (length metatypes))
	  (multiple-value-bind (emf methods accessor-type index)
	      (cache-miss-values-internal gf arg-info wrappers classes types state)
	    (values emf methods
		    dfun-wrappers
		    invalid-wrapper-p
		    accessor-type index))))))

(defun cache-miss-values-internal (gf arg-info wrappers classes types state)
  (let* ((for-accessor-p (eq state 'accessor))
	 (for-cache-p (or (eq state 'caching) (eq state 'accessor)))
	 (cam-std-p (or (null arg-info)
			(gf-info-c-a-m-emf-std-p arg-info))))
    (multiple-value-bind (methods all-applicable-and-sorted-p)
	(if cam-std-p
	    (compute-applicable-methods-using-types gf types)
	    (compute-applicable-methods-using-classes gf classes))
      (let ((emf (if (or cam-std-p all-applicable-and-sorted-p)
		     (function-funcall (get-secondary-dispatch-function1
					gf methods types nil (and for-cache-p wrappers)
					all-applicable-and-sorted-p)
				       nil (and for-cache-p wrappers))
		     (default-secondary-dispatch-function gf))))
	(multiple-value-bind (index accessor-type)
	    (and for-accessor-p all-applicable-and-sorted-p methods
		 (accessor-values gf arg-info classes methods))
	  (values (if (integerp index) index emf)
		  methods accessor-type index))))))

(defun accessor-values (gf arg-info classes methods)
  (declare (ignore gf))
  (let* ((accessor-type (gf-info-simple-accessor-type arg-info))
	 (accessor-class (case accessor-type
			   (reader (car classes))
			   (writer (cadr classes))
			   (boundp (car classes)))))
    (accessor-values-internal accessor-type accessor-class methods)))

(defun accessor-values1 (gf accessor-type accessor-class)
  (let* ((type `(class-eq ,accessor-class))
	 (types (if (eq accessor-type 'writer) `(t ,type) `(,type)))
	 (methods (compute-applicable-methods-using-types gf types)))
    (accessor-values-internal accessor-type accessor-class methods)))

(defun accessor-values-internal (accessor-type accessor-class methods)
  (let* ((meth (car methods))
	 (early-p (not (eq *boot-state* 'complete)))
	 (slot-name (when accessor-class
		      (if (consp meth)
			  (and (early-method-standard-accessor-p meth)
			       (early-method-standard-accessor-slot-name meth))
			  (and (member *the-class-standard-object*
				       (if early-p
					   (early-class-precedence-list accessor-class)
					   (class-precedence-list accessor-class)))
			       (if early-p
				   (not (eq *the-class-standard-method*
					    (early-method-class meth)))
				   (standard-accessor-method-p meth))
			       (if early-p
				   (early-accessor-method-slot-name meth)
				   (accessor-method-slot-name meth))))))
	 (slotd (and accessor-class
		     (if early-p
			 (dolist (slot (early-class-slotds accessor-class) nil)
			   (when (eql slot-name (early-slot-definition-name slot))
			     (return slot)))
			 (find-slot-definition accessor-class slot-name)))))
    (when (and slotd
	       (or early-p
		   (slot-accessor-std-p slotd accessor-type)))
      (values (if early-p
		  (early-slot-definition-location slotd) 
		  (slot-definition-location slotd))
	      accessor-type))))

(defun make-accessor-table (gf type &optional table)
  (unless table (setq table (make-hash-table :test 'eq)))
  (let ((methods (if (early-gf-p gf)
		     (early-gf-methods gf)
		     (generic-function-methods gf)))
	(all-index nil)
	(no-class-slots-p t)
	(early-p (not (eq *boot-state* 'complete)))
	first second (size 0))
    (declare (fixnum size))
    ;; class -> {(specl slotd)}
    (dolist (method methods)
      (let* ((specializers (if (consp method)
			       (early-method-specializers method t)
			       (method-specializers method)))
	     (specl (if (eq type 'reader)
			(car specializers)
			(cadr specializers)))
	     (specl-cpl (if early-p
			    (early-class-precedence-list specl)
			    (and (class-finalized-p specl)
				 (class-precedence-list specl))))
	     (so-p (member *the-class-standard-object* specl-cpl))
	     (slot-name (if (consp method)
			    (and (early-method-standard-accessor-p method)
				 (early-method-standard-accessor-slot-name method))
			    (accessor-method-slot-name method))))
	(when (or (null specl-cpl)
		  (member *the-class-structure-object* specl-cpl))
	  (return-from make-accessor-table nil))
	(maphash #'(lambda (class slotd)
		     (let ((cpl (if early-p
				    (early-class-precedence-list class)
				    (class-precedence-list class))))
		       (when (memq specl cpl)
			 (unless (and (or so-p
					  (member *the-class-standard-object* cpl))
				      (or early-p
					  (slot-accessor-std-p slotd type)))
			   (return-from make-accessor-table nil))
			 (push (cons specl slotd) (gethash class table)))))
		 (gethash slot-name *name->class->slotd-table*))))
    (maphash #'(lambda (class specl+slotd-list)
		 (dolist (sclass (if early-p
				    (early-class-precedence-list class)
				    (class-precedence-list class)) 
			  (error "This can't happen"))
		   (let ((a (assq sclass specl+slotd-list)))
		     (when a
		       (let* ((slotd (cdr a))
			      (index (if early-p
					 (early-slot-definition-location slotd) 
					 (slot-definition-location slotd))))
			 (unless index (return-from make-accessor-table nil))
			 (setf (gethash class table) index)
			 (when (consp index) (setq no-class-slots-p nil))
			 (setq all-index (if (or (null all-index)
						 (eql all-index index))
					     index t))
			 (incf size)
			 (cond ((= size 1) (setq first class))
			       ((= size 2) (setq second class)))
			 (return nil))))))
	     table)
    (values table all-index first second size no-class-slots-p)))

(defun compute-applicable-methods-using-types (generic-function types)
  (let ((definite-p t) (possibly-applicable-methods nil))
    (dolist (method (if (early-gf-p generic-function)
			(early-gf-methods generic-function)
			(generic-function-methods generic-function)))
      (let ((specls (if (consp method)
			(early-method-specializers method t)
			(method-specializers method)))
	    (types types)
	    (possibly-applicable-p t) (applicable-p t))
	(dolist (specl specls)
	  (multiple-value-bind (specl-applicable-p specl-possibly-applicable-p)
	      (specializer-applicable-using-type-p specl (pop types))
	    (unless specl-applicable-p
	      (setq applicable-p nil))
	    (unless specl-possibly-applicable-p
	      (setq possibly-applicable-p nil)
	      (return nil))))
	(when possibly-applicable-p
	  (unless applicable-p (setq definite-p nil))
	  (push method possibly-applicable-methods))))
    (let ((precedence (arg-info-precedence (if (early-gf-p generic-function)
					       (early-gf-arg-info generic-function)
					       (gf-arg-info generic-function)))))
      (values (sort-applicable-methods precedence
				       (nreverse possibly-applicable-methods)
				       types)
	      definite-p))))

(defun sort-applicable-methods (precedence methods types)
  (sort-methods methods
		precedence
		#'(lambda (class1 class2 index)
		    (let* ((class (type-class (nth index types)))
			   (cpl (if (eq *boot-state* 'complete)
				    (class-precedence-list class)
				    (early-class-precedence-list class))))
		      (if (memq class2 (memq class1 cpl))
			  class1 class2)))))

(defun sort-methods (methods precedence compare-classes-function)
  (flet ((sorter (method1 method2)
	   (dolist (index precedence)
	     (let* ((specl1 (nth index (if (listp method1)
					   (early-method-specializers method1 t)
					   (method-specializers method1))))
		    (specl2 (nth index (if (listp method2)
					   (early-method-specializers method2 t)
					   (method-specializers method2))))
		    (order (order-specializers
			     specl1 specl2 index compare-classes-function)))
	       (when order
		 (return-from sorter (eq order specl1)))))))
    (stable-sort methods #'sorter)))

(defun order-specializers (specl1 specl2 index compare-classes-function)
  (let ((type1 (if (eq *boot-state* 'complete)
		   (specializer-type specl1)
		   (bootstrap-get-slot 'specializer specl1 'type)))
	(type2 (if (eq *boot-state* 'complete)
		   (specializer-type specl2)
		   (bootstrap-get-slot 'specializer specl2 'type))))
    (cond ((eq specl1 specl2)
	   nil)
	  ((atom type1)
	   specl2)
	  ((atom type2)
	   specl1)
	  (t
	   (case (car type1)
	     (class    (case (car type2)
			 (class (funcall compare-classes-function specl1 specl2 index))
			 (t specl2)))
	     (prototype (case (car type2)
			 (class (funcall compare-classes-function specl1 specl2 index))
			 (t specl2)))
	     (class-eq (case (car type2)
			 (eql specl2)
			 (class-eq nil)
			 (class type1)))
	     (eql      (case (car type2)
			 (eql nil)
			 (t specl1))))))))

(defun map-all-orders (methods precedence function)
  (let ((choices nil))
    (flet ((compare-classes-function (class1 class2 index)
	     (declare (ignore index))
	     (let ((choice nil))
	       (dolist (c choices nil)
		 (when (or (and (eq (first c) class1)
				(eq (second c) class2))
			   (and (eq (first c) class2)
				(eq (second c) class1)))
		   (return (setq choice c))))
	       (unless choice
		 (setq choice
		       (if (class-might-precede-p class1 class2)
			   (if (class-might-precede-p class2 class1)
			       (list class1 class2 nil t)
			       (list class1 class2 t))
			   (if (class-might-precede-p class2 class1)
			       (list class2 class1 t)
			       (let ((name1 (class-name class1))
				     (name2 (class-name class2)))
				 (if (and name1 name2 (symbolp name1) (symbolp name2)
					  (string< (symbol-name name1)
						   (symbol-name name2)))
				     (list class1 class2 t)
				     (list class2 class1 t))))))
		 (push choice choices))
	       (car choice))))
      (loop (funcall function
		     (sort-methods methods precedence #'compare-classes-function))
	    (unless (dolist (c choices nil)
		      (unless (third c)
			(rotatef (car c) (cadr c))
			(return (setf (third c) t))))
	      (return nil))))))

(defvar *in-precompute-effective-methods-p* nil)

;used only in map-all-orders
(defun class-might-precede-p (class1 class2)
  (if (not *in-precompute-effective-methods-p*)
      (not (member class1 (cdr (class-precedence-list class2))))
      (class-can-precede-p class1 class2)))

(defun compute-precedence (lambda-list nreq argument-precedence-order)
  (if (null argument-precedence-order)
      (let ((list nil))(dotimes (i nreq list) (push (- (1- nreq) i) list)))
      (mapcar #'(lambda (x) (position x lambda-list)) argument-precedence-order)))

(defun saut-and (specl type)
  (let ((applicable nil)
	(possibly-applicable t))
    (dolist (type (cdr type))
      (multiple-value-bind (appl poss-appl)
	  (specializer-applicable-using-type-p specl type)
	(when appl (return (setq applicable t)))
	(unless poss-appl (return (setq possibly-applicable nil)))))
    (values applicable possibly-applicable)))

(defun saut-not (specl type)
  (let ((ntype (cadr type)))
    (values nil
	    (case (car ntype)
	      (class      (saut-not-class specl ntype))
	      (class-eq   (saut-not-class-eq specl ntype))
	      (prototype  (saut-not-prototype specl ntype))
	      (eql        (saut-not-eql specl ntype))
	      (t (error "~s cannot handle the second argument ~s"
			'specializer-applicable-using-type-p type))))))

(defun saut-not-class (specl ntype)
  (let* ((class (type-class specl))
	 (cpl (class-precedence-list class)))
     (not (memq (cadr ntype) cpl))))

(defun saut-not-prototype (specl ntype)
  (let* ((class (case (car specl)
		  (eql       (class-of (cadr specl)))
		  (class-eq  (cadr specl))
		  (prototype (cadr specl))
		  (class     (cadr specl))))
	 (cpl (class-precedence-list class)))
     (not (memq (cadr ntype) cpl))))

(defun saut-not-class-eq (specl ntype)
  (let ((class (case (car specl)
		 (eql      (class-of (cadr specl)))
		 (class-eq (cadr specl)))))
    (not (eq class (cadr ntype)))))

(defun saut-not-eql (specl ntype)
  (case (car specl)
    (eql (not (eql (cadr specl) (cadr ntype))))
    (t   t)))

(defun class-applicable-using-class-p (specl type)
  (let ((pred (memq specl (if (eq *boot-state* 'complete)
			      (class-precedence-list type)
			      (early-class-precedence-list type)))))
    (values pred
	    (or pred
		(if (not *in-precompute-effective-methods-p*)
		    ;; classes might get common subclass
		    (superclasses-compatible-p specl type)
		    ;; worry only about existing classes
		    (classes-have-common-subclass-p specl type))))))

(defun classes-have-common-subclass-p (class1 class2)
  (or (eq class1 class2)
      (let ((class1-subs (class-direct-subclasses class1)))
	(or (memq class2 class1-subs)
	    (dolist (class1-sub class1-subs nil)
	      (when (classes-have-common-subclass-p class1-sub class2)
		(return t)))))))

(defun saut-class (specl type)
  (case (car specl)
    (class (class-applicable-using-class-p (cadr specl) (cadr type)))
    (t     (values nil (let ((class (type-class specl)))
			 (memq (cadr type)
			       (class-precedence-list class)))))))

(defun saut-class-eq (specl type)
  (if (eq (car specl) 'eql)
      (values nil (eq (class-of (cadr specl)) (cadr type)))
      (let ((pred (case (car specl)
		    (class-eq   
		     (eq (cadr specl) (cadr type)))
		    (class      
		     (or (eq (cadr specl) (cadr type))
			 (memq (cadr specl)
			       (if (eq *boot-state* 'complete)
				   (class-precedence-list (cadr type))
				   (early-class-precedence-list (cadr type)))))))))
	(values pred pred))))

(defun saut-prototype (specl type)
  (declare (ignore specl type))
  (values nil nil)) ; fix this someday

(defun saut-eql (specl type) 
  (let ((pred (case (car specl)
		(eql        (eql (cadr specl) (cadr type)))
		(class-eq   (eq (cadr specl) (class-of (cadr type))))
		(class      (memq (cadr specl)
				  (let ((class (class-of (cadr type))))
				    (if (eq *boot-state* 'complete)
					(class-precedence-list class)
					(early-class-precedence-list class))))))))
    (values pred pred)))

(defun specializer-applicable-using-type-p (specl type)
  (setq specl (type-from-specializer specl))
  (when (eq specl 't)
    (return-from specializer-applicable-using-type-p (values t t)))
  ;; This is used by c-a-m-u-t and generate-discrimination-net-internal,
  ;; and has only what they need.
  (if (or (atom type) (eq (car type) 't))
      (values nil t)
      (case (car type)
	(and        (saut-and specl type))
	(not        (saut-not specl type))
	(class      (saut-class specl type))
	(prototype  (saut-prototype specl type))
	(class-eq   (saut-class-eq specl type))
	(eql        (saut-eql specl type))
	(t          (error "~s cannot handle the second argument ~s"
			   'specializer-applicable-using-type-p type)))))

(defun map-all-classes (function &optional (root 't))
  (let ((braid-p (or (eq *boot-state* 'braid)
		     (eq *boot-state* 'complete))))
    (labels ((do-class (class)
	       (mapc #'do-class 
		     (if braid-p
			 (class-direct-subclasses class)
			 (early-class-direct-subclasses class)))
	       (funcall function class)))
      (do-class (if (symbolp root)
		    (find-class root)
		    root)))))

;;;
;;; NOTE: We are assuming a restriction on user code that the method
;;;       combination must not change once it is connected to the
;;;       generic function.
;;;
;;;       This has to be legal, because otherwise any kind of method
;;;       lookup caching couldn't work.  See this by saying that this
;;;       cache, is just a backing cache for the fast cache.  If that
;;;       cache is legal, this one must be too.
;;;
;;; Don't clear this table!  
(defvar *effective-method-table* (make-hash-table :test 'eq))

(defun get-secondary-dispatch-function (gf methods types &optional 
							 method-alist wrappers)
  (function-funcall (get-secondary-dispatch-function1 
		     gf methods types
		     (not (null method-alist))
		     (not (null wrappers))
		     (not (methods-contain-eql-specializer-p methods)))
		    method-alist wrappers))

(defun get-secondary-dispatch-function1 (gf methods types method-alist-p wrappers-p
					    &optional all-applicable-p
					    (all-sorted-p t) function-p)
  (if (null methods)
      #'(lambda (method-alist wrappers)
	  (declare (ignore method-alist wrappers))
	  #'(lambda (&rest args)
	      (apply #'no-applicable-method gf args)))
      (let* ((key (car methods))
	     (ht-value (or (gethash key *effective-method-table*)
			   (setf (gethash key *effective-method-table*)
				 (cons nil nil)))))
	(if (and (null (cdr methods)) all-applicable-p ; the most common case
		 (null method-alist-p) wrappers-p (not function-p))
	    (or (car ht-value)
		(setf (car ht-value)
		      (get-secondary-dispatch-function2 
		       gf methods types method-alist-p wrappers-p
		       all-applicable-p all-sorted-p function-p)))
	    (let ((akey (list methods
			      (if all-applicable-p 'all-applicable types)
			      method-alist-p wrappers-p function-p)))
	      (or (cdr (assoc akey (cdr ht-value) :test #'equal))
		  (let ((value (get-secondary-dispatch-function2 
				gf methods types method-alist-p wrappers-p
				all-applicable-p all-sorted-p function-p)))
		    (push (cons akey value) (cdr ht-value))
		    value)))))))

(defun get-secondary-dispatch-function2 (gf methods types method-alist-p wrappers-p
					    all-applicable-p all-sorted-p function-p)
  (if (and all-applicable-p all-sorted-p (not function-p))
      (if (eq *boot-state* 'complete)
	  (let* ((combin (generic-function-method-combination gf))
		 (effective (compute-effective-method gf combin methods)))
	    (make-effective-method-function1 gf effective method-alist-p wrappers-p))
	  (let ((effective (standard-compute-effective-method gf nil methods)))
	    (make-effective-method-function1 gf effective method-alist-p wrappers-p)))
      (let ((net (generate-discrimination-net 
		  gf methods types all-sorted-p)))
	(compute-secondary-dispatch-function1 gf net function-p))))

(defun get-effective-method-function (gf methods &optional method-alist wrappers)
  (function-funcall (get-secondary-dispatch-function1 gf methods nil 
						      (not (null method-alist))
						      (not (null wrappers))
						      t)
		    method-alist wrappers))

(defun get-effective-method-function1 (gf methods &optional (sorted-p t))
  (get-secondary-dispatch-function1 gf methods nil nil nil t sorted-p))

(defun methods-contain-eql-specializer-p (methods)
  (and (eq *boot-state* 'complete)
       (dolist (method methods nil)
	 (when (dolist (spec (method-specializers method) nil)
		 (when (eql-specializer-p spec) (return t)))
	   (return t)))))

(defun update-dfun (generic-function &optional dfun cache info)
  (let* ((early-p (early-gf-p generic-function))
	 (gf-name (if early-p
		      (early-gf-name generic-function)
		      (generic-function-name generic-function)))
	 (ocache (gf-dfun-cache generic-function)))
    (set-dfun generic-function dfun cache info)
    (let* ((dfun (if early-p
		     (or dfun (make-initial-dfun generic-function))
		     (compute-discriminating-function generic-function)))
	   (info (gf-dfun-info generic-function)))
      (unless (eq 'default-method-only (type-of info))
	(setq dfun (doctor-dfun-for-the-debugger 
		    generic-function
		    #+cmu dfun #-cmu (set-function-name dfun gf-name))))
      (set-funcallable-instance-function generic-function dfun)
      #+cmu (set-function-name generic-function gf-name)
      (when (and ocache (not (eq ocache cache))) (free-cache ocache))
      dfun)))


(defvar dfun-count nil)
(defvar dfun-list nil)
(defvar *minimum-cache-size-to-list*)

(defun list-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (a (assq sym dfun-list)))
    (unless a
      (push (setq a (list sym)) dfun-list))
    (push (generic-function-name gf) (cdr a))))

(defun list-all-dfuns ()
  (setq dfun-list nil)
  (map-all-generic-functions #'list-dfun)
  dfun-list)

(defun list-large-cache (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (cache (gf-dfun-cache gf)))
    (when cache
      (let ((size (cache-size cache)))
	(when (>= size *minimum-cache-size-to-list*)
	  (let ((a (assoc size dfun-list)))
	    (unless a
	      (push (setq a (list size)) dfun-list))
	    (push (let ((name (generic-function-name gf)))
		    (if (eq sym 'caching) name (list name sym)))
		  (cdr a))))))))

(defun list-large-caches (&optional (*minimum-cache-size-to-list* 130))
  (setq dfun-list nil)
  (map-all-generic-functions #'list-large-cache)
  (setq dfun-list (sort dfun-list #'< :key #'car))
  (mapc #'print dfun-list)
  (values))


(defun count-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (cache (gf-dfun-cache gf))
	 (a (assq sym dfun-count)))
    (unless a
      (push (setq a (list sym 0 nil)) dfun-count))
    (incf (cadr a))
    (when cache
      (let* ((size (cache-size cache))
	     (b (assoc size (third a))))
	(unless b 
	  (push (setq b (cons size 0)) (third a)))
	(incf (cdr b))))))

(defun count-all-dfuns ()
  (setq dfun-count (mapcar #'(lambda (type) (list type 0 nil))
			   '(ONE-CLASS TWO-CLASS DEFAULT-METHOD-ONLY
			     ONE-INDEX N-N CHECKING CACHING 
			     DISPATCH)))
  (map-all-generic-functions #'count-dfun)
  (mapc #'(lambda (type+count+sizes)
	    (setf (third type+count+sizes)
		  (sort (third type+count+sizes) #'< :key #'car)))
	dfun-count)
  (mapc #'(lambda (type+count+sizes)
	    (format t "~&There are ~4d dfuns of type ~s"
		    (cadr type+count+sizes) (car type+count+sizes))
	    (format t "~%   ~S~%" (caddr type+count+sizes)))
	dfun-count)
  (values))

(defun gfs-of-type (type)
  (unless (consp type) (setq type (list type)))
  (let ((gf-list nil))
    (map-all-generic-functions #'(lambda (gf)
				   (when (memq (type-of (gf-dfun-info gf)) type)
				     (push gf gf-list))))
    gf-list))
