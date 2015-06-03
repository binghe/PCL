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
;;; This is the CMU Lisp version of the file low.
;;; 

(in-package :pcl)

(defmacro dotimes ((var count &optional (result nil)) &body body)
  `(lisp:dotimes (,var (the fixnum ,count) ,result)
     (declare (fixnum ,var))
     ,@body))

;;; Just use our without-interrupts.  We don't have the INTERRUPTS-ON/OFF local
;;; macros spec'ed in low.lisp, but they aren't used.
;;;
(defmacro without-interrupts (&rest stuff)
  `(sys:without-interrupts ,@stuff))


;;; Print the object addr in default printers.
;;;
(defun printing-random-thing-internal (thing stream)
  (format stream "{~X}" (sys:%primitive c:make-fixnum thing)))


(eval-when (compile load eval)
  (c:def-source-transform std-instance-p (x)
    (ext:once-only ((n-x x))
      `(and (ext:structurep ,n-x)
            (eq (kernel:structure-ref ,n-x 0) 'std-instance)))))

(defun function-arglist (fcn)
  "Returns the argument list of a compiled function, if possible."
  (cond ((symbolp fcn)
         (when (fboundp fcn)
           (function-arglist (symbol-function fcn))))
        ((eval:interpreted-function-p fcn)
         (eval:interpreted-function-arglist fcn))
        ((functionp fcn)
         (let ((lambda-expr (function-lambda-expression fcn)))
           (if lambda-expr
               (cadr lambda-expr)
               (let ((function (kernel:%closure-function fcn)))
                 (values (read-from-string
                          (kernel:%function-header-arglist function)))))))))


;;; We have this here and in fin.lisp, 'cause PCL wants to compile this
;;; file first.
;;; 
(defsetf funcallable-instance-name set-funcallable-instance-name)

;;; And returns the function, not the *name*.
(defun set-function-name (fcn new-name)
  "Set the name of a compiled function object."
  (cond ((symbolp fcn)
         (set-function-name (symbol-function fcn) new-name))
        ((funcallable-instance-p fcn)
         (setf (funcallable-instance-name fcn) new-name)
         fcn)
        ((eval:interpreted-function-p fcn)
         (setf (eval:interpreted-function-name fcn) new-name)
         fcn)
        (t
         (let ((header (kernel:%closure-function fcn)))
	   ;;#+cmu17
	   ;;(setf (c::%function-name header) new-name)
	   ;;#-cmu17
           (system:%primitive c::set-function-name header new-name))
	 fcn)))

(in-package "C")

;;From compiler/ir1util
(def-source-context pcl:defmethod (name &rest stuff)
  (let ((arg-pos (position-if #'listp stuff)))
    (if arg-pos
	`(pcl:defmethod ,name ,@(subseq stuff 0 arg-pos)
	   ,(nth-value 2 (pcl::parse-specialized-lambda-list
			  (elt stuff arg-pos))))
	`(pcl:defmethod ,name "<illegal syntax>"))))

(define-info-type type kind (member :primitive :defined :structure :class nil)
  (if (or (info type builtin name)
	  (info type translator name))
      :primitive
      nil))
(define-info-type type class-info (or pcl::class symbol null) nil)

(in-package "KERNEL")

(export '(clos-type clos-type-class))

(defmacro cold-load-init (&rest forms)
  `(progn ,@forms))

(defstruct (clos-type (:include ctype
				 (:class-info (type-class-or-lose 'clos))
				 (:enumerable t))
		       (:print-function %print-type))
  ;;
  ;; The CLOS class.
  (class nil))

(define-type-class clos)

(define-type-method (clos :unparse) (x)
  (clos-type-class x))

(define-type-method (clos :complex-union) (type1 type2)
  (make-union-type (list type1 type2)))

(define-type-method (clos :simple-=) (type1 type2)
  (values (eq (clos-type-class type1)
	      (clos-type-class type2))
	  t))

(define-type-method (clos :simple-subtypep) (type1 type2)
  (let ((class1 (pcl:find-class (clos-type-class type1) nil))
	(class2 (pcl:find-class (clos-type-class type2) nil)))
    (if (and class1 class2)
	(values (member class2
			(pcl:class-precedence-list class2))  t)
      (values nil nil))))

;; Add the new ctype to its supertypes.
;;
(let ((the-type-* (values-specifier-type '*)))
  (pushnew 'clos (named-type-subclasses the-type-*)))
(let ((the-type-t (values-specifier-type 't)))
  (pushnew 'clos (named-type-subclasses the-type-t)))

(defun-cached (values-specifier-type
	       :hash-function (lambda (x)
				(the fixnum
				     (logand (the fixnum (cache-hash-eq x))
					     #x3FF)))
	       :hash-bits 10)
    ((spec eq))
  (or (info type builtin spec)
      (let ((expand (type-expand spec)))
	(if (eq expand spec)
	    (let* ((lspec (if (atom spec) (list spec) spec))
		   (fun (info type translator (car lspec))))
	      (cond
		(fun (funcall fun lspec))
		((and (symbolp spec)
		      (eq (info type kind spec) :structure))
		 (make-structure-type :name spec))
		((and (symbolp spec)
		      (eq (info type kind spec) :class))
		 (make-clos-type :class spec))
		((or (and (consp spec) (symbolp (car spec)))
		     (symbolp spec))
		 (signal 'parse-unknown-type :specifier spec)
		 ;;
		 ;; Inhibit caching...
		 (return-from values-specifier-type
		   (make-unknown-type :specifier spec)))
		(t
		 (error "Bad thing to be a type specifier: ~S."
			spec))))
	    (values-specifier-type expand)))))

(defun ctypep (obj type)
  (declare (type ctype type))
  (etypecase type
    ((or numeric-type named-type member-type array-type)
     (values (typep obj (type-specifier type)) t))
    (structure-type
     (if (structurep obj)
	 (let* ((name (structure-type-name type))
		(info (info type structure-info name))
		(defined-info (info type defined-structure-info name)))
	   (if (and info defined-info
		    (equal (c::dd-includes info)
			   (c::dd-includes defined-info)))
	       (values (typep obj name) t)
	       (values nil nil)))
	 (values nil t)))
    (clos-type
     (let ((class (clos-type-class type)))
       (if (pcl::std-instance-p obj)
	   (values (pcl::*typep obj class) t)
	   (values nil t))))
    (union-type
     (dolist (mem (union-type-types type) (values nil t))
       (multiple-value-bind (val win)
			    (ctypep obj mem)
	 (unless win (return (values nil nil)))
	 (when val (return (values t t))))))
    (function-type
     (values (functionp obj) t))
    (unknown-type
     (values nil nil))
    (alien-type-type
     (values (alien-typep obj (alien-type-type-alien-type type)) t))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and
	  (if (atom hairy-spec)
	      (values t t)
	      (dolist (spec (cdr hairy-spec) (values t t))
		(multiple-value-bind (res win)
				     (ctypep obj (specifier-type spec))
		  (unless win (return (values nil nil)))
		  (unless res (return (values nil t)))))))
	 (not
	  (multiple-value-bind
	      (res win)
	      (ctypep obj (specifier-type (cadr hairy-spec)))
	    (if win
		(values (not res) t)
		(values nil nil))))
	 (satisfies
	  (let ((fun (second hairy-spec)))
	    (cond ((and (consp fun) (eq (car fun) 'lambda))
		   (values (not (null (funcall (coerce fun 'function) obj)))
			   t))
		  ((and (symbolp fun) (fboundp fun))
		   (values (not (null (funcall fun obj))) t))
		  (t
		   (values nil nil))))))))))

(in-package "LISP")

(defun %%typep (object type)
  (declare (type ctype type))
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((* t)
	t)
       ((nil)
	nil)
       (character (characterp object))
       (base-char (base-char-p object))
       (standard-char (and (characterp object) (standard-char-p object)))
       (extended-char
	(and (characterp object) (not (base-char-p object))))
       (function (functionp object))
       (cons (consp object))
       (symbol (symbolp object))
       (keyword
	(and (symbolp object)
	     (eq (symbol-package object)
		 (symbol-package :foo))))
       (system-area-pointer (system-area-pointer-p object))
       (weak-pointer (weak-pointer-p object))
       (code-component (code-component-p object))
       (lra (lra-p object))
       (fdefn (fdefn-p object))
       (scavenger-hook (scavenger-hook-p object))
       (structure (structurep object))))
    (numeric-type
     (and (numberp object)
	  (let ((num (if (complexp object) (realpart object) object)))
	    (ecase (numeric-type-class type)
	      (integer (integerp num))
	      (rational (rationalp num))
	      (float
	       (ecase (numeric-type-format type)
		 (short-float (typep object 'short-float))
		 (single-float (typep object 'single-float))
		 (double-float (typep object 'double-float))
		 (long-float (typep object 'long-float))
		 ((nil) (floatp num))))
	      ((nil) t)))
	  (flet ((bound-test (val)
			     (let ((low (numeric-type-low type))
				   (high (numeric-type-high type)))
			       (and (cond ((null low) t)
					  ((listp low) (> val (car low)))
					  (t (>= val low)))
				    (cond ((null high) t)
					  ((listp high) (< val (car high)))
					  (t (<= val high)))))))
	    (ecase (numeric-type-complexp type)
	      ((nil) t)
	      (:complex
	       (and (complexp object)
		    (bound-test (realpart object))
		    (bound-test (imagpart object))))
	      (:real
	       (and (not (complexp object))
		    (bound-test object)))))))
    (array-type
     (and (arrayp object)
	  (ecase (array-type-complexp type)
	    ((t) (not (typep object 'simple-array)))
	    ((nil) (typep object 'simple-array))
	    (* t))
	  (or (eq (array-type-dimensions type) '*)
	      (do ((want (array-type-dimensions type) (cdr want))
		   (got (array-dimensions object) (cdr got)))
		  ((and (null want) (null got)) t)
		(unless (and want got
			     (or (eq (car want) '*)
				 (= (car want) (car got))))
		  (return nil))))
	  (or (eq (array-type-element-type type) *wild-type*)
	      (type= (array-type-specialized-element-type type)
		     (specifier-type (array-element-type object))))))
    (member-type
     (if (member object (member-type-members type)) t))
    (structure-type
     (structure-typep object (structure-type-name type)))
    (clos-type
     (pcl::*typep object (clos-type-class type)))
    (union-type
     (dolist (type (union-type-types type))
       (when (%%typep object type)
	 (return t))))
    (unknown-type
     ;; Type may be unknown to the compiler (and SPECIFIER-TYPE), yet be
     ;; a defined structure in the core.
     (let ((orig-spec (unknown-type-specifier type)))
       (if (and (symbolp orig-spec)
		(info type defined-structure-info orig-spec))
	   (structure-typep object orig-spec)
	   (error "Unknown type specifier: ~S" orig-spec))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and
	  (or (atom hairy-spec)
	      (dolist (spec (cdr hairy-spec) t)
		(unless (%%typep object (specifier-type spec))
		  (return nil)))))
	 (not
	  (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
	    (error "Invalid type specifier: ~S" hairy-spec))
	  (not (%%typep object (specifier-type (cadr hairy-spec)))))
	 (satisfies
	  (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
	    (error "Invalid type specifier: ~S" hairy-spec))
	  (let ((fn (cadr hairy-spec)))
	    (if (funcall (typecase fn
			   (function fn)
			   (symbol (symbol-function fn))
			   (t
			    (coerce fn 'function)))
			 object)
		t
		nil))))))
    (alien-type-type
     (alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (function-type
     (error "Function types are not a legal argument to TYPEP:~%  ~S"
	    (type-specifier type)))))

(defun %deftype (name expander &optional doc)
  (ecase (info type kind name)
    (:primitive
     (error "Illegal to redefine standard type: ~S." name))
    (:structure
     (warn "Redefining structure type ~S with DEFTYPE." name)
     (c::undefine-structure (info type structure-info name)))
    (:class
     (warn "Redefining class ~S with DEFTYPE." name))
    ((nil :defined)))
  (setf (info type kind name) :defined)
  (setf (info type expander name) expander)
  (when doc
    (setf (documentation name 'type) doc))
  ;; ### Bootstrap hack -- we need to define types before %note-type-defined
  ;; is defined.
  (when (fboundp 'c::%note-type-defined)
    (c::%note-type-defined name))
  name)

(defun type-of (object)
  "Return the type of OBJECT."
  (typecase object
    ;; First the ones that we can tell by testing the lowtag
    (fixnum 'fixnum)
    (function (type-specifier (ctype-of object)))
    (null 'null)
    (list 'cons)

    ;; Any other immediates.
    (character
     (typecase object
       (standard-char 'standard-char)
       (base-char 'base-char)
       (t 'character)))

    ;; And now for the complicated ones.
    (number
     (etypecase object
       (fixnum 'fixnum)
       (bignum 'bignum)
       (float
	(etypecase object
	  (double-float 'double-float)
	  (single-float 'single-float)
	  (short-float 'short-float)
	  (long-float 'long-float)))
       (ratio 'ratio)
       (complex 'complex)))
    (symbol
     (if (eq (symbol-package object)
	     (symbol-package :foo))
	 'keyword
	 'symbol))
    (structure
     (let ((name (structure-ref object 0)))
       (case name
	 (pcl::std-instance
	  (pcl:class-name (pcl:class-of object)))
	 (alien-internals:alien-value
	  `(alien:alien
	    ,(alien-internals:unparse-alien-type
	      (alien-internals:alien-value-type object))))
	 (t name))))
    (array (type-specifier (ctype-of object)))
    (system-area-pointer 'system-area-pointer)
    (weak-pointer 'weak-pointer)
    (code-component 'code-component)
    (lra 'lra)
    (fdefn 'fdefn)
    (scavenger-hook 'scavenger-hook)
    (t
     (warn "Can't figure out the type of ~S" object)
     t)))

(in-package "PCL")

(pushnew :structure-wrapper *features*)

(defun structure-functions-exist-p ()
  t)

(defun structure-instance-p (x)
  (and (structurep x)
       (not (eq (kernel:structure-ref x 0) 'std-instance))))

(defun structure-type (x)
  (kernel:structure-ref x 0))

(defun structure-type-p (type)
  (not (null (ext:info c::type c::defined-structure-info type))))

(defun structure-type-included-type-name (type)
  (let ((include (c::dd-include (ext:info c::type c::defined-structure-info type))))
    (if (consp include)
	(car include)
	include)))

(defun structure-type-slot-description-list (type)
  (nthcdr (length (let ((include (structure-type-included-type-name type)))
		    (and include (structure-type-slot-description-list include))))
	  (c::dd-slots (ext:info c::type c::defined-structure-info type))))

(defun structure-slotd-name (slotd)
  (intern (c::dsd-%name slotd) "USER"))

(defun structure-slotd-accessor-symbol (slotd)
  (c::dsd-accessor slotd))

(defun structure-slotd-reader-function (slotd)
  (fdefinition (c::dsd-accessor slotd)))

(defun structure-slotd-writer-function (slotd)
  (unless (c::dsd-read-only slotd)
    (fdefinition `(setf ,(c::dsd-accessor slotd)))))

(defun structure-slotd-type (slotd)
  (c::dsd-type slotd))

(defun structure-slotd-init-form (slotd)
  (c::dsd-default slotd))
