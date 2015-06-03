;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
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
;;; Bootstrapping the meta-braid.
;;;
;;; The code in this file takes the early definitions that have been saved
;;; up and actually builds those class objects.  This work is largely driven
;;; off of those class definitions, but the fact that STANDARD-CLASS is the
;;; class of all metaclasses in the braid is built into this code pretty
;;; deeply.
;;;
;;; 

(in-package :pcl)

(defun allocate-standard-instance (wrapper &optional (slots-init nil slots-init-p))
  #-new-kcl-wrapper (declare (special *slot-unbound*))
  #-new-kcl-wrapper
  (let ((instance (%%allocate-instance--class))
	(no-of-slots (wrapper-no-of-instance-slots wrapper)))
    (setf (%std-instance-wrapper instance) wrapper)
    (setf (%std-instance-slots instance) 
	  (if slots-init-p
	      (make-array no-of-slots :initial-contents slots-init)
	      (make-array no-of-slots :initial-element *slot-unbound*)))
    instance)
  #+new-kcl-wrapper
  (apply #'si:make-structure wrapper
	 (if slots-init-p
	     slots-init
	     (let ((no-of-slots (si::s-data-length wrapper)))
	       (if (< no-of-slots (fill-pointer *init-vector*))
		   (aref *init-vector* no-of-slots)
		   (get-init-list no-of-slots))))))

(defmacro allocate-funcallable-instance-slots (wrapper &optional 
						       slots-init-p slots-init)
  #-new-kcl-wrapper
  `(let ((no-of-slots (wrapper-no-of-instance-slots ,wrapper)))
     ,(if slots-init-p
	  `(if ,slots-init-p
	       (make-array no-of-slots :initial-contents ,slots-init)
	       (make-array no-of-slots :initial-element *slot-unbound*))
	  `(make-array no-of-slots :initial-element *slot-unbound*)))
  #+new-kcl-wrapper
  (if slots-init-p
      `(if ,slots-init-p
	   (allocate-standard-instance ,wrapper ,slots-init)
	   (allocate-standard-instance ,wrapper))
      `(allocate-standard-instance ,wrapper)))

(defun allocate-funcallable-instance (wrapper &optional (slots-init nil slots-init-p))
  (let ((fin (allocate-funcallable-instance-1)))
    (set-funcallable-instance-function
     fin #'(lambda (&rest args)
	     (declare (ignore args))
	     (error "The function of the funcallable-instance ~S has not been set"
		    fin)))
    (setf (fsc-instance-wrapper fin) wrapper
	  (fsc-instance-slots fin) (allocate-funcallable-instance-slots
				    wrapper slots-init-p slots-init))
    fin))

(defun allocate-structure-instance (wrapper &optional (slots-init nil slots-init-p))
  #-new-kcl-wrapper
  (let* ((class (wrapper-class wrapper))
	 (constructor (class-defstruct-constructor class)))
    (if constructor
	(let ((instance (funcall constructor))
	      (slots (class-slots class)))
	  (when slots-init-p
	    (dolist (slot slots)
	      (setf (slot-value-using-class class instance slot) (pop slots-init))))
	  instance)
	(error "Can't allocate an instance of class ~S" (class-name class))))
  #+new-kcl-wrapper
  (if slots-init-p
      (allocate-standard-instance wrapper slots-init)
      (allocate-standard-instance wrapper)))

;;;
;;; bootstrap-meta-braid
;;;
;;; This function builds the base metabraid from the early class definitions.
;;;   
(defmacro initial-classes-and-wrappers (&rest classes)
  `(progn
     ,@(mapcar #'(lambda (class)
		   (let ((wr (intern (format nil "~A-WRAPPER" class) 
				     *the-pcl-package*)))
		     `(setf ,wr ,(if (eq class 'standard-generic-function)
				     '*sgf-wrapper*
				     `(make-wrapper (early-class-size ',class)))
		            ,class (allocate-standard-instance
				    ,(if (eq class 'standard-generic-function)
					 'funcallable-standard-class-wrapper
					 'standard-class-wrapper))
		            (wrapper-class ,wr) ,class
		            #+new-kcl-wrapper (si::s-data-name ,wr)
		                   #+new-kcl-wrapper ',class
		            (find-class ',class) ,class)))
	      classes)))		        

(defun bootstrap-meta-braid ()
  (let* ((name 'class)
	 (predicate-name (make-type-predicate-name name)))
    (setf (gdefinition predicate-name)
	  #'(lambda (x) (declare (ignore x)) t))
    (do-satisfies-deftype name predicate-name))  
  (let* ((*create-classes-from-internal-structure-definitions-p* nil)
	 standard-class-wrapper standard-class
	 funcallable-standard-class-wrapper funcallable-standard-class
	 slot-class-wrapper slot-class
	 built-in-class-wrapper built-in-class
	 structure-class-wrapper structure-class
	 standard-direct-slot-definition-wrapper standard-direct-slot-definition
	 standard-effective-slot-definition-wrapper standard-effective-slot-definition
	 class-eq-specializer-wrapper class-eq-specializer
	 standard-generic-function-wrapper standard-generic-function)
    (initial-classes-and-wrappers 
     standard-class funcallable-standard-class
     slot-class built-in-class structure-class
     standard-direct-slot-definition standard-effective-slot-definition 
     class-eq-specializer standard-generic-function)
    ;;
    ;; First, make a class metaobject for each of the early classes.  For
    ;; each metaobject we also set its wrapper.  Except for the class T,
    ;; the wrapper is always that of STANDARD-CLASS.
    ;; 
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
	     (meta (ecd-metaclass definition))
	     (wrapper (ecase meta
			(slot-class slot-class-wrapper)
			(standard-class standard-class-wrapper)
			(funcallable-standard-class funcallable-standard-class-wrapper)
			(built-in-class built-in-class-wrapper)
			(structure-class structure-class-wrapper)))
             (class (or (find-class name nil)
			(allocate-standard-instance wrapper))))
	(when (or (eq meta 'standard-class) (eq meta 'funcallable-standard-class))
	  (inform-type-system-about-std-class name))
        (setf (find-class name) class)))
    ;;
    ;;
    ;;
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
	    (meta (ecd-metaclass definition))
	    (source (ecd-source definition))
	    (direct-supers (ecd-superclass-names definition))
	    (direct-slots  (ecd-canonical-slots definition))
	    (other-initargs (ecd-other-initargs definition)))
	(let ((direct-default-initargs
	       (getf other-initargs :direct-default-initargs)))
	  (multiple-value-bind (slots cpl default-initargs direct-subclasses)
	      (early-collect-inheritance name)
	    (let* ((class (find-class name))
		   (wrapper (cond ((eq class slot-class)
				   slot-class-wrapper)
				  ((eq class standard-class) 
				   standard-class-wrapper)
				  ((eq class funcallable-standard-class) 
				   funcallable-standard-class-wrapper)
				  ((eq class standard-direct-slot-definition) 
				   standard-direct-slot-definition-wrapper)
				  ((eq class standard-effective-slot-definition) 
				   standard-effective-slot-definition-wrapper)
				  ((eq class built-in-class)  
				   built-in-class-wrapper)
				  ((eq class structure-class)
				   structure-class-wrapper)
				  ((eq class class-eq-specializer)
				   class-eq-specializer-wrapper)
				  ((eq class standard-generic-function)
				   standard-generic-function-wrapper)
				  (t (make-wrapper (length slots) class))))
		   (proto nil))
	      (when (eq name 't) (setq *the-wrapper-of-t* wrapper))
	      (set (intern (format nil "*THE-CLASS-~A*" (symbol-name name))
			   *the-pcl-package*)
		   class)
	      (dolist (slot slots)
		(unless (eq (getf slot :allocation :instance) :instance)
		  (error "Slot allocation ~S not supported in bootstrap.")))
	      
	      (setf (wrapper-instance-slots-layout wrapper)
		    (mapcar #'canonical-slot-name slots))
	      (setf (wrapper-class-slots wrapper)
		    ())
	      
	      (setq proto (if (eq meta 'funcallable-standard-class)
			      (allocate-funcallable-instance wrapper)
			      (allocate-standard-instance wrapper)))
	    
	      (setq direct-slots
		    (bootstrap-make-slot-definitions 
		     name class direct-slots
		     standard-direct-slot-definition-wrapper nil))
	      (setq slots
		    (bootstrap-make-slot-definitions 
		     name class slots
		     standard-effective-slot-definition-wrapper t))
	      
	      (case meta
		((standard-class funcallable-standard-class)
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto
		  direct-slots slots direct-default-initargs default-initargs))
		(built-in-class		; *the-class-t*
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto))
		(slot-class		; *the-class-slot-object*
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto))
		(structure-class	; *the-class-structure-object*
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper))))))))

    (let* ((smc-class (find-class 'standard-method-combination))
	   (smc-wrapper (bootstrap-get-slot 'standard-class smc-class 'wrapper))
	   (smc (allocate-standard-instance smc-wrapper)))
      (flet ((set-slot (name value)
	       (bootstrap-set-slot 'standard-method-combination smc name value)))
	(set-slot 'source (load-truename))
	(set-slot 'type 'standard)
	(set-slot 'documentation "The standard method combination.")
	(set-slot 'options ()))
      (setq *standard-method-combination* smc))))

;;;
;;; Initialize a class metaobject.
;;;
(defun bootstrap-initialize-class
       (metaclass-name class name
        class-eq-wrapper source direct-supers direct-subclasses cpl wrapper
	&optional proto direct-slots slots direct-default-initargs default-initargs)
  (flet ((classes (names) (mapcar #'find-class names))
	 (set-slot (slot-name value)
	   (bootstrap-set-slot metaclass-name class slot-name value)))
    (set-slot 'name name)
    (set-slot 'source source)
    (set-slot 'type (if (eq class (find-class 't))
			t
			`(class ,class)))
    (set-slot 'class-eq-specializer 
	      (let ((spec (allocate-standard-instance class-eq-wrapper)))
		(bootstrap-set-slot 'class-eq-specializer spec 'type 
				    `(class-eq ,class))
		(bootstrap-set-slot 'class-eq-specializer spec 'object
				    class)
		spec))
    (set-slot 'class-precedence-list (classes cpl))
    (set-slot 'can-precede-list (classes (cdr cpl)))
    (set-slot 'incompatible-superclass-list nil)
    (set-slot 'direct-superclasses (classes direct-supers))
    (set-slot 'direct-subclasses (classes direct-subclasses))
    (set-slot 'direct-methods (cons nil nil))
    (set-slot 'wrapper wrapper)
    #+new-kcl-wrapper
    (setf (si::s-data-name wrapper) name)
    (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
				  (make-class-predicate-name name)))
    (set-slot 'plist
	      `(,@(and direct-default-initargs
		       `(direct-default-initargs ,direct-default-initargs))
		,@(and default-initargs
		       `(default-initargs ,default-initargs))))
    (when (memq metaclass-name '(standard-class funcallable-standard-class
				 structure-class slot-class))
      (set-slot 'direct-slots direct-slots)
      (set-slot 'slots slots)
      (set-slot 'initialize-info nil))
    (if (eq metaclass-name 'structure-class)
	(let ((constructor-sym '|STRUCTURE-OBJECT class constructor|))
	  (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
					(make-class-predicate-name name)))
	  (set-slot 'defstruct-form 
		    `(defstruct (structure-object (:constructor ,constructor-sym))))
	  (set-slot 'defstruct-constructor constructor-sym)
	  (set-slot 'from-defclass-p t)    
	  (set-slot 'plist nil)
	  (set-slot 'prototype (funcall constructor-sym)))
	(set-slot 'prototype (or proto (allocate-standard-instance wrapper))))
    class))

(defun bootstrap-make-slot-definitions (name class slots wrapper effective-p)
  (let ((index -1))
    (mapcar #'(lambda (slot)
		(incf index)
		(bootstrap-make-slot-definition
		  name class slot wrapper effective-p index))
	    slots)))

(defun bootstrap-make-slot-definition (name class slot wrapper effective-p index)  
  (let* ((slotd-class-name (if effective-p
			       'standard-effective-slot-definition
			       'standard-direct-slot-definition))
	 (slotd (allocate-standard-instance wrapper))
	 (slot-name (getf slot :name)))
    (flet ((get-val (name) (getf slot name))
	   (set-val (name val) (bootstrap-set-slot slotd-class-name slotd name val)))
      (set-val 'name         slot-name)
      (set-val 'initform     (get-val :initform))
      (set-val 'initfunction (get-val :initfunction))      
      (set-val 'initargs     (get-val :initargs))
      (set-val 'readers      (get-val :readers))
      (set-val 'writers      (get-val :writers))
      (set-val 'allocation   :instance)
      (set-val 'type         (or (get-val :type) t))
      (set-val 'documentation (or (get-val :documentation) ""))
      (set-val 'class        class)
      (when effective-p
	(set-val 'location index)
	(let ((fsc-p nil))
	  (set-val 'reader-function (make-optimized-std-reader-method-function 
				     fsc-p slot-name index))
	  (set-val 'writer-function (make-optimized-std-writer-method-function 
				     fsc-p slot-name index))
	  (set-val 'boundp-function (make-optimized-std-boundp-method-function 
				     fsc-p slot-name index)))
	(set-val 'accessor-flags 7)
	(let ((table (or (gethash slot-name *name->class->slotd-table*)
			 (setf (gethash slot-name *name->class->slotd-table*)
			       (make-hash-table :test 'eq :size 5)))))
	  (setf (gethash class table) slotd)))
      (when (and (eq name 'standard-class)
		 (eq slot-name 'slots) effective-p)
	(setq *the-eslotd-standard-class-slots* slotd))
      (when (and (eq name 'funcallable-standard-class)
		 (eq slot-name 'slots) effective-p)
	(setq *the-eslotd-funcallable-standard-class-slots* slotd))
      slotd)))

(defun bootstrap-accessor-definitions (early-p)
  (let ((*early-p* early-p))
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
	    (meta (ecd-metaclass definition)))
	(unless (eq meta 'built-in-class)
	  (let ((direct-slots  (ecd-canonical-slots definition)))
	    (dolist (slotd direct-slots)
	      (let ((slot-name (getf slotd :name))
		    (readers (getf slotd :readers))
		    (writers (getf slotd :writers)))
		(bootstrap-accessor-definitions1 name slot-name readers writers nil)
		(bootstrap-accessor-definitions1 
		 'slot-object
		 slot-name
		 (list (slot-reader-symbol slot-name))
		 (list (slot-writer-symbol slot-name))
		 (list (slot-boundp-symbol slot-name)))))))))))

(defun bootstrap-accessor-definition (class-name accessor-name slot-name type)
  (multiple-value-bind (accessor-class make-method-function arglist specls doc)
      (ecase type
	(reader (values 'standard-reader-method #'make-std-reader-method-function
			(list class-name) (list class-name)
			"automatically generated reader method"))
	(writer (values 'standard-writer-method #'make-std-writer-method-function
			(list 'new-value class-name) (list 't class-name)
			"automatically generated writer method"))
	(boundp (values 'standard-boundp-method #'make-std-boundp-method-function
			(list class-name) (list class-name)
			"automatically generated boundp method")))
    (let ((gf (ensure-generic-function accessor-name)))
      (if (find specls (early-gf-methods gf) 
		:key #'early-method-specializers
		:test #'equal)
	  (unless (assoc accessor-name *generic-function-fixups*
			 :test #'equal)
	    (update-dfun gf))
	  (add-method gf
		      (make-a-method accessor-class
				     ()
				     arglist specls
				     (funcall make-method-function
					      class-name slot-name)
				     doc
				     slot-name))))))

(defun bootstrap-accessor-definitions1 (class-name slot-name readers writers boundps)
  (flet ((do-reader-definition (reader)
	   (bootstrap-accessor-definition class-name reader slot-name 'reader))
	 (do-writer-definition (writer)
	   (bootstrap-accessor-definition class-name writer slot-name 'writer))
	 (do-boundp-definition (boundp)
	   (bootstrap-accessor-definition class-name boundp slot-name 'boundp)))
    (dolist (reader readers) (do-reader-definition reader))
    (dolist (writer writers) (do-writer-definition writer))
    (dolist (boundp boundps) (do-boundp-definition boundp))))

(defun bootstrap-class-predicates (early-p)
  (let ((*early-p* early-p))
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
	     (class (find-class name)))
	(setf (find-class-predicate name)
	      (make-class-predicate class (class-predicate-name class)))))))

(defun bootstrap-built-in-classes ()
  ;;
  ;; First make sure that all the supers listed in *built-in-class-lattice*
  ;; are themselves defined by *built-in-class-lattice*.  This is just to
  ;; check for typos and other sorts of brainos.
  ;; 
  (dolist (e *built-in-classes*)
    (dolist (super (cadr e))
      (unless (or (eq super 't)
		  (assq super *built-in-classes*))
	(error "In *built-in-classes*: ~S has ~S as a super,~%~
                but ~S is not itself a class in *built-in-classes*."
	       (car e) super super))))

  ;;
  ;; In the first pass, we create a skeletal object to be bound to the
  ;; class name.
  ;;
  (let* ((built-in-class (find-class 'built-in-class))
	 (built-in-class-wrapper (class-wrapper built-in-class)))
    (dolist (e *built-in-classes*)
      (let ((class (allocate-standard-instance built-in-class-wrapper)))
	(setf (find-class (car e)) class))))

  ;;
  ;; In the second pass, we initialize the class objects.
  ;;
  (let ((class-eq-wrapper (class-wrapper (find-class 'class-eq-specializer))))
    (dolist (e *built-in-classes*)
      (destructuring-bind (name supers subs cpl prototype) e
	(let* ((class (find-class name))
	       (wrapper (make-wrapper 0 class)))
	  (set (get-built-in-class-symbol name) class)
	  (set (get-built-in-wrapper-symbol name) wrapper)

	  (setf (wrapper-instance-slots-layout wrapper) ()
		(wrapper-class-slots wrapper) ())

	  (bootstrap-initialize-class 'built-in-class class
				      name class-eq-wrapper nil
				      supers subs
				      (cons name cpl)
				      wrapper prototype)))))
  
  (dolist (e *built-in-classes*)
    (let* ((name (car e))
	   (class (find-class name)))
      (setf (find-class-predicate name)
	    (make-class-predicate class (class-predicate-name class))))))


;;;
;;;
;;;
#-new-kcl-wrapper
(progn
(defvar *built-in-or-structure-wrapper-table*
  (make-hash-table :test 'eq))

(defvar wft-type1 nil)
(defvar wft-wrapper1 nil)
(defvar wft-type2 nil)
(defvar wft-wrapper2 nil)

(defun wrapper-for-structure (x)
  (let ((type (structure-type x)))
    (when (symbolp type)
      (cond ((eq type 'std-instance) 
	     (return-from wrapper-for-structure (std-instance-wrapper x)))
	    ((eq type wft-type1) (return-from wrapper-for-structure wft-wrapper1))
	    ((eq type wft-type2) (return-from wrapper-for-structure wft-wrapper2))
	    (t (setq wft-type2 wft-type1  wft-wrapper2 wft-wrapper1))))
    (let* ((cell (find-class-cell type))
	   (class (or (find-class-cell-class cell)
		      (let* (#+lucid 
			     (*structure-type* type)
			     #+lucid
			     (*structure-length* (structure-length x type)))
			(find-class-from-cell type cell))))
	   (wrapper (if class (class-wrapper class) *the-wrapper-of-t*)))
      (when (symbolp type)
	(setq wft-type1 type  wft-wrapper1 wrapper))
      wrapper)))

(defun built-in-or-structure-wrapper1 (x)
  (let ((biw (or (built-in-wrapper-of x) *the-wrapper-of-t*)))
    (or (and (eq biw *the-wrapper-of-t*)
	     (structurep x)
	     (let* ((type (type-of x))
		    #+lucid 
		    (*structure-type* type)
		    #+lucid
		    (*structure-length* (structure-length x type))
		    (class (find-class type nil)))
	       (and class (class-wrapper class))))
	biw)))
)

#|| ; moved to low.lisp
(defmacro built-in-or-structure-wrapper (x)
  (once-only (x)
    (if (structure-functions-exist-p) ; otherwise structurep is too slow for this
	`(if (structurep ,x)
	     (wrapper-for-structure ,x)
	     (if (symbolp ,x)
		 (if ,x *the-wrapper-of-symbol* *the-wrapper-of-null*)
		 (built-in-wrapper-of ,x)))
	`(or (and (symbolp ,x)
		  (if ,x *the-wrapper-of-symbol* *the-wrapper-of-null*))
	     (built-in-or-structure-wrapper1 ,x)))))
||#

(defmacro wrapper-of-macro (x)
  `(cond ((std-instance-p ,x)
	  (std-instance-wrapper ,x))
         ((fsc-instance-p ,x)
	  (fsc-instance-wrapper ,x))	      
         (t
	  (#+new-kcl-wrapper built-in-wrapper-of
	   #-new-kcl-wrapper built-in-or-structure-wrapper
	   ,x))))

(defun class-of (x)
  (wrapper-class* (wrapper-of-macro x)))

(defun wrapper-of (x)
  (wrapper-of-macro x))

(defun structure-wrapper (x)
  (class-wrapper (find-class (structure-type x))))

(defvar find-structure-class nil)

(defun eval-form (form)
  #'(lambda () (eval form)))

(defun slot-initargs-from-structure-slotd (slotd)
  `(:name ,(structure-slotd-name slotd)
    :defstruct-accessor-symbol ,(structure-slotd-accessor-symbol slotd)
    :internal-reader-function ,(structure-slotd-reader-function slotd)
    :internal-writer-function ,(structure-slotd-writer-function slotd)
    :type ,(or (structure-slotd-type slotd) t)
    :initform ,(structure-slotd-init-form slotd)
    :initfunction ,(eval-form (structure-slotd-init-form slotd))))

(defun find-structure-class (symbol)
  (if (structure-type-p symbol)
      (unless (eq find-structure-class symbol)
	(let ((find-structure-class symbol))
	  (ensure-class symbol
			:metaclass 'structure-class
			:name symbol
			:direct-superclasses
			(when (structure-type-included-type-name symbol)
			  (list (structure-type-included-type-name symbol)))
			:direct-slots
			(mapcar #'slot-initargs-from-structure-slotd
				(structure-type-slot-description-list symbol)))))
      (error "~S is not a legal structure class name." symbol)))

(eval-when (compile eval)

(defun make-built-in-class-subs ()
  (mapcar #'(lambda (e)
	      (let ((class (car e))
		    (class-subs ()))
		(dolist (s *built-in-classes*)
		  (when (memq class (cadr s)) (pushnew (car s) class-subs)))
		(cons class class-subs)))
	  (cons '(t) *built-in-classes*)))

(defun make-built-in-class-tree ()
  (let ((subs (make-built-in-class-subs)))
    (labels ((descend (class)
	       (cons class (mapcar #'descend (cdr (assq class subs))))))
      (descend 't))))

(defun make-built-in-wrapper-of-body ()
  (make-built-in-wrapper-of-body-1 (make-built-in-class-tree)
				   'x
				   #'get-built-in-wrapper-symbol))

(defun make-built-in-wrapper-of-body-1 (tree var get-symbol)
  (let ((*specials* ()))
    (declare (special *specials*))
    (let ((inner (make-built-in-wrapper-of-body-2 tree var get-symbol)))
      `(locally (declare (special .,*specials*)) ,inner))))

(defun make-built-in-wrapper-of-body-2 (tree var get-symbol)
  (declare (special *specials*))
  (let ((symbol (funcall get-symbol (car tree))))
    (push symbol *specials*)
    (let ((sub-tests
	    (mapcar #'(lambda (x)
			(make-built-in-wrapper-of-body-2 x var get-symbol))
		    (cdr tree))))
      `(and (typep ,var ',(car tree))
	    ,(if sub-tests
		 `(or ,.sub-tests ,symbol)
		 symbol)))))
)

(defun built-in-wrapper-of (x)
  #.(when (fboundp 'make-built-in-wrapper-of-body) ; so we can at least read this file
      (make-built-in-wrapper-of-body)))



(defun method-function-returning-nil (args next-methods)
  (declare (ignore args next-methods))
  nil)

(defun method-function-returning-t (args next-methods)
  (declare (ignore args next-methods))
  t)

(defun make-class-predicate (class name)
  (let* ((gf (ensure-generic-function name))
	 (mlist (if (eq *boot-state* 'complete)
		    (generic-function-methods gf)
		    (early-gf-methods gf))))
    (unless mlist
      (unless (eq class *the-class-t*)
	(let* ((default-method-function #'method-function-returning-nil)
	       (default-method-initargs (list :function
					      default-method-function))
	       (default-method (make-a-method 'standard-method
					      ()
					      (list 'object)
					      (list *the-class-t*)
					      default-method-initargs
					      "class predicate default method")))
	  (setf (method-function-get default-method-function :constant-value) nil)
	  (add-method gf default-method)))
      (let* ((class-method-function #'method-function-returning-t)
	     (class-method-initargs (list :function
					  class-method-function))
	     (class-method (make-a-method 'standard-method
					  ()
					  (list 'object)
					  (list class)
					  class-method-initargs
					  "class predicate class method")))
	(setf (method-function-get class-method-function :constant-value) t)
	(add-method gf class-method)))
    gf))

(eval-when (load eval)
  (clrhash *find-class*)
  (bootstrap-meta-braid)
  (bootstrap-accessor-definitions t)
  (bootstrap-class-predicates t)
  (bootstrap-accessor-definitions nil)
  (bootstrap-class-predicates nil)
  (bootstrap-built-in-classes)
  (setq *boot-state* 'braid)
  )

(deftype slot-object ()
  '(or standard-object structure-object))

(defmethod no-applicable-method (generic-function &rest args)
  (cerror "Retry call to ~S"
	  "No matching method for the generic-function ~S,~@
          when called with arguments ~S."
	  generic-function args)
  (apply generic-function args))
