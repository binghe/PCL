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

(in-package :pcl)

(eval-when (compile load eval)
  
(defvar *defclass-times*   '(load eval))	;Probably have to change this
						;if you use defconstructor.
(defvar *defmethod-times*  '(load eval))
(defvar *defgeneric-times* '(load eval))

(defvar *boot-state* ())			;NIL
						;EARLY
						;BRAID
						;COMPLETE
(defvar *fegf-started-p* nil)


)

(eval-when (load eval)
  (when (eq *boot-state* 'complete)
    (error "Trying to load (or compile) PCL in an environment in which it~%~
            has already been loaded.  This doesn't work, you will have to~%~
            get a fresh lisp (reboot) and then load PCL."))
  (when *boot-state*
    (cerror "Try loading (or compiling) PCL anyways."
	    "Trying to load (or compile) PCL in an environment in which it~%~
             has already been partially loaded.  This may not work, you may~%~
             need to get a fresh lisp (reboot) and then load PCL."))
  )



;;;
;;; This is like fdefinition on the Lispm.  If Common Lisp had something like
;;; function specs I wouldn't need this.  On the other hand, I don't like the
;;; way this really works so maybe function specs aren't really right either?
;;; 
;;; I also don't understand the real implications of a Lisp-1 on this sort of
;;; thing.  Certainly some of the lossage in all of this is because these
;;; SPECs name global definitions.
;;;
;;; Note that this implementation is set up so that an implementation which
;;; has a 'real' function spec mechanism can use that instead and in that way
;;; get rid of setf generic function names.
;;;
(defmacro parse-gspec (spec
		       (non-setf-var . non-setf-case)
		       (setf-var . setf-case))
  (declare (indentation 1 1))
  (once-only (spec)
    `(cond (#-setf (symbolp ,spec) #+setf t
	    (let ((,non-setf-var ,spec)) ,@non-setf-case))
	   #-setf
	   ((and (listp ,spec)
		 (eq (car ,spec) 'setf)
		 (symbolp (cadr ,spec)))
	    (let ((,setf-var (cadr ,spec))) ,@setf-case))
	   #-setf
	   (t
	    (error
	      "Can't understand ~S as a generic function specifier.~%~
               It must be either a symbol which can name a function or~%~
               a list like ~S, where the car is the symbol ~S and the cadr~%~
               is a symbol which can name a generic function."
	      ,spec '(setf <foo>) 'setf)))))

;;;
;;; If symbol names a function which is traced or advised, return the
;;; unadvised, traced etc. definition.  This lets me get at the generic
;;; function object even when it is traced.
;;;
(defun unencapsulated-fdefinition (symbol)
  #+Lispm (si:fdefinition (si:unencapsulate-function-spec symbol))
  #+Lucid (lucid::get-unadvised-procedure (symbol-function symbol))
  #+excl  (or (excl::encapsulated-basic-definition symbol)
	      (symbol-function symbol))
  #+xerox (il:virginfn symbol)
  #+setf (fdefinition symbol)
  #+kcl (symbol-function
	  (let ((sym (get symbol 'si::traced)) first-form)
	    (if (and sym
		     (consp (symbol-function symbol))
		     (consp (setq first-form (nth 3 (symbol-function symbol))))
		     (eq (car first-form) 'si::trace-call))
		sym
		symbol)))
  #-(or Lispm Lucid excl Xerox setf kcl) (symbol-function symbol))

;;;
;;; If symbol names a function which is traced or advised, redefine
;;; the `real' definition without affecting the advise.
;;;
(defun fdefine-carefully (name new-definition)
  #+Lispm (si:fdefine name new-definition t t)
  #+Lucid (let ((lucid::*redefinition-action* nil))
	    (setf (symbol-function name) new-definition))
  #+excl  (setf (symbol-function name) new-definition)
  #+xerox (let ((advisedp (member name il:advisedfns :test #'eq))
                (brokenp (member name il:brokenfns :test #'eq)))
	    ;; In XeroxLisp (late of envos) tracing is implemented
	    ;; as a special case of "breaking".  Advising, however,
	    ;; is treated specially.
            (xcl:unadvise-function name :no-error t)
            (xcl:unbreak-function name :no-error t)
            (setf (symbol-function name) new-definition)
            (when brokenp (xcl:rebreak-function name))
            (when advisedp (xcl:readvise-function name)))
  #+(and setf (not cmu)) (setf (fdefinition name) new-definition)
  #+kcl (setf (symbol-function 
	       (let ((sym (get name 'si::traced)) first-form)
		 (if (and sym
			  (consp (symbol-function name))
			  (consp (setq first-form
				       (nth 3 (symbol-function name))))
			  (eq (car first-form) 'si::trace-call))
		     sym
		     name)))
	      new-definition)
  #+cmu (progn
	  (c::%%defun name new-definition nil)
	  (c::note-name-defined name :function)
	  new-definition)
  #-(or Lispm Lucid excl Xerox setf kcl cmu)
  (setf (symbol-function name) new-definition))

(defun gboundp (spec)
  (parse-gspec spec
    (name (fboundp name))
    (name (fboundp (get-setf-function-name name)))))

(defun gmakunbound (spec)
  (parse-gspec spec
    (name (fmakunbound name))
    (name (fmakunbound (get-setf-function-name name)))))

(defun gdefinition (spec)
  (parse-gspec spec
    (name (or #-setf (macro-function name)		;??
	      (unencapsulated-fdefinition name)))
    (name (unencapsulated-fdefinition (get-setf-function-name name)))))

(defun #-setf SETF\ PCL\ GDEFINITION #+setf (setf gdefinition) (new-value spec)
  (parse-gspec spec
    (name (fdefine-carefully name new-value))
    (name (fdefine-carefully (get-setf-function-name name) new-value))))


(proclaim '(special *the-class-t* 
	            *the-class-vector* *the-class-symbol*
                    *the-class-string* *the-class-sequence*
                    *the-class-rational* *the-class-ratio*
                    *the-class-number* *the-class-null* *the-class-list*
                    *the-class-integer* *the-class-float* *the-class-cons*
                    *the-class-complex* *the-class-character*
                    *the-class-bit-vector* *the-class-array*

                    *the-class-slot-object*
                    *the-class-standard-object*
                    *the-class-structure-object*
                    *the-class-class*
                    *the-class-generic-function*
                    *the-class-built-in-class*
                    *the-class-slot-class*
                    *the-class-structure-class*
                    *the-class-standard-class*
                    *the-class-funcallable-standard-class*
                    *the-class-method*
                    *the-class-standard-method*
	            *the-class-standard-reader-method*
	            *the-class-standard-writer-method*
	            *the-class-standard-boundp-method*
                    *the-class-standard-generic-function*
                    *the-class-standard-effective-slot-definition*

                    *the-eslotd-standard-class-slots*
                    *the-eslotd-funcallable-standard-class-slots*))

(proclaim '(special *the-wrapper-of-t*
                    *the-wrapper-of-vector* *the-wrapper-of-symbol*
                    *the-wrapper-of-string* *the-wrapper-of-sequence*
                    *the-wrapper-of-rational* *the-wrapper-of-ratio*
                    *the-wrapper-of-number* *the-wrapper-of-null*
                    *the-wrapper-of-list* *the-wrapper-of-integer*
                    *the-wrapper-of-float* *the-wrapper-of-cons*
                    *the-wrapper-of-complex* *the-wrapper-of-character*
                    *the-wrapper-of-bit-vector* *the-wrapper-of-array*))

(defun coerce-to-class (class &optional make-forward-referenced-class-p)
  (if (symbolp class)
      (or (find-class class (not make-forward-referenced-class-p))
	  (ensure-class class))
      class))

(defun specializer-from-type (type &aux args)
  (when (consp type)
    (setq args (cdr type) type (car type)))
  (cond ((symbolp type)
	 (or (and (null args) (find-class type))
	     (ecase type
	       (class    (coerce-to-class (car args)))
	       (prototype (make-instance 'class-prototype-specializer
					 :object (coerce-to-class (car args))))
	       (class-eq (class-eq-specializer (coerce-to-class (car args))))
	       (eql      (intern-eql-specializer (car args))))))
	((specializerp type) type)))

(defun type-from-specializer (specl)
  (cond ((eq specl 't)
	 't)
	((consp specl)
         (unless (member (car specl) '(class prototype class-eq eql))
           (error "~S is not a legal specializer type" specl))
         specl)
        ((progn
	   (when (symbolp specl)
	     ;;maybe (or (find-class specl nil) (ensure-class specl)) instead?
	     (setq specl (find-class specl)))
	   (or (not (eq *boot-state* 'complete))
	       (specializerp specl)))
	 (specializer-type specl))
        (t
         (error "~s is neither a type nor a specializer" specl))))

(defun type-class (type)
  (declare (special *the-class-t*))
  (setq type (type-from-specializer type))
  (if (atom type)
      (if (eq type 't)
	  *the-class-t*
	  (error "bad argument to type-class"))
      (case (car type)
        (eql (class-of (cadr type)))
	(prototype (class-of (cadr type))) ;?
        (class-eq (cadr type))
        (class (cadr type)))))

(defun class-eq-type (class)
  (specializer-type (class-eq-specializer class)))

(defun inform-type-system-about-std-class (name)
  (let ((predicate-name (make-type-predicate-name name)))
    (setf (gdefinition predicate-name) (make-type-predicate name))
    (do-satisfies-deftype name predicate-name)))

(defun make-type-predicate (name)
  (let ((cell (find-class-cell name)))
    #'(lambda (x)
	(funcall (the function (find-class-cell-predicate cell)) x))))


;This stuff isn't right.  Good thing it isn't used.
;The satisfies predicate has to be a symbol.  There is no way to
;construct such a symbol from a class object if class names change.
(defun class-predicate (class)
  (when (symbolp class) (setq class (find-class class)))
  #'(lambda (object) (memq class (class-precedence-list (class-of object)))))

(defun make-class-eq-predicate (class)
  (when (symbolp class) (setq class (find-class class)))
  #'(lambda (object) (eq class (class-of object))))

(defun make-eql-predicate (eql-object)
  #'(lambda (object) (eql eql-object object)))

#|| ; The argument to satisfies must be a symbol.  
(deftype class (&optional class)
  (if class
      `(satisfies ,(class-predicate class))
      `(satisfies ,(class-predicate 'class))))

(deftype class-eq (class)
  `(satisfies ,(make-class-eq-predicate class)))
||#

#-excl
(deftype eql (type-object)
  `(member ,type-object))



;;;
;;; These functions are a pale imitiation of their namesake.  They accept
;;; class objects or types where they should.
;;; 
(defun *normalize-type (type)
  (cond ((consp type)
         (if (member (car type) '(not and or))
             `(,(car type) ,@(mapcar #'*normalize-type (cdr type)))
             (if (null (cdr type))
                 (*normalize-type (car type))
                 type)))
        ((symbolp type)
         (let ((class (find-class type nil)))
           (if class
               (let ((type (specializer-type class)))
		 (if (listp type) type `(,type)))
               `(,type))))
        ((or (not (eq *boot-state* 'complete))
	     (specializerp type))
	 (specializer-type type))
        (t
         (error "~s is not a type" type))))

(defun unparse-type-list (tlist)
  (mapcar #'unparse-type tlist))

(defun unparse-type (type)
  (if (atom type)
      (if (specializerp type)
          (unparse-type (specializer-type type))
          type)
      (case (car type)
        (eql type)
        (class-eq `(class-eq ,(class-name (cadr type))))
        (class (class-name (cadr type)))
        (t `(,(car type) ,@(unparse-type-list (cdr type)))))))

(defun convert-to-system-type (type)
  (case (car type)
    ((not and or) `(,(car type) ,@(mapcar #'convert-to-system-type (cdr type))))
    (class (class-name (cadr type))) ; it had better be a named class
    (class-eq (class-name (cadr type))) ; this one is impossible to do right
    (eql type)
    (t (if (null (cdr type))
	   (car type)
	   type))))

(defun *typep (object type)
  (setq type (*normalize-type type))
  (cond ((member (car type) '(eql wrapper-eq class-eq class))
         (specializer-applicable-using-type-p type `(eql ,object)))
        ((eq (car type) 'not)
         (not (*typep object (cadr type))))
        (t
         (typep object (convert-to-system-type type)))))

;Writing the missing NOT and AND clauses will improve
;the quality of code generated by generate-discrimination-net, but
;calling subtypep in place of just returning (values nil nil) can be
;very slow.  *subtypep is used by PCL itself, and must be fast.
(defun *subtypep (type1 type2)
  (if (equal type1 type2)
      (values t t)
      (if (eq *boot-state* 'early)
	  (values (eq type1 type2) t)
	  (let ((*in-precompute-effective-methods-p* t)) 
	    (declare (special *in-precompute-effective-methods-p*))
            ;; *in-precompute-effective-methods-p* is not a good name.
	    ;; It changes the way class-applicable-using-class-p works.
	    (setq type1 (*normalize-type type1))
	    (setq type2 (*normalize-type type2))
	    (case (car type2)
	      (not
	       (values nil (not (equal (cadr type2) type1))))
	      (and
	       (values nil nil)) ; Should improve this.
	      ((eql wrapper-eq class-eq class)
	       (multiple-value-bind (app-p maybe-app-p)
		   (specializer-applicable-using-type-p type2 type1)
		 (values app-p (or app-p (not maybe-app-p)))))
	      (t
	       (subtypep (convert-to-system-type type1)
			 (convert-to-system-type type2))))))))

(defun do-satisfies-deftype (name predicate)
  #+(or :Genera (and :Lucid (not :Prime)) ExCL :coral)
  (let* ((specifier `(satisfies ,predicate))
	 (expand-fn #'(lambda (&rest ignore)
			(declare (ignore ignore))
			specifier)))
    ;; Specific ports can insert their own way of doing this.  Many
    ;; ports may find the expand-fn defined above useful.
    ;;
    (or #+:Genera
	(setf (get name 'deftype) expand-fn)
	#+(and :Lucid (not :Prime))
	(system::define-macro `(deftype ,name) expand-fn nil)
	#+ExCL
	(setf (get name 'excl::deftype-expander) expand-fn)
	#+:coral
	(setf (get name 'ccl::deftype-expander) expand-fn)))
  #-(or :Genera (and :Lucid (not :Prime)) ExCL :coral)
  ;; This is the default for ports for which we don't know any
  ;; better.  Note that for most ports, providing this definition
  ;; should just speed up class definition.  It shouldn't have an
  ;; effect on performance of most user code.
  (eval `(deftype ,name () '(satisfies ,predicate))))

(defun make-type-predicate-name (name &optional kind)
  (if (symbol-package name)
      (intern (format nil
		      "~@[~A ~]TYPE-PREDICATE ~A ~A"
		      kind
		      (package-name (symbol-package name))
		      (symbol-name name))
	      *the-pcl-package*)
      (make-symbol (format nil
			   "~@[~A ~]TYPE-PREDICATE ~A"
			   kind
			   (symbol-name name)))))



(defvar *built-in-class-symbols* ())
(defvar *built-in-wrapper-symbols* ())

(defun get-built-in-class-symbol (class-name)
  (or (cadr (assq class-name *built-in-class-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-CLASS-~A*"
				    (symbol-name class-name))
			    *the-pcl-package*)))
	(push (list class-name symbol) *built-in-class-symbols*)
	symbol)))

(defun get-built-in-wrapper-symbol (class-name)
  (or (cadr (assq class-name *built-in-wrapper-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-WRAPPER-OF-~A*"
				    (symbol-name class-name))
			    *the-pcl-package*)))
	(push (list class-name symbol) *built-in-wrapper-symbols*)
	symbol)))




(pushnew 'class *variable-declarations*)
(pushnew 'variable-rebinding *variable-declarations*)

(defun variable-class (var env)
  (caddr (variable-declaration 'class var env)))

(defvar *name->class->slotd-table* (make-hash-table))


;;;
;;; This is used by combined methods to communicate the next methods to
;;; the methods they call.  This variable is captured by a lexical variable
;;; of the methods to give it the proper lexical scope.
;;; 
(defvar *next-methods* nil)

(defvar *not-an-eql-specializer* '(not-an-eql-specializer))

(defvar *umi-gfs*)
(defvar *umi-complete-classes*)
(defvar *umi-reorder*)

(defvar *invalidate-discriminating-function-force-p* ())
(defvar *invalid-dfuns-on-stack* ())


(defvar *standard-method-combination*)

(defvar *slotd-unsupplied* (list '*slotd-unsupplied*))	;***


(defmacro define-gf-predicate (predicate-name &rest classes)
  `(progn 
     (defmethod ,predicate-name ((x t)) nil)
     ,@(mapcar #'(lambda (c) `(defmethod ,predicate-name ((x ,c)) t))
	       classes)))

(defun make-class-predicate-name (name)
  (intern (format nil "~A::~A class predicate"
		  (package-name (symbol-package name))
		  name)
	  *the-pcl-package*))

(defun plist-value (object name)
  (getf (object-plist object) name))

(defun #-setf SETF\ PCL\ PLIST-VALUE #+setf (setf plist-value) (new-value object name)
  (if new-value
      (setf (getf (object-plist object) name) new-value)
      (progn
        (remf (object-plist object) name)
        nil)))



(defvar *built-in-classes*
  ;;
  ;; name       supers     subs                     cdr of cpl
  ;; prototype
  '(;(t         ()         (number sequence array character symbol) ())
    (number     (t)        (complex float rational) (t))
    (complex    (number)   ()                       (number t)
     #c(1 1))
    (float      (number)   ()                       (number t)
     1.0)
    (rational   (number)   (integer ratio)          (number t))
    (integer    (rational) ()                       (rational number t)
     1)
    (ratio      (rational) ()                       (rational number t)
     1/2)

    (sequence   (t)        (list vector)            (t))
    (list       (sequence) (cons null)              (sequence t))
    (cons       (list)     ()                       (list sequence t)
     (nil))
    

    (array      (t)        (vector)                 (t)
     #2A((NIL)))
    (vector     (array
		 sequence) (string bit-vector)      (array sequence t)
     #())
    (string     (vector)   ()                       (vector array sequence t)
     "")
    (bit-vector (vector)   ()                       (vector array sequence t)
     #*1)
    (character  (t)        ()                       (t)
     #\c)
   
    (symbol     (t)        (null)                   (t)
     symbol)
    (null       (symbol 
		 list)     ()                       (symbol list sequence t)
     nil)))


;;;
;;; The classes that define the kernel of the metabraid.
;;;
(defclass t () ()
  (:metaclass built-in-class))

(defclass slot-object (t) ()
  (:metaclass slot-class))

(defclass structure-object (slot-object) ()
  (:metaclass structure-class))

(defstruct (structure-object
	     (:constructor |STRUCTURE-OBJECT class constructor|)))

(defclass standard-object (slot-object) ())

(defclass metaobject (standard-object) ())

(defclass specializer (metaobject) 
     ((type
        :initform nil
        :reader specializer-type)))

(defclass definition-source-mixin (standard-object)
     ((source
	:initform (load-truename)
	:reader definition-source
	:initarg :definition-source)))

(defclass plist-mixin (standard-object)
     ((plist
	:initform ()
	:accessor object-plist)))

(defclass documentation-mixin (plist-mixin)
     ())

(defclass dependent-update-mixin (plist-mixin)
    ())

;;;
;;; The class CLASS is a specified basic class.  It is the common superclass
;;; of any kind of class.  That is any class that can be a metaclass must
;;; have the class CLASS in its class precedence list.
;;; 
(defclass class (documentation-mixin dependent-update-mixin definition-source-mixin
		 specializer)
     ((name
	:initform nil
	:initarg  :name
	:accessor class-name)
      (class-eq-specializer
        :initform nil
        :reader class-eq-specializer)
      (direct-superclasses
	:initform ()
	:reader class-direct-superclasses)
      (direct-subclasses
	:initform ()
	:reader class-direct-subclasses)
      (direct-methods
	:initform (cons nil nil))
      (predicate-name
        :initform nil
	:reader class-predicate-name)))

;;;
;;; The class PCL-CLASS is an implementation-specific common superclass of
;;; all specified subclasses of the class CLASS.
;;; 
(defclass pcl-class (class)
     ((class-precedence-list
	:reader class-precedence-list)
      (can-precede-list
        :initform ()
	:reader class-can-precede-list)
      (incompatible-superclass-list
        :initform ()
	:accessor class-incompatible-superclass-list)
      (wrapper
	:initform nil
	:reader class-wrapper)
      (prototype
	:initform nil
	:reader class-prototype)))

(defclass slot-class (pcl-class)
     ((direct-slots
	:initform ()
	:accessor class-direct-slots)
      (slots
        :initform ()
	:accessor class-slots)
      (initialize-info
        :initform nil
	:accessor class-initialize-info)))

;;;
;;; The class STD-CLASS is an implementation-specific common superclass of
;;; the classes STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.
;;; 
(defclass std-class (slot-class)
     ())

(defclass standard-class (std-class)
     ())

(defclass funcallable-standard-class (std-class)
     ())
    
(defclass forward-referenced-class (pcl-class) ())

(defclass built-in-class (pcl-class) ())

(defclass structure-class (slot-class)
     ((defstruct-form
        :initform ()
	:accessor class-defstruct-form)
      (defstruct-constructor
        :initform nil
	:accessor class-defstruct-constructor)
      (from-defclass-p
        :initform nil
	:initarg :from-defclass-p)))
     

(defclass specializer-with-object (specializer) ())

(defclass exact-class-specializer (specializer) ())

(defclass class-eq-specializer (exact-class-specializer specializer-with-object)
  ((object :initarg :class :reader specializer-class :reader specializer-object)))

(defclass class-prototype-specializer (specializer-with-object)
  ((object :initarg :class :reader specializer-class :reader specializer-object)))

(defclass eql-specializer (exact-class-specializer specializer-with-object)
  ((object :initarg :object :reader specializer-object 
	   :reader eql-specializer-object)))

(defvar *eql-specializer-table* (make-hash-table :test 'eql))

(defun intern-eql-specializer (object)
  (or (gethash object *eql-specializer-table*)
      (setf (gethash object *eql-specializer-table*)
	    (make-instance 'eql-specializer :object object))))


;;;
;;; Slot definitions.
;;;
(defclass slot-definition (metaobject) 
     ((name
	:initform nil
	:initarg :name
        :accessor slot-definition-name)
      (initform
	:initform nil
	:initarg :initform
	:accessor slot-definition-initform)
      (initfunction
	:initform nil
	:initarg :initfunction
	:accessor slot-definition-initfunction)
      (readers
	:initform nil
	:initarg :readers
	:accessor slot-definition-readers)
      (writers
	:initform nil
	:initarg :writers
	:accessor slot-definition-writers)
      (initargs
	:initform nil
	:initarg :initargs
	:accessor slot-definition-initargs)
      (type
	:initform t
	:initarg :type
	:accessor slot-definition-type)
      (documentation
	:initform ""
	:initarg :documentation)
      (class
        :initform nil
	:initarg :class
	:accessor slot-definition-class)))

(defclass standard-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)))

(defclass structure-slot-definition (slot-definition)
  ((defstruct-accessor-symbol 
     :initform nil
     :initarg :defstruct-accessor-symbol
     :accessor slot-definition-defstruct-accessor-symbol)
   (internal-reader-function 
     :initform nil
     :initarg :internal-reader-function
     :accessor slot-definition-internal-reader-function)
   (internal-writer-function 
     :initform nil
     :initarg :internal-writer-function
     :accessor slot-definition-internal-writer-function)))

(defclass direct-slot-definition (slot-definition)
  ())

(defclass effective-slot-definition (slot-definition)
  ((reader-function ; #'(lambda (object) ...)
    :accessor slot-definition-reader-function)
   (writer-function ; #'(lambda (new-value object) ...)
    :accessor slot-definition-writer-function)
   (boundp-function ; #'(lambda (object) ...)
    :accessor slot-definition-boundp-function)
   (accessor-flags
    :initform 0)))

(defclass standard-direct-slot-definition (standard-slot-definition
					   direct-slot-definition)
  ())

(defclass standard-effective-slot-definition (standard-slot-definition
					      effective-slot-definition)
  ((location ; nil, a fixnum, a cons: (slot-name . value)
    :initform nil
    :accessor slot-definition-location)))

(defclass structure-direct-slot-definition (structure-slot-definition
					    direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
					       effective-slot-definition)
  ())

(defclass method (metaobject) ())

(defclass standard-method (definition-source-mixin plist-mixin method)
     ((generic-function
	:initform nil	
	:accessor method-generic-function)
;     (qualifiers
;	:initform ()
;	:initarg  :qualifiers
;	:reader method-qualifiers)
      (specializers
	:initform ()
	:initarg  :specializers
	:reader method-specializers)
      (lambda-list
	:initform ()
	:initarg  :lambda-list
	:reader method-lambda-list)
      (function
	:initform nil
	:initarg :function)		;no writer
      (fast-function
	:initform nil
	:initarg :fast-function		;no writer
	:reader method-fast-function)
;     (documentation
;	:initform nil
;	:initarg  :documentation
;	:reader method-documentation)
      ))

(defclass standard-accessor-method (standard-method)
     ((slot-name :initform nil
		 :initarg :slot-name
		 :reader accessor-method-slot-name)
      (slot-definition :initform nil
		       :initarg :slot-definition
		       :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())

(defclass standard-writer-method (standard-accessor-method) ())

(defclass standard-boundp-method (standard-accessor-method) ())

(defclass generic-function (dependent-update-mixin
			    definition-source-mixin
			    documentation-mixin
			    metaobject)
     ()
  (:metaclass funcallable-standard-class))
    
(defclass standard-generic-function (generic-function)
     ((name
	:initform nil
	:initarg :name
	:accessor generic-function-name)
      (methods
	:initform ()
	:accessor generic-function-methods)
      (method-class
	:initarg :method-class
	:accessor generic-function-method-class)
      (method-combination
	:initarg :method-combination
	:accessor generic-function-method-combination)
      (arg-info
        :initform (make-arg-info)
	:reader gf-arg-info)
      (dfun-state
	:initform ()
	:accessor gf-dfun-state)
      (pretty-arglist
	:initform ()
	:accessor gf-pretty-arglist)
      )
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class *the-class-standard-method*
		     :method-combination *standard-method-combination*))

(defclass method-combination (metaobject) ())

(defclass standard-method-combination
	  (definition-source-mixin method-combination)
     ((type          :reader method-combination-type
	             :initarg :type)
      (documentation :reader method-combination-documentation
		     :initarg :documentation)
      (options       :reader method-combination-options
	             :initarg :options)))

(defparameter *early-class-predicates*
  '((specializer specializerp)
    (exact-class-specializer exact-class-specializer-p)
    (class-eq-specializer class-eq-specializer-p)
    (eql-specializer eql-specializer-p)
    (class classp)
    (slot-class slot-class-p)
    (standard-class standard-class-p)
    (funcallable-standard-class funcallable-standard-class-p)
    (structure-class structure-class-p)
    (forward-referenced-class forward-referenced-class-p)
    (method method-p)
    (standard-method standard-method-p)
    (standard-accessor-method standard-accessor-method-p)
    (standard-reader-method standard-reader-method-p)
    (standard-writer-method standard-writer-method-p)
    (standard-boundp-method standard-boundp-method-p)
    (generic-function generic-function-p)
    (standard-generic-function standard-generic-function-p)
    (method-combination method-combination-p)))

