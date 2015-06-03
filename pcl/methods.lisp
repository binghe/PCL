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

(defmethod print-object (instance stream)
  (printing-random-thing (instance stream)
    (let ((name (class-name (class-of instance))))
      (if name
	  (format stream "~S" name)
	  (format stream "Instance")))))

(defmethod print-object ((class class) stream)
  (named-object-print-function class stream))

(defmethod print-object ((slotd slot-definition) stream)
  (named-object-print-function slotd stream))

(defun named-object-print-function (instance stream
				    &optional (extra nil extra-p))
  (printing-random-thing (instance stream)
    (if extra-p					
	(format stream "~A ~S ~:S"
		(capitalize-words (class-name (class-of instance)))
		(slot-value-or-default instance 'name)
		extra)
	(format stream "~A ~S"
		(capitalize-words (class-name (class-of instance)))
		(slot-value-or-default instance 'name)))))

(defmethod print-object ((mc standard-method-combination) stream)
  (printing-random-thing (mc stream)
    (format stream
	    "Method-Combination ~S ~S"
	    (slot-value-or-default mc 'type)
	    (slot-value-or-default mc 'options))))


;;;
;;;
;;;
(defmethod shared-initialize :after ((slotd standard-slot-definition) slot-names &key)
  (declare (ignore slot-names))
  (with-slots (allocation class)
    slotd
    (setq allocation (if (eq allocation :class) class allocation))))

(defmethod shared-initialize :after ((slotd structure-slot-definition) slot-names 
				     &key (allocation :instance))
  (declare (ignore slot-names))
  (unless (eq allocation :instance)
    (error "structure slots must have :instance allocation")))

(defmethod inform-type-system-about-class ((class structure-class) (name t))
  nil)

;;;
;;; METHODS
;;;
;;; Methods themselves are simple inanimate objects.  Most properties of
;;; methods are immutable, methods cannot be reinitialized.  The following
;;; properties of methods can be changed:
;;;   METHOD-GENERIC-FUNCTION
;;;   METHOD-FUNCTION            ??
;;;   
;;;

(defmethod method-function ((method standard-method))
  (or (slot-value method 'function)
      (let ((fmf (slot-value method 'fast-function)))
	(unless fmf ; the :before shared-initialize method prevents this
	  (error "~S doesn't seem to have a method-function" method))
	(setf (slot-value method 'function)
	      (method-function-from-fast-function fmf)))))

(defmethod accessor-method-class ((method standard-accessor-method))
  (car (slot-value method 'specializers)))

(defmethod accessor-method-class ((method standard-writer-method))
  (cadr (slot-value method 'specializers)))

(defmethod print-object ((method standard-method) stream)
  (printing-random-thing (method stream)
    (if (slot-boundp method 'generic-function)
	(let ((generic-function (method-generic-function method))
	      (class-name (capitalize-words (class-name (class-of method)))))
	  (format stream "~A ~S ~{~S ~}~:S"
		  class-name
		  (and generic-function (generic-function-name generic-function))
		  (method-qualifiers method)
		  (unparse-specializers method)))
	(call-next-method))))

(defmethod print-object ((method standard-accessor-method) stream)
  (printing-random-thing (method stream)
    (if (slot-boundp method 'generic-function)
	(let ((generic-function (method-generic-function method))
	      (class-name (capitalize-words (class-name (class-of method)))))
	  (format stream "~A ~S, slot:~S, ~:S"
		  class-name
		  (and generic-function (generic-function-name generic-function))
		  (accessor-method-slot-name method)
		  (unparse-specializers method)))
	(call-next-method))))

;;;
;;; INITIALIZATION
;;;
;;; Error checking is done in before methods.  Because of the simplicity of
;;; standard method objects the standard primary method can fill the slots.
;;;
;;; Methods are not reinitializable.
;;; 

(defmethod reinitialize-instance ((method standard-method) &rest initargs)
  (declare (ignore initargs))
  (error "Attempt to reinitialize the method ~S.~%~
          Method objects cannot be reinitialized."
	 method))

(defmethod legal-documentation-p ((object standard-method) x)
  (if (or (null x) (stringp x))
      t
      "a string or NULL"))

(defmethod legal-lambda-list-p ((object standard-method) x)
  (declare (ignore x))
  t)

(defmethod legal-method-function-p ((object standard-method) x)
  (if (functionp x)
      t
      "a function"))

(defmethod legal-qualifiers-p ((object standard-method) x)
  (flet ((improper-list ()
	   (return-from legal-qualifiers-p "Is not a proper list.")))
    (dolist-carefully (q x improper-list)
      (let ((ok (legal-qualifier-p object q)))
	(unless (eq ok t)
	  (return-from legal-qualifiers-p
	    (format nil "Contains ~S which ~A" q ok)))))
    t))

(defmethod legal-qualifier-p ((object standard-method) x)
  (if (and x (atom x))
      t
      "is not a non-null atom"))

(defmethod legal-slot-name-p ((object standard-method) x)
  (cond ((not (symbolp x)) "is not a symbol and so cannot be bound")
	((keywordp x)      "is a keyword and so cannot be bound")
	((memq x '(t nil)) "cannot be bound")
	((constantp x)     "is a constant and so cannot be bound")
	(t t)))

(defmethod legal-specializers-p ((object standard-method) x)
  (flet ((improper-list ()
	   (return-from legal-specializers-p "Is not a proper list.")))
    (dolist-carefully (s x improper-list)
      (let ((ok (legal-specializer-p object s)))
	(unless (eq ok t)
	  (return-from legal-specializers-p
	    (format nil "Contains ~S which ~A" s ok)))))
    t))

(defvar *allow-experimental-specializers-p* nil)

(defmethod legal-specializer-p ((object standard-method) x)
  (if (if *allow-experimental-specializers-p*
	  (specializerp x)
	  (or (classp x)
	      (eql-specializer-p x)))
      t
      "is neither a class object nor an eql specializer"))

(defmethod shared-initialize :before ((method standard-method)
				      slot-names
				      &key qualifiers
					   lambda-list
					   specializers
					   function
				           fast-function
					   documentation)
  (declare (ignore slot-names))
  (flet ((lose (initarg value string)
	   (error "When initializing the method ~S:~%~
                   The ~S initialization argument was: ~S.~%~
                   which ~A."
		  method initarg value string)))
    (let ((check-qualifiers    (legal-qualifiers-p method qualifiers))
	  (check-lambda-list   (legal-lambda-list-p method lambda-list))
	  (check-specializers  (legal-specializers-p method specializers))
	  (check-function      (legal-method-function-p method (or function
								   fast-function)))
	  (check-documentation (legal-documentation-p method documentation)))
      (unless (eq check-qualifiers t)
	(lose :qualifiers qualifiers check-qualifiers))
      (unless (eq check-lambda-list t)
	(lose :lambda-list lambda-list check-lambda-list))
      (unless (eq check-specializers t)
	(lose :specializers specializers check-specializers))
      (unless (eq check-function t)
	(lose :function function check-function))
      (unless (eq check-documentation t)
	(lose :documentation documentation check-documentation)))))

(defmethod shared-initialize :before ((method standard-accessor-method)
				      slot-names
				      &key slot-name slot-definition)
  (declare (ignore slot-names))
  (unless slot-definition
    (let ((legalp (legal-slot-name-p method slot-name)))
      (unless (eq legalp t)
	(error "The value of the :SLOT-NAME initarg ~A." legalp)))))

(defmethod shared-initialize :after ((method standard-method) slot-names
				     &rest initargs
				     &key qualifiers method-spec plist)
  (declare (ignore slot-names method-spec plist))
  (initialize-method-function initargs nil method)
  (setf (plist-value method 'qualifiers) qualifiers)
  #+ignore
  (setf (slot-value method 'closure-generator) 
	(method-function-closure-generator (slot-value method 'function))))

(defmethod shared-initialize :after ((method standard-accessor-method)
				     slot-names
				     &key)
  (declare (ignore slot-names))
  (with-slots (slot-name slot-definition)
    method
    (unless slot-definition
      (let ((class (accessor-method-class method)))
	(when (slot-class-p class)
	  (setq slot-definition (find slot-name (class-direct-slots class)
				      :key #'slot-definition-name)))))
    (when (and slot-definition (null slot-name))
      (setq slot-name (slot-definition-name slot-definition)))))

(defmethod method-qualifiers ((method standard-method))
  (plist-value method 'qualifiers))



(defvar *the-class-generic-function*          (find-class 'generic-function))
(defvar *the-class-standard-generic-function* (find-class 'standard-generic-function))



(defmethod print-object ((generic-function generic-function) stream)
  (named-object-print-function
    generic-function
    stream
    (if (slot-boundp generic-function 'methods)
	(list (length (generic-function-methods generic-function)))
	"?")))

(defmethod shared-initialize :before
	   ((generic-function standard-generic-function)
	    slot-names
	    &key (name nil namep)
		 (lambda-list () lambda-list-p)
		 argument-precedence-order
		 declarations
		 documentation
		 (method-class nil method-class-supplied-p)
		 (method-combination nil method-combination-supplied-p))
  (declare (ignore slot-names
		   declarations argument-precedence-order documentation
		   lambda-list lambda-list-p))

  (when namep
    (set-function-name generic-function name))
		   
  (flet ((initarg-error (initarg value string)
	   (error "When initializing the generic-function ~S:~%~
                   The ~S initialization argument was: ~A.~%~
                   It must be ~A."
		  generic-function initarg value string)))
    (cond (method-class-supplied-p
	   (when (symbolp method-class)
	     (setq method-class (find-class method-class)))
	   (unless (and (classp method-class)
			(*subtypep (class-eq-specializer method-class)
				   *the-class-method*))
	     (initarg-error :method-class
			    method-class
			    "a subclass of the class METHOD"))
	   (setf (slot-value generic-function 'method-class) method-class))
	  ((slot-boundp generic-function 'method-class))
	  (t
	   (initarg-error :method-class
			  "not supplied"
			  "a subclass of the class METHOD")))
    (cond (method-combination-supplied-p
	   (unless (method-combination-p method-combination)
	     (initarg-error :method-combination
			    method-combination
			    "a method combination object")))
	  ((slot-boundp generic-function 'method-combination))
	  (t
	   (initarg-error :method-combination
			  "not supplied"
			  "a method combination object")))))


#||
(defmethod reinitialize-instance ((generic-function standard-generic-function)
				  &rest initargs
				  &key name
				       lambda-list
				       argument-precedence-order
				       declarations
				       documentation
				       method-class
				       method-combination)
  (declare (ignore documentation declarations argument-precedence-order
		   lambda-list name method-class method-combination))
  (macrolet ((add-initarg (check name slot-name)
	       `(unless ,check
		  (push (slot-value generic-function ,slot-name) initargs)
		  (push ,name initargs))))
;   (add-initarg name :name 'name)
;   (add-initarg lambda-list :lambda-list 'lambda-list)
;   (add-initarg argument-precedence-order
;		 :argument-precedence-order
;		 'argument-precedence-order)
;   (add-initarg declarations :declarations 'declarations)
;   (add-initarg documentation :documentation 'documentation)
;   (add-initarg method-class :method-class 'method-class)
;   (add-initarg method-combination :method-combination 'method-combination)
    (apply #'call-next-method generic-function initargs)))
||#


;;;
;;; These three are scheduled for demolition.
;;; 
(defmethod remove-named-method (generic-function-name argument-specifiers
						      &optional extra)
  (let ((generic-function ())
	(method ()))
    (cond ((or (null (fboundp generic-function-name))
	       (not (generic-function-p
		      (setq generic-function
			    (symbol-function generic-function-name)))))
	   (error "~S does not name a generic-function."
		  generic-function-name))
	  ((null (setq method (get-method generic-function
					  extra
					  (parse-specializers
					    argument-specifiers)
					  nil)))
	   (error "There is no method for the generic-function ~S~%~
                   which matches the argument-specifiers ~S."
		  generic-function
		  argument-specifiers))
	  (t
	   (remove-method generic-function method)))))

(defun real-add-named-method (generic-function-name
			      qualifiers
			      specializers
			      lambda-list
			      &rest other-initargs)
  #+copy-&rest-arg (setq other-initargs (copy-list other-initargs))
  ;; What about changing the class of the generic-function if there is
  ;; one.  Whose job is that anyways.  Do we need something kind of
  ;; like class-for-redefinition?
  (let* ((generic-function
	   (ensure-generic-function generic-function-name))
	 (specs (parse-specializers specializers))
;	 (existing (get-method generic-function qualifiers specs nil))
	 (proto (method-prototype-for-gf generic-function-name))
	 (new (apply #'make-instance (class-of proto)
				     :qualifiers qualifiers
				     :specializers specs
				     :lambda-list lambda-list
				     other-initargs)))
;   (when existing (remove-method generic-function existing))
    (add-method generic-function new)))

	
(defun make-specializable (function-name &key (arglist nil arglistp))
  (cond ((not (null arglistp)))
	((not (fboundp function-name)))
	((fboundp 'function-arglist)
	 ;; function-arglist exists, get the arglist from it.
	 (setq arglist (function-arglist function-name)))
	(t
	 (error
	   "The :arglist argument to make-specializable was not supplied~%~
            and there is no version of FUNCTION-ARGLIST defined for this~%~
            port of Portable CommonLoops.~%~
            You must either define a version of FUNCTION-ARGLIST (which~%~
            should be easy), and send it off to the Portable CommonLoops~%~
            people or you should call make-specializable again with the~%~
            :arglist keyword to specify the arglist.")))
  (let ((original (and (fboundp function-name)
		       (symbol-function function-name)))
	(generic-function (make-instance 'standard-generic-function
					 :name function-name))
	(nrequireds 0))
    (if (generic-function-p original)
	original
	(progn
	  (dolist (arg arglist)
	    (if (memq arg lambda-list-keywords)
		(return)
		(incf nrequireds)))
	  (setf (gdefinition function-name) generic-function)
	  (set-function-name generic-function function-name)
	  (when arglistp
	    (setf (gf-pretty-arglist generic-function) arglist))
	  (when original
	    (add-named-method function-name
			      ()
			      (make-list nrequireds :initial-element 't)
			      arglist
			      (list :function
				    #'(lambda (args next-methods)
					(declare (ignore next-methods))
					(apply original args)))))
	  generic-function))))



(defun real-get-method (generic-function qualifiers specializers
					 &optional (errorp t))
  (let ((hit
	  (dolist (method (generic-function-methods generic-function))
	    (when (and (equal qualifiers (method-qualifiers method))
		       (every #'same-specializer-p specializers
			      (method-specializers method)))
	      (return method)))))
    (cond (hit hit)
	  ((null errorp) nil)
	  (t
	   (error "No method on ~S with qualifiers ~:S and specializers ~:S."
		  generic-function qualifiers specializers)))))


;;;
;;; Compute various information about a generic-function's arglist by looking
;;; at the argument lists of the methods.  The hair for trying not to use
;;; &rest arguments lives here.
;;;  The values returned are:
;;;    number-of-required-arguments
;;;       the number of required arguments to this generic-function's
;;;       discriminating function
;;;    &rest-argument-p
;;;       whether or not this generic-function's discriminating
;;;       function takes an &rest argument.
;;;    specialized-argument-positions
;;;       a list of the positions of the arguments this generic-function
;;;       specializes (e.g. for a classical generic-function this is the
;;;       list: (1)).
;;;
(defmethod compute-discriminating-function-arglist-info
	   ((generic-function standard-generic-function))
  ;;(declare (values number-of-required-arguments &rest-argument-p
  ;;                 specialized-argument-postions))
  (let ((number-required nil)
        (restp nil)
        (specialized-positions ())
	(methods (generic-function-methods generic-function)))
    (dolist (method methods)
      (multiple-value-setq (number-required restp specialized-positions)
        (compute-discriminating-function-arglist-info-internal
	  generic-function method number-required restp specialized-positions)))
    (values number-required restp (sort specialized-positions #'<))))

(defun compute-discriminating-function-arglist-info-internal
       (generic-function method number-of-requireds restp
	specialized-argument-positions)
  (declare (ignore generic-function) (type (or null fixnum) number-of-requireds))
  (let ((requireds 0))
    (declare (fixnum requireds))
    ;; Go through this methods arguments seeing how many are required,
    ;; and whether there is an &rest argument.
    (dolist (arg (method-lambda-list method))
      (cond ((eq arg '&aux) (return))
            ((memq arg '(&optional &rest &key))
             (return (setq restp t)))
	    ((memq arg lambda-list-keywords))
            (t (incf requireds))))
    ;; Now go through this method's type specifiers to see which
    ;; argument positions are type specified.  Treat T specially
    ;; in the usual sort of way.  For efficiency don't bother to
    ;; keep specialized-argument-positions sorted, rather depend
    ;; on our caller to do that.
    (iterate ((type-spec (list-elements (method-specializers method)))
              (pos (interval :from 0)))
      (unless (eq type-spec *the-class-t*)
	(pushnew pos specialized-argument-positions)))
    ;; Finally merge the values for this method into the values
    ;; for the exisiting methods and return them.  Note that if
    ;; num-of-requireds is NIL it means this is the first method
    ;; and we depend on that.
    (values (min (or number-of-requireds requireds) requireds)
            (or restp
		(and number-of-requireds (/= number-of-requireds requireds)))
            specialized-argument-positions)))

(defun make-discriminating-function-arglist (number-required-arguments restp)
  (nconc (gathering ((args (collecting)))
           (iterate ((i (interval :from 0 :below number-required-arguments)))
             (gather (intern (format nil "Discriminating Function Arg ~D" i))
		     args)))
         (when restp
               `(&rest ,(intern "Discriminating Function &rest Arg")))))


;;;
;;;
;;;

(defmethod generic-function-lambda-list ((gf generic-function))
  (gf-lambda-list gf))

(defmethod gf-fast-method-function-p ((gf standard-generic-function))
  (gf-info-fast-mf-p (slot-value gf 'arg-info)))

(defmethod initialize-instance :after ((gf standard-generic-function)
				       &key (lambda-list nil lambda-list-p)
				       argument-precedence-order)
  (with-slots (arg-info)
    gf
    (if lambda-list-p
	(set-arg-info gf 
		      :lambda-list lambda-list
		      :argument-precedence-order argument-precedence-order)
	(set-arg-info gf))
    (when (arg-info-valid-p arg-info)
      (update-dfun gf))))

(defmethod reinitialize-instance :after ((gf standard-generic-function)
					 &rest args
					 &key (lambda-list nil lambda-list-p)
					 (argument-precedence-order 
					  nil argument-precedence-order-p))
  (with-slots (arg-info)
    gf
    (if lambda-list-p
	(if argument-precedence-order-p
	    (set-arg-info gf 
			  :lambda-list lambda-list
			  :argument-precedence-order argument-precedence-order)
	    (set-arg-info gf 
			  :lambda-list lambda-list))
	(set-arg-info gf))
    (when (and (arg-info-valid-p arg-info)
	       args
	       (or lambda-list-p (cddr args)))
      (update-dfun gf))))

;;;
;;;
;;;

(proclaim '(special *lazy-dfun-compute-p*))

(defun set-methods (gf methods)
  (setf (generic-function-methods gf) nil)
  (loop (when (null methods) (return gf))
	(real-add-method gf (pop methods) methods)))

(defun real-add-method (generic-function method &optional skip-dfun-update-p)
  (if (method-generic-function method)
      (error "The method ~S is already part of the generic~@
              function ~S.  It can't be added to another generic~@
              function until it is removed from the first one."
	     method (method-generic-function method))

      (let* ((name (generic-function-name generic-function))
	     (qualifiers   (method-qualifiers method))
	     (specializers (method-specializers method))
	     (existing (get-method generic-function qualifiers specializers nil)))
	;;
	;; If there is already a method like this one then we must
	;; get rid of it before proceeding.  Note that we call the
	;; generic function remove-method to remove it rather than
	;; doing it in some internal way.
	;; 
	(when existing (remove-method generic-function existing))
	;;
	(setf (method-generic-function method) generic-function)
	(pushnew method (generic-function-methods generic-function))
	(dolist (specializer specializers)
	  (add-direct-method specializer method))
	(set-arg-info generic-function :new-method method)
	(unless skip-dfun-update-p
	  (when (member name 
			'(make-instance default-initargs
			  allocate-instance shared-initialize initialize-instance))
	    (update-make-instance-function-table (type-class (car specializers))))
	  (update-dfun generic-function))
	method)))
  
(defun real-remove-method (generic-function method)
  (if  (neq generic-function (method-generic-function method))
       (error "The method ~S is attached to the generic function~@
               ~S.  It can't be removed from the generic function~@
               to which it is not attached."
	      method (method-generic-function method))
       (let* ((name         (generic-function-name generic-function))
	      (specializers (method-specializers method))
	      (methods      (generic-function-methods generic-function))
	      (new-methods  (remove method methods)))	      
	 (setf (method-generic-function method) nil)
	 (setf (generic-function-methods generic-function) new-methods)
	 (dolist (specializer (method-specializers method))
	   (remove-direct-method specializer method))
	 (set-arg-info generic-function)
	 (when (member name '(make-instance default-initargs
			      allocate-instance shared-initialize initialize-instance))
	   (update-make-instance-function-table (type-class (car specializers))))
	 (update-dfun generic-function)
	 generic-function)))


(defun compute-applicable-methods-function (generic-function arguments)
  (values (compute-applicable-methods-using-types 
	   generic-function
	   (types-from-arguments generic-function arguments 'eql))))
  
(defmethod compute-applicable-methods 
    ((generic-function generic-function) arguments)
  (values (compute-applicable-methods-using-types 
	   generic-function
	   (types-from-arguments generic-function arguments 'eql))))

(defmethod compute-applicable-methods-using-classes 
    ((generic-function generic-function) classes)
  (compute-applicable-methods-using-types 
   generic-function
   (types-from-arguments generic-function classes 'class-eq)))

(defun proclaim-incompatible-superclasses (classes)
  (setq classes (mapcar #'(lambda (class)
			    (if (symbolp class)
				(find-class class)
				class))
			classes))
  (dolist (class classes)
    (dolist (other-class classes)
      (unless (eq class other-class)
	(pushnew other-class (class-incompatible-superclass-list class))))))

(defun superclasses-compatible-p (class1 class2)
  (let ((cpl1 (class-precedence-list class1))
	(cpl2 (class-precedence-list class2)))
    (dolist (sc1 cpl1 t)
      (dolist (ic (class-incompatible-superclass-list sc1))
	(when (memq ic cpl2)
	  (return-from superclasses-compatible-p nil))))))

(mapc
 #'proclaim-incompatible-superclasses
 '(;; superclass class
   (built-in-class std-class structure-class) ; direct subclasses of pcl-class
   (standard-class funcallable-standard-class)
   ;; superclass metaobject
   (class eql-specializer class-eq-specializer method method-combination
    generic-function slot-definition)
   ;; metaclass built-in-class
   (number sequence character   	; direct subclasses of t, but not array
    standard-object structure-object)   ;                         or symbol
   (number array character symbol	; direct subclasses of t, but not sequence
    standard-object structure-object)
   (complex float rational)		; direct subclasses of number
   (integer ratio)			; direct subclasses of rational
   (list vector)			; direct subclasses of sequence
   (cons null)				; direct subclasses of list
   (string bit-vector)			; direct subclasses of vector
   ))




(defmethod same-specializer-p ((specl1 specializer) (specl2 specializer))
  nil)

(defmethod same-specializer-p ((specl1 class) (specl2 class))
  (eq specl1 specl2))

(defmethod specializer-class ((specializer class))
  specializer)

(defmethod same-specializer-p ((specl1 class-eq-specializer)
			       (specl2 class-eq-specializer))
  (eq (specializer-class specl1) (specializer-class specl2)))

(defmethod same-specializer-p ((specl1 eql-specializer)
			       (specl2 eql-specializer))
  (eq (specializer-object specl1) (specializer-object specl2)))

(defmethod specializer-class ((specializer eql-specializer))
  (class-of (slot-value specializer 'object)))

(defvar *in-gf-arg-info-p* nil)
(setf (gdefinition 'arg-info-reader)
      (let ((mf (initialize-method-function
		 (make-internal-reader-method-function
		  'standard-generic-function 'arg-info)
		 t)))
	#'(lambda (&rest args) (funcall mf args nil))))

(defun types-from-arguments (generic-function arguments &optional type-modifier)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-function-info generic-function)
    (declare (ignore applyp metatypes nkeys))
    (let ((types-rev nil))
      (dotimes (i nreq)
	i
	(unless arguments
	  (error "The function ~S requires at least ~D arguments"
		 (generic-function-name generic-function)
		 nreq))
	(let ((arg (pop arguments)))
	  (push (if type-modifier `(,type-modifier ,arg) arg) types-rev)))
      (values (nreverse types-rev) arg-info))))

(defun get-wrappers-from-classes (nkeys wrappers classes metatypes)
  (let* ((w wrappers) (w-tail w) (mt-tail metatypes))
    (dolist (class (if (listp classes) classes (list classes)))
      (unless (eq 't (car mt-tail))
	(let ((c-w (class-wrapper class)))
	  (unless c-w (return-from get-wrappers-from-classes nil))
	  (if (eql nkeys 1)
	      (setq w c-w)
	      (setf (car w-tail) c-w
		    w-tail (cdr w-tail)))))
      (setq mt-tail (cdr mt-tail)))
    w))

(defun sdfun-for-caching (gf classes)
  (let ((types (mapcar #'class-eq-type classes)))
    (multiple-value-bind (methods all-applicable-and-sorted-p)
	(compute-applicable-methods-using-types gf types)
      (function-funcall (get-secondary-dispatch-function1 
			 gf methods types nil t all-applicable-and-sorted-p)
			nil (mapcar #'class-wrapper classes)))))

(defun value-for-caching (gf classes)
  (let ((methods (compute-applicable-methods-using-types 
		   gf (mapcar #'class-eq-type classes))))
    (method-function-get (or (method-fast-function (car methods))
			     (method-function (car methods)))
			 :constant-value)))

(defun default-secondary-dispatch-function (generic-function)
  #'(lambda (&rest args)
      #+copy-&rest-arg (setq args (copy-list args))
      (let ((methods (compute-applicable-methods generic-function args)))
	(if methods
	    (let ((emf (get-effective-method-function generic-function methods)))
	      (invoke-emf emf args))
	    (apply #'no-applicable-method generic-function args)))))

(defun list-eq (x y)
  (loop (when (atom x) (return (eq x y)))
	(when (atom y) (return nil))
	(unless (eq (car x) (car y)) (return nil))
	(setq x (cdr x)  y (cdr y))))

(defvar *std-cam-methods* nil)

(defun compute-applicable-methods-emf (generic-function)  
  (if (eq *boot-state* 'complete)
      (let* ((cam (gdefinition 'compute-applicable-methods))
	     (cam-methods (compute-applicable-methods-using-types
			   cam (list `(eql ,generic-function) t))))
	(values (get-effective-method-function cam cam-methods)
		(list-eq cam-methods 
			 (or *std-cam-methods*
			     (setq *std-cam-methods*
				   (compute-applicable-methods-using-types
				    cam (list `(eql ,cam) t)))))))
      (values #'compute-applicable-methods-function t)))

(defun compute-applicable-methods-emf-std-p (gf)
  (gf-info-c-a-m-emf-std-p (gf-arg-info gf)))

(defvar *old-c-a-m-gf-methods* nil)

(defun update-all-c-a-m-gf-info (c-a-m-gf)
  (let ((methods (generic-function-methods c-a-m-gf)))
    (if (and *old-c-a-m-gf-methods*
	     (every #'(lambda (old-method)
			(member old-method methods))
		    *old-c-a-m-gf-methods*))
	(let ((gfs-to-do nil)
	      (gf-classes-to-do nil))
	  (dolist (method methods)
	    (unless (member method *old-c-a-m-gf-methods*)
	      (let ((specl (car (method-specializers method))))
		(if (eql-specializer-p specl)
		    (pushnew (specializer-object specl) gfs-to-do)
		    (pushnew (specializer-class specl) gf-classes-to-do)))))
	  (map-all-generic-functions 
	   #'(lambda (gf)
	       (when (or (member gf gfs-to-do)
			 (dolist (class gf-classes-to-do nil)
			   (member class (class-precedence-list (class-of gf)))))
		 (update-c-a-m-gf-info gf)))))
	(map-all-generic-functions #'update-c-a-m-gf-info))
    (setq *old-c-a-m-gf-methods* methods)))

(defun update-gf-info (gf)
  (update-c-a-m-gf-info gf)
  (update-gf-simple-accessor-type gf))

(defun update-c-a-m-gf-info (gf)
  (unless (early-gf-p gf)
    (multiple-value-bind (c-a-m-emf std-p)
	(compute-applicable-methods-emf gf)
      (let ((arg-info (gf-arg-info gf)))
	(setf (gf-info-static-c-a-m-emf arg-info) c-a-m-emf)
	(setf (gf-info-c-a-m-emf-std-p arg-info) std-p)))))

(defun update-gf-simple-accessor-type (gf)
  (let ((arg-info (gf-arg-info gf)))
    (setf (gf-info-simple-accessor-type arg-info)
	  (let* ((methods (generic-function-methods gf))
		 (class (and methods (class-of (car methods))))
		 (type (and class (cond ((eq class *the-class-standard-reader-method*)
					 'reader)
					((eq class *the-class-standard-writer-method*)
					 'writer)
					((eq class *the-class-standard-boundp-method*)
					 'boundp)))))
	    (when (and (gf-info-c-a-m-emf-std-p arg-info)
		       type
		       (dolist (method (cdr methods) t)
			 (unless (eq class (class-of method)) (return nil)))
		       (eq (generic-function-method-combination gf)
			   *standard-method-combination*))
	      type)))))

(defun get-accessor-method-function (gf type class slotd)  
  (let* ((std-method (standard-svuc-method type))
	 (str-method (structure-svuc-method type))
	 (types1 `((eql ,class) (class-eq ,class) (eql ,slotd)))
	 (types (if (eq type 'writer) `(t ,@types1) types1))
	 (methods (compute-applicable-methods-using-types gf types))
	 (std-p (null (cdr methods))))
    (values
     (if std-p
	 (get-optimized-std-accessor-method-function class slotd type)
	 (get-accessor-from-svuc-method-function
	  class slotd
	  (get-secondary-dispatch-function 
	   gf methods types
	   `((,(car (or (member std-method methods)
			(member str-method methods)
			(error "error in get-accessor-method-function")))
	      ,(get-optimized-std-slot-value-using-class-method-function
		class slotd type)))
	   (unless (and (eq type 'writer)
			(dolist (method methods t)
			  (unless (eq (car (method-specializers method))
				      *the-class-t*)
			    (return nil))))
	     (let ((wrappers (list (wrapper-of class)
				   (class-wrapper class)
				   (wrapper-of slotd))))
	       (if (eq type 'writer)
		   (cons (class-wrapper *the-class-t*) wrappers)
		   wrappers))))
	  type))
     std-p)))

;used by optimize-slot-value-by-class-p (vector.lisp)
(defun update-slot-value-gf-info (gf type)
  (unless *new-class*
    (update-std-or-str-methods gf type))
  (when (and (standard-svuc-method type) (structure-svuc-method type))
    (flet ((update-class (class)
	     (when (class-finalized-p class)
	       (dolist (slotd (class-slots class))
		 (compute-slot-accessor-info slotd type gf)))))
      (if *new-class*
	  (update-class *new-class*)
	  (map-all-classes #'update-class 'slot-object)))))

(defvar *standard-slot-value-using-class-method* nil)
(defvar *standard-setf-slot-value-using-class-method* nil)
(defvar *standard-slot-boundp-using-class-method* nil)
(defvar *structure-slot-value-using-class-method* nil)
(defvar *structure-setf-slot-value-using-class-method* nil)
(defvar *structure-slot-boundp-using-class-method* nil)

(defun standard-svuc-method (type)
  (case type
    (reader *standard-slot-value-using-class-method*)
    (writer *standard-setf-slot-value-using-class-method*)
    (boundp *standard-slot-boundp-using-class-method*)))

(defun set-standard-svuc-method (type method)
  (case type
    (reader (setq *standard-slot-value-using-class-method* method))
    (writer (setq *standard-setf-slot-value-using-class-method* method))
    (boundp (setq *standard-slot-boundp-using-class-method* method))))

(defun structure-svuc-method (type)
  (case type
    (reader *structure-slot-value-using-class-method*)
    (writer *structure-setf-slot-value-using-class-method*)
    (boundp *structure-slot-boundp-using-class-method*)))

(defun set-structure-svuc-method (type method)
  (case type
    (reader (setq *structure-slot-value-using-class-method* method))
    (writer (setq *structure-setf-slot-value-using-class-method* method))
    (boundp (setq *structure-slot-boundp-using-class-method* method))))

(defun update-std-or-str-methods (gf type)
  (dolist (method (generic-function-methods gf))
    (let ((specls (method-specializers method)))
      (when (and (or (not (eq type 'writer))
		     (eq (pop specls) *the-class-t*))
		 (every #'classp specls))
	(cond ((and (eq (class-name (car specls))
			'std-class)
		    (eq (class-name (cadr specls)) 
			'standard-object)
		    (eq (class-name (caddr specls)) 
			'standard-effective-slot-definition))
	       (set-standard-svuc-method type method))
	      ((and (eq (class-name (car specls))
			'structure-class)
		    (eq (class-name (cadr specls))
			'structure-object)
		    (eq (class-name (caddr specls)) 
			'structure-effective-slot-definition))
	       (set-structure-svuc-method type method)))))))

(defun mec-all-classes-internal (spec precompute-p)
  (cons (specializer-class spec)
	(and (classp spec)
	     precompute-p
	     (not (or (eq spec *the-class-t*)
		      (eq spec *the-class-slot-object*)
		      (eq spec *the-class-standard-object*)
		      (eq spec *the-class-structure-object*)))
	     (let ((sc (class-direct-subclasses spec)))
	       (when sc
		 (mapcan #'(lambda (class)
			     (mec-all-classes-internal class precompute-p))
			 sc))))))

(defun mec-all-classes (spec precompute-p)
  (let ((classes (mec-all-classes-internal spec precompute-p)))
    (if (null (cdr classes))
	classes
	(let* ((a-classes (cons nil classes))
	       (tail classes))
	  (loop (when (null (cdr tail))
		  (return (cdr a-classes)))
		(let ((class (cadr tail))
		      (ttail (cddr tail)))
		  (if (dolist (c ttail nil)
			(when (eq class c) (return t)))
		      (setf (cdr tail) (cddr tail))
		      (setf tail (cdr tail)))))))))

(defun mec-all-class-lists (spec-list precompute-p)
  (if (null spec-list)
      (list nil)
      (let* ((car-all-classes (mec-all-classes (car spec-list) precompute-p))
	     (all-class-lists (mec-all-class-lists (cdr spec-list) precompute-p)))
	(mapcan #'(lambda (list)
		    (mapcar #'(lambda (c) (cons c list)) car-all-classes))
		all-class-lists))))

(defun make-emf-cache (generic-function valuep cache classes-list new-class)
  (let* ((arg-info (gf-arg-info generic-function))
	 (nkeys (arg-info-nkeys arg-info))
	 (metatypes (arg-info-metatypes arg-info))
	 (wrappers (unless (eq nkeys 1) (make-list nkeys)))
	 (precompute-p (gf-precompute-dfun-and-emf-p arg-info))
	 (default '(default)))
    (flet ((add-class-list (classes)
	     (when (or (null new-class) (memq new-class classes))
	       (let ((wrappers (get-wrappers-from-classes 
				nkeys wrappers classes metatypes)))
		 (when (and wrappers
			    (eq default (probe-cache cache wrappers default)))
		   (let ((value (cond ((eq valuep t)
				       (sdfun-for-caching generic-function classes))
				      ((eq valuep :constant-value)
				       (value-for-caching generic-function classes)))))
		     (setq cache (fill-cache cache wrappers value t))))))))
      (if classes-list
	  (mapc #'add-class-list classes-list)
	  (dolist (method (generic-function-methods generic-function))
	    (mapc #'add-class-list
		  (mec-all-class-lists (method-specializers method) precompute-p))))
      cache)))

(defmacro class-test (arg class)
  (cond ((eq class *the-class-t*)
	 't)
	((eq class *the-class-slot-object*)
	 #+new-kcl-wrapper
	 `(or (std-instance-p ,arg)
	      (fsc-instance-p ,arg))
	 #-new-kcl-wrapper
	 `(not (eq *the-class-built-in-class* 
		   (wrapper-class (std-instance-wrapper (class-of ,arg))))))
	#-new-kcl-wrapper
	((eq class *the-class-standard-object*)
	 `(or (std-instance-p ,arg) (fsc-instance-p ,arg)))
	((eq class *the-class-structure-object*)
	 `(memq ',class (class-precedence-list (class-of ,arg))))
	;; TYPEP is now sometimes faster than doing memq of the cpl
	(t
	 `(typep ,arg ',(class-name class)))))

(defmacro class-eq-test (arg class)
  `(eq (class-of ,arg) ',class))

(defmacro eql-test (arg object)
  `(eql ,arg ',object))

(defun dnet-methods-p (form)
  (and (consp form)
       (or (eq (car form) 'methods)
	   (eq (car form) 'unordered-methods))))

(defmacro scase (arg &rest clauses) ; This is case, but without gensyms
  `(let ((.case-arg. ,arg))
     (cond ,@(mapcar #'(lambda (clause)
			 (list* (cond ((null (car clause))
				       nil)
				      ((consp (car clause))
				       (if (null (cdar clause))
					   `(eql .case-arg. ',(caar clause))
					   `(member .case-arg. ',(car clause))))
				      ((member (car clause) '(t otherwise))
				       `t)
				      (t
				       `(eql .case-arg. ',(car clause))))
				nil
				(cdr clause)))
		     clauses))))

(defmacro mcase (arg &rest clauses) `(scase ,arg ,@clauses))

(defun generate-discrimination-net (generic-function methods types sorted-p)
  (let* ((arg-info (gf-arg-info generic-function))
	 (precedence (arg-info-precedence arg-info)))
    (generate-discrimination-net-internal 
     generic-function methods types
     #'(lambda (methods known-types)
	 (if (or sorted-p
		 (block one-order-p
		   (let ((sorted-methods nil))
		     (map-all-orders 
		      (copy-list methods) precedence
		      #'(lambda (methods)
			  (when sorted-methods (return-from one-order-p nil))
			  (setq sorted-methods methods)))
		     (setq methods sorted-methods))
		   t))
	     `(methods ,methods ,known-types)
	     `(unordered-methods ,methods ,known-types)))
     #'(lambda (position type true-value false-value)
	 (let ((arg (dfun-arg-symbol position)))
	   (if (eq (car type) 'eql)
	       (let* ((false-case-p (and (consp false-value)
					 (or (eq (car false-value) 'scase)
					     (eq (car false-value) 'mcase))
					 (eq arg (cadr false-value))))
		      (false-clauses (if false-case-p
					 (cddr false-value)
					 `((t ,false-value))))
		      (case-sym (if (and (dnet-methods-p true-value)
					 (if false-case-p
					     (eq (car false-value) 'mcase)
					     (dnet-methods-p false-value)))
				    'mcase
				    'scase))
		      (type-sym `(,(cadr type))))
		 `(,case-sym ,arg
		    (,type-sym ,true-value)
		    ,@false-clauses))
	       `(if ,(let ((arg (dfun-arg-symbol position)))
		       (case (car type)
			 (class    `(class-test    ,arg ,(cadr type)))
			 (class-eq `(class-eq-test ,arg ,(cadr type)))))
		    ,true-value
		    ,false-value))))
     #'identity)))

(defun class-from-type (type)
  (if (or (atom type) (eq (car type) 't))
      *the-class-t*
      (case (car type)
	(and (dolist (type (cdr type) *the-class-t*)
	       (when (and (consp type) (not (eq (car type) 'not)))
		 (return (class-from-type type)))))
	(not *the-class-t*)
        (eql (class-of (cadr type)))
        (class-eq (cadr type))
        (class (cadr type)))))

(defun precompute-effective-methods (gf caching-p &optional classes-list-p)
  (let* ((arg-info (gf-arg-info gf))
	 (methods (generic-function-methods gf))
	 (precedence (arg-info-precedence arg-info))
	 (*in-precompute-effective-methods-p* t)
	 (classes-list nil))
    (generate-discrimination-net-internal 
     gf methods nil
     #'(lambda (methods known-types)
	 (when methods
	   (when classes-list-p
	     (push (mapcar #'class-from-type known-types) classes-list))
	   (let ((no-eql-specls-p (not (methods-contain-eql-specializer-p methods))))
	     (map-all-orders 
	      methods precedence
	      #'(lambda (methods)
		  (get-secondary-dispatch-function1 
		   gf methods known-types
		   nil caching-p no-eql-specls-p))))))
     #'(lambda (position type true-value false-value)
	 (declare (ignore position type true-value false-value))
	 nil)
     #'(lambda (type)
	 (if (and (consp type) (eq (car type) 'eql))
	     `(class-eq ,(class-of (cadr type)))
	     type)))
    classes-list))

; we know that known-type implies neither new-type nor `(not ,new-type) 
(defun augment-type (new-type known-type)
  (if (or (eq known-type 't)
	  (eq (car new-type) 'eql))
      new-type
      (let ((so-far (if (and (consp known-type) (eq (car known-type) 'and))
			(cdr known-type)
			(list known-type))))
	(unless (eq (car new-type) 'not)
	  (setq so-far
		(mapcan #'(lambda (type)
			    (unless (*subtypep new-type type)
			      (list type)))
			so-far)))
	(if (null so-far)
	    new-type
	    `(and ,new-type ,@so-far)))))

#+lcl3.0 (dont-use-production-compiler)

(defun generate-discrimination-net-internal 
    (gf methods types methods-function test-function type-function)
  (let* ((arg-info (gf-arg-info gf))
	 (precedence (arg-info-precedence arg-info))
	 (nreq (arg-info-number-required arg-info))
	 (metatypes (arg-info-metatypes arg-info)))
    (labels ((do-column (p-tail contenders known-types)
	       (if p-tail
		   (let* ((position (car p-tail))
			  (known-type (or (nth position types) t)))
		     (if (eq (nth position metatypes) 't)
			 (do-column (cdr p-tail) contenders
				    (cons (cons position known-type) known-types))
			 (do-methods p-tail contenders 
				     known-type () known-types)))
		   (funcall methods-function contenders 
			    (let ((k-t (make-list nreq)))
			      (dolist (index+type known-types)
				(setf (nth (car index+type) k-t) (cdr index+type)))
			      k-t))))
	     (do-methods (p-tail contenders known-type winners known-types)
	       ;;
               ;; <contenders>
	       ;;   is a (sorted) list of methods that must be discriminated
               ;; <known-type>
	       ;;   is the type of this argument, constructed from tests already made.
               ;; <winners>
	       ;;   is a (sorted) list of methods that are potentially applicable
	       ;;   after the discrimination has been made.
	       ;;   
               (if (null contenders)
		   (do-column (cdr p-tail) winners
			      (cons (cons (car p-tail) known-type) known-types))
                   (let* ((position (car p-tail))
			  (method (car contenders))
			  (specl (nth position (method-specializers method)))
                          (type (funcall type-function (type-from-specializer specl))))
		     (multiple-value-bind (app-p maybe-app-p)
			 (specializer-applicable-using-type-p type known-type)
		       (flet ((determined-to-be (truth-value)
				(if truth-value app-p (not maybe-app-p)))
			      (do-if (truth &optional implied)
				(let ((ntype (if truth type `(not ,type))))
				  (do-methods p-tail
				    (cdr contenders)
				    (if implied
					known-type
					(augment-type ntype known-type))
				    (if truth
					(append winners `(,method))
					winners)
				    known-types))))
			 (cond ((determined-to-be nil) (do-if nil t))
			       ((determined-to-be t)   (do-if t   t))
			       (t (funcall test-function position type 
					   (do-if t) (do-if nil))))))))))
      (do-column precedence methods ()))))

#+lcl3.0 (use-previous-compiler)

(defun compute-secondary-dispatch-function (generic-function net &optional 
					    method-alist wrappers)
  (function-funcall (compute-secondary-dispatch-function1 generic-function net)
		    method-alist wrappers))

(defvar *eq-case-table-limit* 15)
(defvar *case-table-limit* 10)

(defun compute-mcase-parameters (case-list)
  (unless (eq 't (caar (last case-list)))
    (error "The key for the last case arg to mcase was not T"))
  (let* ((eq-p (dolist (case case-list t)
		 (unless (or (eq (car case) 't)
			     (symbolp (caar case)))
		   (return nil))))
	 (len (1- (length case-list)))
	 (type (cond ((= len 1)
		      :simple)
		     ((<= len
			  (if eq-p
			      *eq-case-table-limit*
			      *case-table-limit*))
		      :assoc)
		     (t
		      :hash-table))))
    (list eq-p type)))

(defmacro mlookup (key info default &optional eq-p type)
  (unless (or (eq eq-p 't) (null eq-p))
    (error "Invalid eq-p argument"))
  (ecase type
    (:simple
     `(if (,(if eq-p 'eq 'eql) ,key (car ,info))
          (cdr ,info)
          ,default))
    (:assoc
     `(dolist (e ,info ,default)
        (when (,(if eq-p 'eq 'eql) (car e) ,key)
	  (return (cdr e)))))
    (:hash-table
     `(gethash ,key ,info ,default))))

(defun net-test-converter (form)
  (if (atom form)
      (default-test-converter form)
      (case (car form)
	((invoke-effective-method-function invoke-fast-method-call)
	 '.call.)
	(methods
	 '.methods.)
	(unordered-methods
	 '.umethods.)
	(mcase
	 `(mlookup ,(cadr form) nil nil ,@(compute-mcase-parameters (cddr form))))
	(t (default-test-converter form)))))

(defun net-code-converter (form)
  (if (atom form)
      (default-code-converter form)
      (case (car form)
	((methods unordered-methods)
	 (let ((gensym (gensym)))
	   (values gensym
		   (list gensym))))
	(mcase
	 (let ((mp (compute-mcase-parameters (cddr form)))
	       (gensym (gensym)) (default (gensym)))
	   (values `(mlookup ,(cadr form) ,gensym ,default ,@mp)
		   (list gensym default))))
	(t
	 (default-code-converter form)))))

(defun net-constant-converter (form generic-function)
  (or (let ((c (methods-converter form generic-function)))
	(when c (list c)))
      (if (atom form)
	  (default-constant-converter form)
	  (case (car form)
	    (mcase
	     (let* ((mp (compute-mcase-parameters (cddr form)))
		    (list (mapcar #'(lambda (clause)
				      (let ((key (car clause))
					    (meth (cadr clause)))
					(cons (if (consp key) (car key) key)
					      (methods-converter
					       meth generic-function))))
				  (cddr form)))
		    (default (car (last list))))
	       (list (list* ':mcase mp (nbutlast list))
		     (cdr default))))
	    (t
	     (default-constant-converter form))))))

(defun methods-converter (form generic-function)
  (cond ((and (consp form) (eq (car form) 'methods))
	 (cons '.methods.
	       (get-effective-method-function1 generic-function (cadr form))))
	((and (consp form) (eq (car form) 'unordered-methods))
	 (default-secondary-dispatch-function generic-function))))

(defun convert-methods (constant method-alist wrappers)
  (if (and (consp constant)
	   (eq (car constant) '.methods.))
      (funcall (cdr constant) method-alist wrappers)
      constant))

(defun convert-table (constant method-alist wrappers)
  (cond ((and (consp constant)
	      (eq (car constant) ':mcase))
	 (let ((alist (mapcar #'(lambda (k+m)
				  (cons (car k+m)
					(convert-methods (cdr k+m)
							 method-alist wrappers)))
			      (cddr constant)))
	       (mp (cadr constant)))
	   (ecase (cadr mp)
	     (:simple
	      (car alist))
	     (:assoc
	      alist)
	     (:hash-table
	      (let ((table (make-hash-table :test (if (car mp) 'eq 'eql))))
		(dolist (k+m alist)
		  (setf (gethash (car k+m) table) (cdr k+m)))
		table)))))))

(defun compute-secondary-dispatch-function1 (generic-function net
					     &optional function-p)
  (if (eq (car net) 'methods)
      (get-effective-method-function1 generic-function (cadr net))
      (let* ((name (generic-function-name generic-function))
	     (arg-info (gf-arg-info generic-function))
	     (metatypes (arg-info-metatypes arg-info))
	     (applyp (arg-info-applyp arg-info))
	     (fmc-arg-info (cons (length metatypes) applyp))
	     (arglist (if function-p
			  (make-dfun-lambda-list metatypes applyp)
			  (make-fast-method-call-lambda-list metatypes applyp))))
	(multiple-value-bind (cfunction constants)
	    (get-function1 `(lambda ,arglist
			      ,@(unless function-p
				  `((declare (ignore .pv-cell.
						     .next-method-call.))))
			      #+copy-&rest-arg
			      ,@(when (and applyp function-p)
				  `((setq .dfun-rest-arg.
					  (copy-list .dfun-rest-arg.))))
			      (let ((emf ,net))
			        ,(make-emf-call metatypes applyp 'emf)))
			   #'net-test-converter
			   #'net-code-converter
			   #'(lambda (form)
			       (net-constant-converter form generic-function)))
	  #'(lambda (method-alist wrappers)
	      (let* ((alist (list nil))
		     (alist-tail alist))
		(dolist (constant constants)
		  (let* ((a (or (dolist (a alist nil)
				  (when (eq (car a) constant)
				    (return a)))
				(cons constant
				      (or (convert-table
					   constant method-alist wrappers)
					  (convert-methods
					   constant method-alist wrappers)))))
			 (new (list a)))
		    (setf (cdr alist-tail) new)
		    (setf alist-tail new)))
		(let ((function (apply cfunction (mapcar #'cdr (cdr alist)))))
		  (if function-p
		      function
		      (make-fast-method-call
		       :function (set-function-name function `(sdfun-method ,name))
		       :arg-info fmc-arg-info)))))))))

(defvar *show-make-unordered-methods-emf-calls* nil)

(defun make-unordered-methods-emf (generic-function methods)
  (when *show-make-unordered-methods-emf-calls*
    (format t "~&make-unordered-methods-emf ~s~%" 
	    (generic-function-name generic-function)))
  #'(lambda (&rest args)
      #+copy-&rest-arg (setq args (copy-list args))
      (let* ((types (types-from-arguments generic-function args 'eql))
	     (smethods (sort-applicable-methods generic-function methods types))
	     (emf (get-effective-method-function generic-function smethods)))
	(invoke-emf emf args))))


;;;
;;; The value returned by compute-discriminating-function is a function
;;; object.  It is called a discriminating function because it is called
;;; when the generic function is called and its role is to discriminate
;;; on the arguments to the generic function and then call appropriate
;;; method functions.
;;; 
;;; A discriminating function can only be called when it is installed as
;;; the funcallable instance function of the generic function for which
;;; it was computed.
;;;
;;; More precisely, if compute-discriminating-function is called with an
;;; argument <gf1>, and returns a result <df1>, that result must not be
;;; passed to apply or funcall directly.  Rather, <df1> must be stored as
;;; the funcallable instance function of the same generic function <gf1>
;;; (using set-funcallable-instance-function).  Then the generic function
;;; can be passed to funcall or apply.
;;;
;;; An important exception is that methods on this generic function are
;;; permitted to return a function which itself ends up calling the value
;;; returned by a more specific method.  This kind of `encapsulation' of
;;; discriminating function is critical to many uses of the MOP.
;;; 
;;; As an example, the following canonical case is legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (let ((std (call-next-method)))
;;;       #'(lambda (arg)
;;;            (print (list 'call-to-gf gf arg))
;;;            (funcall std arg))))
;;;
;;; Because many discriminating functions would like to use a dynamic
;;; strategy in which the precise discriminating function changes with
;;; time it is important to specify how a discriminating function is
;;; permitted itself to change the funcallable instance function of the
;;; generic function.
;;;
;;; Discriminating functions may set the funcallable instance function
;;; of the generic function, but the new value must be generated by making
;;; a call to COMPUTE-DISCRIMINATING-FUNCTION.  This is to ensure that any
;;; more specific methods which may have encapsulated the discriminating
;;; function will get a chance to encapsulate the new, inner discriminating
;;; function.
;;;
;;; This implies that if a discriminating function wants to modify itself
;;; it should first store some information in the generic function proper,
;;; and then call compute-discriminating-function.  The appropriate method
;;; on compute-discriminating-function will see the information stored in
;;; the generic function and generate a discriminating function accordingly.
;;;
;;; The following is an example of a discriminating function which modifies
;;; itself in accordance with this protocol:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     #'(lambda (arg)
;;;         (cond (<some condition>
;;;                <store some info in the generic function>
;;;                (set-funcallable-instance-function
;;;                  gf
;;;                  (compute-discriminating-function gf))
;;;                (funcall gf arg))
;;;               (t
;;;                <call-a-method-of-gf>))))
;;;
;;; Whereas this code would not be legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     #'(lambda (arg)
;;;         (cond (<some condition>
;;;                (set-funcallable-instance-function
;;;                  gf
;;;                  #'(lambda (a) ..))
;;;                (funcall gf arg))
;;;               (t
;;;                <call-a-method-of-gf>))))
;;;
;;; NOTE:  All the examples above assume that all instances of the class
;;;        my-generic-function accept only one argument.
;;;
;;;
;;;
;;;
(defun slot-value-using-class-dfun (class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-reader-function slotd) object))

(defun setf-slot-value-using-class-dfun (new-value class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-writer-function slotd) new-value object))

(defun slot-boundp-using-class-dfun (class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-boundp-function slotd) object))

(defmethod compute-discriminating-function ((gf standard-generic-function))
  (with-slots (dfun-state arg-info) gf
    (typecase dfun-state
      (null (let ((name (generic-function-name gf)))
	      (when (eq name 'compute-applicable-methods)
		(update-all-c-a-m-gf-info gf))
	      (cond ((eq name 'slot-value-using-class)
		     (update-slot-value-gf-info gf 'reader)
		     #'slot-value-using-class-dfun)
		    ((equal name '(setf slot-value-using-class))
		     (update-slot-value-gf-info gf 'writer)
		     #'setf-slot-value-using-class-dfun)
		    ((eq name 'slot-boundp-using-class)
		     (update-slot-value-gf-info gf 'boundp)
		     #'slot-boundp-using-class-dfun)
		    ((gf-precompute-dfun-and-emf-p arg-info)
		     (make-final-dfun gf))
		    (t
		     (make-initial-dfun gf)))))
      (function dfun-state)
      (cons (car dfun-state)))))

(defmethod update-gf-dfun ((class std-class) gf)
  (let ((*new-class* class)
	#|| (name (generic-function-name gf)) ||#
	(arg-info (gf-arg-info gf)))
    (cond #||
	  ((eq name 'slot-value-using-class)
	   (update-slot-value-gf-info gf 'reader))
	  ((equal name '(setf slot-value-using-class))
	   (update-slot-value-gf-info gf 'writer))
	  ((eq name 'slot-boundp-using-class)
	   (update-slot-value-gf-info gf 'boundp))
	  ||#
	  ((gf-precompute-dfun-and-emf-p arg-info)
	   (multiple-value-bind (dfun cache info)
	       (make-final-dfun-internal gf)
	     (set-dfun gf dfun cache info) ; otherwise cache might get freed twice
	     (update-dfun gf dfun cache info))))))

;;;
;;;
;;;
(defmethod function-keywords ((method standard-method))
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (if (consp method)
			       (early-method-lambda-list method)
			       (method-lambda-list method)))
    (declare (ignore nreq nopt keysp restp))
    (values keywords allow-other-keys-p)))

(defun method-ll->generic-function-ll (ll)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords keyword-parameters)
      (analyze-lambda-list ll)
    (declare (ignore nreq nopt keysp restp allow-other-keys-p keywords))
    (remove-if #'(lambda (s)
		   (or (memq s keyword-parameters)
		       (eq s '&allow-other-keys)))
	       ll)))


;;;
;;; This is based on the rules of method lambda list congruency defined in
;;; the spec.  The lambda list it constructs is the pretty union of the
;;; lambda lists of all the methods.  It doesn't take method applicability
;;; into account at all yet.
;;; 
(defmethod generic-function-pretty-arglist
	   ((generic-function standard-generic-function))
  (let ((methods (generic-function-methods generic-function))
	(arglist ()))      
    (when methods
      (multiple-value-bind (required optional rest key allow-other-keys)
	  (method-pretty-arglist (car methods))
	(dolist (m (cdr methods))
	  (multiple-value-bind (method-key-keywords
				method-allow-other-keys
				method-key)
	      (function-keywords m)
	    ;; we've modified function-keywords to return what we want as
	    ;;  the third value, no other change here.
	    (declare (ignore method-key-keywords))
	    (setq key (union key method-key))
	    (setq allow-other-keys (or allow-other-keys
				       method-allow-other-keys))))
	(when allow-other-keys
	  (setq arglist '(&allow-other-keys)))
	(when key
	  (setq arglist (nconc (list '&key) key arglist)))
	(when rest
	  (setq arglist (nconc (list '&rest rest) arglist)))
	(when optional
	  (setq arglist (nconc (list '&optional) optional arglist)))
	(nconc required arglist)))))
  

(defmethod method-pretty-arglist ((method standard-method))
  (let ((required ())
	(optional ())
	(rest nil)
	(key ())
	(allow-other-keys nil)
	(state 'required)
	(arglist (method-lambda-list method)))
    (dolist (arg arglist)
      (cond ((eq arg '&optional)         (setq state 'optional))
	    ((eq arg '&rest)             (setq state 'rest))
	    ((eq arg '&key)              (setq state 'key))
	    ((eq arg '&allow-other-keys) (setq allow-other-keys 't))
	    ((memq arg lambda-list-keywords))
	    (t
	     (ecase state
	       (required (push arg required))
	       (optional (push arg optional))
	       (key      (push arg key))
	       (rest     (setq rest arg))))))
    (values (nreverse required)
	    (nreverse optional)
	    rest
	    (nreverse key)
	    allow-other-keys)))

