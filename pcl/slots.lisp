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

(defmethod wrapper-fetcher ((class standard-class))
  'std-instance-wrapper)

(defmethod slots-fetcher ((class standard-class))
  'std-instance-slots)

(defmethod raw-instance-allocator ((class standard-class))
  'allocate-standard-instance)

;;;
;;; These four functions work on std-instances and fsc-instances.  These are
;;; instances for which it is possible to change the wrapper and the slots.
;;;
;;; For these kinds of instances, most specified methods from the instance
;;; structure protocol are promoted to the implementation-specific class
;;; std-class.  Many of these methods call these four functions.
;;;

(defun set-wrapper (inst new)
  (cond ((std-instance-p inst)
	 #+new-kcl-wrapper
	 (set-structure-def inst new)
	 #-new-kcl-wrapper
	 (setf (std-instance-wrapper inst) new))
	((fsc-instance-p inst)
	 (setf (fsc-instance-wrapper inst) new))
	(t
	 (error "What kind of instance is this?"))))

#+ignore ; can't do this when using #+new-kcl-wrapper
(defun set-slots (inst new)
  (cond ((std-instance-p inst)
	 (setf (std-instance-slots inst) new))
	((fsc-instance-p inst)
	 (setf (fsc-instance-slots inst) new))
	(t
	 (error "What kind of instance is this?"))))

(defun swap-wrappers-and-slots (i1 i2)
  (without-interrupts
   (cond ((std-instance-p i1)
	  #+new-kcl-wrapper
	  (swap-structure-contents i1 i2)
	  #-new-kcl-wrapper
	  (let ((w1 (std-instance-wrapper i1))
		(s1 (std-instance-slots i1)))
	    (setf (std-instance-wrapper i1) (std-instance-wrapper i2))
	    (setf (std-instance-slots i1) (std-instance-slots i2))
	    (setf (std-instance-wrapper i2) w1)
	    (setf (std-instance-slots i2) s1)))
	 ((fsc-instance-p i1)
	  (let ((w1 (fsc-instance-wrapper i1))
		(s1 (fsc-instance-slots i1)))
	    (setf (fsc-instance-wrapper i1) (fsc-instance-wrapper i2))
	    (setf (fsc-instance-slots i1) (fsc-instance-slots i2))
	    (setf (fsc-instance-wrapper i2) w1)
	    (setf (fsc-instance-slots i2) s1)))
	 (t
	  (error "What kind of instance is this?")))))






(defun get-class-slot-value-1 (object wrapper slot-name)
  (let ((entry (assoc slot-name (wrapper-class-slots wrapper))))
    (if (null entry)
	(slot-missing (wrapper-class wrapper) object slot-name 'slot-value)
	(if (eq (cdr entry) *slot-unbound*)
	    (slot-unbound (wrapper-class wrapper) object slot-name)
	    (cdr entry)))))

(defun set-class-slot-value-1 (new-value object wrapper slot-name)
  (let ((entry (assoc slot-name (wrapper-class-slots wrapper))))
    (if (null entry)
	(slot-missing (wrapper-class wrapper)
		      object
		      slot-name
		      'setf
		      new-value)
	(setf (cdr entry) new-value))))

(defmethod class-slot-value ((class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
	(prototype (class-prototype class)))
    (get-class-slot-value-1 prototype wrapper slot-name)))

(defmethod (setf class-slot-value) (nv (class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
	(prototype (class-prototype class)))
    (set-class-slot-value-1 nv prototype wrapper slot-name)))



(defun find-slot-definition (class slot-name)
  (dolist (slot (class-slots class) nil)
    (when (eql slot-name (slot-definition-name slot))
      (return slot))))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'slot-value)
	(slot-value-using-class class object slot-definition))))

(setf (gdefinition 'slot-value-normal) #'slot-value)

(define-compiler-macro slot-value (object-form slot-name-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-slot-value ,object-form ,slot-name-form)
      `(slot-value-normal ,object-form ,slot-name-form)))

(defun set-slot-value (object slot-name new-value)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'setf)
	(setf (slot-value-using-class class object slot-definition) 
	      new-value))))

(setf (gdefinition 'set-slot-value-normal) #'set-slot-value)

(define-compiler-macro set-slot-value (object-form slot-name-form new-value-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-set-slot-value ,object-form ,slot-name-form ,new-value-form)
      `(set-slot-value-normal ,object-form ,slot-name-form ,new-value-form)))

(defconstant *optimize-slot-boundp* nil)

(defun slot-boundp (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'slot-boundp)
	(slot-boundp-using-class class object slot-definition))))

(setf (gdefinition 'slot-boundp-normal) #'slot-boundp)

(define-compiler-macro slot-boundp (object-form slot-name-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-slot-boundp ,object-form ,slot-name-form)
      `(slot-boundp-normal ,object-form ,slot-name-form)))

(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
        (slot-missing class object slot-name 'slot-makunbound)
        (slot-makunbound-using-class class object slot-definition))))

(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    (not (null (find-slot-definition class slot-name)))))

;;;
;;; This isn't documented, but is used within PCL in a number of print
;;; object methods (see named-object-print-function).
;;; 
(defun slot-value-or-default (object slot-name &optional (default "unbound"))
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))


;;;
;;; 
;;; 
(defun standard-instance-access (instance location)
  (%instance-ref (std-instance-slots instance) location))

(defun funcallable-standard-instance-access (instance location)
  (%instance-ref (fsc-instance-slots instance) location))

(defmethod slot-value-using-class ((class std-class)
                                   (object standard-object)
                                   (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum 
		   (cond ((std-instance-p object)
			  (unless (eq 't (wrapper-state (std-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%instance-ref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%instance-ref (fsc-instance-slots object) location))
			 (t (error "What kind of instance is this?"))))
		  (cons
		   (cdr location))
		  (t
		   (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be read by the default ~s method."
			  slotd 'slot-value-using-class)))))
    (if (eq value *slot-unbound*)
	(slot-unbound class object (slot-definition-name slotd))
	value)))

(defmethod (setf slot-value-using-class)
	   (new-value (class std-class)
		      (object standard-object)
		      (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
	      (unless (eq 't (wrapper-state (std-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%instance-ref (std-instance-slots object) location) new-value))
	     ((fsc-instance-p object)
	      (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%instance-ref (fsc-instance-slots object) location) new-value))
	     (t (error "What kind of instance is this?"))))
      (cons
       (setf (cdr location) new-value))
      (t
       (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be written by the default ~s method."
	      slotd '(setf slot-value-using-class))))))

(defmethod slot-boundp-using-class
	   ((class std-class) 
	    (object standard-object) 
	    (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum 
		   (cond ((std-instance-p object)
			  (unless (eq 't (wrapper-state (std-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%instance-ref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%instance-ref (fsc-instance-slots object) location))
			 (t (error "What kind of instance is this?"))))
		  (cons
		   (cdr location))
		  (t
		   (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be read by the default ~s method."
			  slotd 'slot-boundp-using-class)))))
    (not (eq value *slot-unbound*))))

(defmethod slot-makunbound-using-class
	   ((class std-class)
	    (object standard-object) 
	    (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
	      (unless (eq 't (wrapper-state (std-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%instance-ref (std-instance-slots object) location) *slot-unbound*))
	     ((fsc-instance-p object)
	      (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%instance-ref (fsc-instance-slots object) location) *slot-unbound*))
	     (t (error "What kind of instance is this?"))))
      (cons
       (setf (cdr location) *slot-unbound*))
      (t
       (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be written by the default ~s method."
	      slotd 'slot-makunbound-using-class))))
  nil)

(defmethod slot-value-using-class
    ((class structure-class)
     (object structure-object)
     (slotd structure-effective-slot-definition))
  (let* ((function (slot-definition-internal-reader-function slotd))
	 (value (funcall function object)))
    #+cmu (declare (type function function))
    (if (eq value *slot-unbound*)
	(slot-unbound class object (slot-definition-name slotd))
	value)))

(defmethod (setf slot-value-using-class)
    (new-value (class structure-class)
	       (object structure-object)
	       (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-writer-function slotd)))
    #+cmu (declare (type function function))
    (funcall function new-value object)))

(defmethod slot-boundp-using-class
	   ((class structure-class) 
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
  #-new-kcl-wrapper t
  #+new-kcl-wrapper
  (let* ((function (slot-definition-internal-reader-function slotd))
	 (value (funcall function object)))
    #+cmu (declare (type function function))
    (not (eq value *slot-unbound*))))

(defmethod slot-makunbound-using-class
	   ((class structure-class)
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
  (error "Structure slots can't be unbound"))


(defmethod slot-missing
	   ((class t) instance slot-name operation &optional new-value)
  (error "When attempting to ~A,~%the slot ~S is missing from the object ~S."
	 (ecase operation
	   (slot-value "read the slot's value (slot-value)")
	   (setf (format nil
			 "set the slot's value to ~S (setf of slot-value)"
			 new-value))
	   (slot-boundp "test to see if slot is bound (slot-boundp)")
	   (slot-makunbound "make the slot unbound (slot-makunbound)"))
	 slot-name
	 instance))

(defmethod slot-unbound ((class t) instance slot-name)
  (error "The slot ~S is unbound in the object ~S." slot-name instance))

(defun slot-unbound-internal (instance position)
  (slot-unbound (class-of instance) instance 
		(etypecase position
		  (fixnum 
		   (nth position 
			(wrapper-instance-slots-layout (wrapper-of instance))))
		  (cons
		   (car position)))))


(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (allocate-standard-instance (class-wrapper class)))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  #-new-kcl-wrapper
  (let ((constructor (class-defstruct-constructor class)))
    (if constructor
	(funcall constructor)
	(error "Can't allocate an instance of class ~S" (class-name class))))
  #+new-kcl-wrapper
  (allocate-standard-instance (class-wrapper class)))


