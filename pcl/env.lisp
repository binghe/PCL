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
;;; Basic environmental stuff.
;;;

(in-package :pcl)

#+Lucid
(progn

(defun pcl-arglist (function &rest other-args)
  (let ((defn nil))
    (cond ((and (fsc-instance-p function)
		(generic-function-p function))
	   (generic-function-pretty-arglist function))
	  ((and (symbolp function)
		(fboundp function)
		(setq defn (symbol-function function))
		(fsc-instance-p defn)
		(generic-function-p defn))
	   (generic-function-pretty-arglist defn))
	  (t (apply (original-definition 'sys::arglist)
		    function other-args)))))

(redefine-function 'sys::arglist 'pcl-arglist)

)


;;;
;;;
;;;

(defgeneric describe-object (object stream))

#-Genera
(progn

(defun pcl-describe (object #+Lispm &optional #+Lispm no-complaints)
  (let (#+Lispm (*describe-no-complaints* no-complaints))
    #+Lispm (declare (special *describe-no-complaints*))
    (describe-object object *standard-output*)
    (values)))

(defmethod describe-object (object stream)
  (let ((*standard-output* stream))
    (cond ((or #+kcl (packagep object))
	   (describe-package object stream))
	  (t
	   (funcall (original-definition 'describe) object)))))

(redefine-function 'describe 'pcl-describe)

)

(defmethod describe-object ((object slot-object) stream)
  (let* ((class (class-of object))
	 (slotds (slots-to-inspect class object))
	 (max-slot-name-length 0)
	 (instance-slotds ())
	 (class-slotds ())
	 (other-slotds ()))
    (flet ((adjust-slot-name-length (name)
	     (setq max-slot-name-length
		   (max max-slot-name-length
			(length (the string (symbol-name name))))))
	   (describe-slot (name value &optional (allocation () alloc-p))
	     (if alloc-p
		 (format stream
			 "~% ~A ~S ~VT  ~S"
			 name allocation (+ max-slot-name-length 7) value)
		 (format stream
			 "~% ~A~VT  ~S"
			 name max-slot-name-length value))))
      ;; Figure out a good width for the slot-name column.
      (dolist (slotd slotds)
	(adjust-slot-name-length (slot-definition-name slotd))
	(case (slot-definition-allocation slotd)
	  (:instance (push slotd instance-slotds))
	  (:class  (push slotd class-slotds))
	  (otherwise (push slotd other-slotds))))
      (setq max-slot-name-length  (min (+ max-slot-name-length 3) 30))
      (format stream "~%~S is an instance of class ~S:" object class)

      (when instance-slotds
	(format stream "~% The following slots have :INSTANCE allocation:")
	(dolist (slotd (nreverse instance-slotds))
	  (describe-slot (slot-definition-name slotd)
			 (slot-value-or-default object (slot-definition-name slotd)))))

      (when class-slotds
	(format stream "~% The following slots have :CLASS allocation:")
	(dolist (slotd (nreverse class-slotds))
	  (describe-slot (slot-definition-name slotd)
			 (slot-value-or-default object (slot-definition-name slotd)))))

      (when other-slotds 
	(format stream "~% The following slots have allocation as shown:")
	(dolist (slotd (nreverse other-slotds))
	  (describe-slot (slot-definition-name slotd)
			 (slot-value-or-default object (slot-definition-name slotd))
			 (slot-definition-allocation slotd))))
      (values))))

(defmethod slots-to-inspect ((class slot-class) (object slot-object))
  (class-slots class))

(defvar *describe-metaobjects-as-objects-p* nil)

(defmethod describe-object ((fun standard-generic-function) stream)
  (format stream "~A is a generic function.~%" fun)
  (format stream "Its arguments are:~%  ~S~%"
          (generic-function-pretty-arglist fun))
  (format stream "Its methods are:")
  (dolist (meth (generic-function-methods fun))
    (format stream "~2%    ~{~S ~}~:S =>~%"
            (method-qualifiers meth)
            (unparse-specializers meth))
    (describe-object (or (method-fast-function meth)
			 (method-function meth))
		     stream))
  (when *describe-metaobjects-as-objects-p*
    (call-next-method)))

;;;
;;;
;;;
(defmethod describe-object ((class class) stream)
  (flet ((pretty-class (c) (or (class-name c) c)))
    (macrolet ((ft (string &rest args) `(format stream ,string ,@args)))
      (ft "~&~S is a class, it is an instance of ~S.~%"
	  class (pretty-class (class-of class)))
      (let ((name (class-name class)))
	(if name
	    (if (eq class (find-class name nil))
		(ft "Its proper name is ~S.~%" name)
		(ft "Its name is ~S, but this is not a proper name.~%" name))
	    (ft "It has no name (the name is NIL).~%")))
      (ft "The direct superclasses are: ~:S, and the direct~%~
           subclasses are: ~:S.  The class precedence list is:~%~S~%~
           There are ~D methods specialized for this class."
	  (mapcar #'pretty-class (class-direct-superclasses class))
	  (mapcar #'pretty-class (class-direct-subclasses class))
	  (mapcar #'pretty-class (class-precedence-list class))
	  (length (specializer-direct-methods class)))))
  (when *describe-metaobjects-as-objects-p*
    (call-next-method)))

(defun describe-package (object stream)
  (unless (packagep object) (setq object (find-package object)))
  (format stream "~&~S is a ~S.~%" object (type-of object))
  (let ((nick (package-nicknames object)))
    (when nick
      (format stream "You can also call it~@[ ~{~S~^, ~} or~] ~S.~%"
	      (butlast nick) (first (last nick)))))  
  (let* (#+cmu (internal (lisp::package-internal-symbols object))
	 (internal-count #+cmu (- (lisp::package-hashtable-size internal)
				  (lisp::package-hashtable-free internal))
			 #-cmu 0)
	 #+cmu (external (lisp::package-external-symbols object))
	 (external-count #+cmu (- (lisp::package-hashtable-size external)
				  (lisp::package-hashtable-free external))
			 #-cmu 0))
    #-cmu (do-external-symbols (sym object)
	    (declare (ignore sym))
	    (incf external-count))
    #-cmu (do-symbols (sym object)
	    (declare (ignore sym))
	    (incf internal-count))
    #-cmu (decf internal-count external-count)
    (format stream "It has ~D internal and ~D external symbols (~D total).~%"
	    internal-count external-count (+ internal-count external-count)))
  (let ((used (package-use-list object)))
    (when used
      (format stream "It uses the packages ~{~S~^, ~}.~%"
	      (mapcar #'package-name used))))
  (let ((users (package-use-list object)))
    (when users
      (format stream "It is used by the packages ~{~S~^, ~}.~%"
	      (mapcar #'package-name users)))))

#+cmu
(defmethod describe-object ((object package) stream)
  (describe-package object stream))

#+cmu
(defmethod describe-object ((object hash-table) stream)
  (format stream "~&~S is an ~a hash table."
	  object
	  #-cmu17 (lisp::hash-table-kind object)
	  #+cmu17 (lisp::hash-table-test object))
  (format stream "~&Its size is ~d buckets."
	  (lisp::hash-table-size object))
  (format stream "~&Its rehash-size is ~d."
	  (lisp::hash-table-rehash-size object))
  (format stream "~&Its rehash-threshold is ~d."
	  (lisp::hash-table-rehash-threshold object))
  (format stream "~&It currently holds ~d entries."
	  (lisp::hash-table-number-entries object)))



;;;
;;; trace-method and untrace-method accept method specs as arguments.  A
;;; method-spec should be a list like:
;;;   (<generic-function-spec> qualifiers* (specializers*))
;;; where <generic-function-spec> should be either a symbol or a list
;;; of (SETF <symbol>).
;;;
;;;   For example, to trace the method defined by:
;;;
;;;     (defmethod foo ((x spaceship)) 'ss)
;;;
;;;   You should say:
;;;
;;;     (trace-method '(foo (spaceship)))
;;;
;;;   You can also provide a method object in the place of the method
;;;   spec, in which case that method object will be traced.
;;;
;;; For untrace-method, if an argument is given, that method is untraced.
;;; If no argument is given, all traced methods are untraced.
;;;
(defclass traced-method (method)
     ((method :initarg :method)
      (function :initarg :function
		:reader method-function)
      (generic-function :initform nil
			:accessor method-generic-function)))

(defmethod method-lambda-list ((m traced-method))
  (with-slots (method) m (method-lambda-list method)))

(defmethod method-specializers ((m traced-method))
  (with-slots (method) m (method-specializers method)))

(defmethod method-qualifiers ((m traced-method))
  (with-slots (method) m (method-qualifiers method)))

(defmethod accessor-method-slot-name ((m traced-method))
  (with-slots (method) m (accessor-method-slot-name method)))

(defvar *traced-methods* ())

(defun trace-method (spec &rest options)
  #+copy-&rest-arg (setq options (copy-list options))
  (multiple-value-bind (gf omethod name)
      (parse-method-or-spec spec)
    (let* ((tfunction (trace-method-internal (method-function omethod)
					     name
					     options))
	   (tmethod (make-instance 'traced-method
				   :method omethod
				   :function tfunction)))
      (remove-method gf omethod)
      (add-method gf tmethod)
      (pushnew tmethod *traced-methods*)
      tmethod)))

(defun untrace-method (&optional spec)  
  (flet ((untrace-1 (m)
	   (let ((gf (method-generic-function m)))
	     (when gf
	       (remove-method gf m)
	       (add-method gf (slot-value m 'method))
	       (setq *traced-methods* (remove m *traced-methods*))))))
    (if (not (null spec))
	(multiple-value-bind (gf method)	    
	    (parse-method-or-spec spec)
	  (declare (ignore gf))
	  (if (memq method *traced-methods*)
	      (untrace-1 method)
	      (error "~S is not a traced method?" method)))
	(dolist (m *traced-methods*) (untrace-1 m)))))

(defun trace-method-internal (ofunction name options)
  (eval `(untrace ,name))
  (setf (symbol-function name) ofunction)
  (eval `(trace ,name ,@options))
  (symbol-function name))




;(defun compile-method (spec)
;  (multiple-value-bind (gf method name)
;      (parse-method-or-spec spec)
;    (declare (ignore gf))
;    (compile name (method-function method))
;    (setf (method-function method) (symbol-function name))))

(defmacro undefmethod (&rest args)
  #+(or (not :lucid) :lcl3.0)
  (declare (arglist name {method-qualifier}* specializers))
  `(undefmethod-1 ',args))

(defun undefmethod-1 (args)
  (multiple-value-bind (gf method)
      (parse-method-or-spec args)
    (when (and gf method)
      (remove-method gf method)
      method)))


(pushnew :pcl *features*)
(pushnew :portable-commonloops *features*)
(pushnew :pcl-structures *features*)
