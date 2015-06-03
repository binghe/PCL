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
;;; Permutation vectors.
;;;

(in-package :pcl)

(defmacro instance-slot-index (wrapper slot-name)
  `(let ((pos 0))
     (declare (fixnum pos))
     (block loop
       (dolist (sn (wrapper-instance-slots-layout ,wrapper))
	 (when (eq ,slot-name sn) (return-from loop pos))
	 (incf pos)))))


;;;
;;;
;;;
(defun pv-cache-limit-fn (nlines)
  (default-limit-fn nlines))

(defstruct (pv-table
	     (:predicate pv-tablep)
	     (:constructor make-pv-table-internal
			   (slot-name-lists call-list)))
  (cache nil :type (or cache null))
  (pv-size 0 :type fixnum)
  (slot-name-lists nil :type list)
  (call-list nil :type list))

(defvar *initial-pv-table* (make-pv-table-internal nil nil))

; help new slot-value-using-class methods affect fast iv access
(defvar *all-pv-table-list* nil) 

(defun make-pv-table (&key slot-name-lists call-list)
  (let ((pv-table (make-pv-table-internal slot-name-lists call-list)))
    (push pv-table *all-pv-table-list*)
    pv-table))

(defun make-pv-table-type-declaration (var)
  `(type pv-table ,var))

(defvar *slot-name-lists-inner* (make-hash-table :test #'equal))
(defvar *slot-name-lists-outer* (make-hash-table :test #'equal))

;entries in this are lists of (table . pv-offset-list)
(defvar *pv-key-to-pv-table-table* (make-hash-table :test 'equal))

(defun intern-pv-table (&key slot-name-lists call-list)
  (let ((new-p nil))
    (flet ((inner (x)
	     (or (gethash x *slot-name-lists-inner*)
		 (setf (gethash x *slot-name-lists-inner*) (copy-list x))))
	   (outer (x)
	     (or (gethash x *slot-name-lists-outer*)
		 (setf (gethash x *slot-name-lists-outer*)
		       (let ((snl (copy-list (cdr x)))
			     (cl (car x)))
			 (setq new-p t)
			 (make-pv-table :slot-name-lists snl
					:call-list cl))))))
    (let ((pv-table (outer (mapcar #'inner (cons call-list slot-name-lists)))))
      (when new-p
	(let ((pv-index 1))
	  (dolist (slot-name-list slot-name-lists)
	    (dolist (slot-name (cdr slot-name-list))
	      (note-pv-table-reference slot-name pv-index pv-table)
	      (incf pv-index)))
	  (dolist (gf-call call-list)
	    (note-pv-table-reference gf-call pv-index pv-table)
	    (incf pv-index))
	  (setf (pv-table-pv-size pv-table) pv-index)))
      pv-table))))

(defun note-pv-table-reference (ref pv-offset pv-table)
  (let ((entry (gethash ref *pv-key-to-pv-table-table*)))
    (when (listp entry)
      (let ((table-entry (assq pv-table entry)))
	(when (and (null table-entry)
		   (> (length entry) 8))
	  (let ((new-table-table (make-hash-table :size 16 :test 'eq)))
	    (dolist (table-entry entry)
	      (setf (gethash (car table-entry) new-table-table)
		    (cdr table-entry)))
	    (setf (gethash ref *pv-key-to-pv-table-table*) new-table-table)))
	(when (listp entry)
	  (if (null table-entry)
	      (let ((new (cons pv-table pv-offset)))
		(if (consp entry)
		    (push new (cdr entry))
		    (setf (gethash ref *pv-key-to-pv-table-table*) (list new))))
	      (push pv-offset (cdr table-entry)))
	  (return-from note-pv-table-reference nil))))
    (let ((list (gethash pv-table entry)))
      (if (consp list)
	  (push pv-offset (cdr list))
	  (setf (gethash pv-table entry) (list pv-offset)))))
  nil)

(defun map-pv-table-references-of (ref function)
  (let ((entry (gethash ref *pv-key-to-pv-table-table*)))
    (if (listp entry)
	(dolist (table+pv-offset-list entry)
	  (funcall function
		   (car table+pv-offset-list) (cdr table+pv-offset-list)))
	(maphash function entry)))
  ref)


(defvar *pvs* (make-hash-table :test #'equal))

(defun optimize-slot-value-by-class-p (class slot-name type)
  (or (not (eq *boot-state* 'complete))
      (let ((slotd (find-slot-definition class slot-name)))
	(and slotd 
	     (slot-accessor-std-p slotd type)))))

(defun compute-pv-slot (slot-name wrapper class class-slots class-slot-p-cell)
  (if (symbolp slot-name)
      (when (optimize-slot-value-by-class-p class slot-name 'all)
	(or (instance-slot-index wrapper slot-name)
	    (let ((cell (assq slot-name class-slots)))
	      (when cell
		(setf (car class-slot-p-cell) t)
		cell))))
      (when (consp slot-name)
	(dolist (type '(reader writer) nil)
	  (when (eq (car slot-name) type)
	    (return
	      (let* ((gf-name (cadr slot-name))
		     (gf (gdefinition gf-name))
		     (location 
		      (when (eq *boot-state* 'complete)
			(accessor-values1 gf type class))))
		(when (consp location)
		  (setf (car class-slot-p-cell) t))
		location)))))))

(defun compute-pv (slot-name-lists wrappers)
  (unless (listp wrappers) (setq wrappers (list wrappers)))
  (let* ((not-simple-p-cell (list nil))
	 (elements
	  (gathering1 (collecting)
	    (iterate ((slot-names (list-elements slot-name-lists)))
	      (when slot-names
		(let* ((wrapper     (pop wrappers))
		       (class       (wrapper-class* wrapper))
		       (class-slots (wrapper-class-slots wrapper)))
		  (dolist (slot-name (cdr slot-names))
		    (gather1
		     (compute-pv-slot slot-name wrapper class 
				      class-slots not-simple-p-cell)))))))))
    (if (car not-simple-p-cell)
	(make-permutation-vector (cons t elements))
	(or (gethash elements *pvs*)
	    (setf (gethash elements *pvs*)
		  (make-permutation-vector (cons nil elements)))))))

(defun compute-calls (call-list wrappers)
  (declare (ignore call-list wrappers))
  #||
  (map 'vector
       #'(lambda (call)
	   (compute-emf-from-wrappers call wrappers))
       call-list)
  ||#  
  '#())

#|| ; Need to finish this, then write the maintenance functions.
(defun compute-emf-from-wrappers (call wrappers)
  (when call
    (destructuring-bind (gf-name nreq restp arg-info) call
      (if (eq gf-name 'make-instance)
	  (error "should not get here") ; there is another mechanism for this.
	  #'(lambda (&rest args)
	      (if (not (eq *boot-state* 'complete))
		  (apply (gdefinition gf-name) args)
		  (let* ((gf (gdefinition gf-name))
			 (arg-info (arg-info-reader gf))
			 (classes '?)
			 (types '?)
			 (emf (cache-miss-values-internal gf arg-info 
							  wrappers classes types 
							  'caching)))
		    (update-all-pv-tables call wrappers emf)
		    #+copy-&rest-arg (setq args (copy-list args))
		    (invoke-emf emf args))))))))
||#

(defun make-permutation-vector (indexes)
  (make-array (length indexes) :initial-contents indexes))

(defun pv-table-lookup (pv-table pv-wrappers)
  (let* ((slot-name-lists (pv-table-slot-name-lists pv-table))
	 (call-list (pv-table-call-list pv-table))
	 (cache (or (pv-table-cache pv-table)
		    (setf (pv-table-cache pv-table)
			  (get-cache (- (length slot-name-lists)
					(count nil slot-name-lists))
				     t
				     #'pv-cache-limit-fn
				     2)))))
    (or (probe-cache cache pv-wrappers)
	(let* ((pv (compute-pv slot-name-lists pv-wrappers))
	       (calls (compute-calls call-list pv-wrappers))
	       (pv-cell (cons pv calls))
	       (new-cache (fill-cache cache pv-wrappers pv-cell)))
	  (unless (eq new-cache cache)
	    (setf (pv-table-cache pv-table) new-cache)
	    (free-cache cache))
	  pv-cell))))

(defun make-pv-type-declaration (var)
  `(type simple-vector ,var))

(defvar *empty-pv* #())

(defmacro pvref (pv index)
  `(svref ,pv ,index))

(defmacro copy-pv (pv)
  `(copy-seq ,pv))

(defun make-calls-type-declaration (var)
  `(type simple-vector ,var))

(defmacro callsref (calls index)
  `(svref ,calls ,index))

(defvar *pv-table-cache-update-info* nil)

;called by: 
;(method shared-initialize :after (structure-class t))
;update-slots
(defun update-pv-table-cache-info (class)
  (let ((slot-names-for-pv-table-update nil)
	(new-icui nil))
    (dolist (icu *pv-table-cache-update-info*)
      (if (eq (car icu) class)
	  (pushnew (cdr icu) slot-names-for-pv-table-update)
	  (push icu new-icui)))
    (setq *pv-table-cache-update-info* new-icui)
    (when slot-names-for-pv-table-update
      (update-all-pv-table-caches class slot-names-for-pv-table-update))))

(defun update-all-pv-table-caches (class slot-names)
  (let* ((cwrapper (class-wrapper class))
	 (class-slots (wrapper-class-slots cwrapper))
	 (class-slot-p-cell (list nil))
	 (new-values (mapcar #'(lambda (slot-name)
				 (cons slot-name
				       (compute-pv-slot 
					slot-name cwrapper class 
					class-slots class-slot-p-cell)))
			     slot-names))
	 (pv-tables nil))
    (dolist (slot-name slot-names)
      (map-pv-table-references-of
       slot-name
       #'(lambda (pv-table pv-offset-list)
	   (declare (ignore pv-offset-list))
	   (pushnew pv-table pv-tables))))
    (dolist (pv-table pv-tables)
      (let* ((cache (pv-table-cache pv-table))
	     (slot-name-lists (pv-table-slot-name-lists pv-table))
	     (pv-size (pv-table-pv-size pv-table))
	     (pv-map (make-array pv-size)))
	(let ((map-index 1)(param-index 0))
	  (dolist (slot-name-list slot-name-lists)
	    (dolist (slot-name (cdr slot-name-list))
	      (let ((a (assoc slot-name new-values)))
		(setf (svref pv-map map-index)
		      (and a (cons param-index (cdr a)))))
	      (incf map-index))
	    (incf param-index)))
	(when cache
	  (map-cache #'(lambda (wrappers pv-cell)
			 (setf (car pv-cell)
			       (update-slots-in-pv wrappers (car pv-cell)
						   cwrapper pv-size pv-map)))
		     cache))))))

(defun update-slots-in-pv (wrappers pv cwrapper pv-size pv-map)
  (if (not (if (atom wrappers)
	       (eq cwrapper wrappers)
	       (dolist (wrapper wrappers nil)
		 (when (eq wrapper cwrapper)
		   (return t)))))
      pv
      (let* ((old-intern-p (listp (pvref pv 0)))
	     (new-pv (if old-intern-p
			 (copy-pv pv)
			 pv))
	     (new-intern-p t))
	(if (atom wrappers)
	    (dotimes (i pv-size)
	      (when (consp (let ((map (svref pv-map i)))
			     (if map
				 (setf (pvref new-pv i) (cdr map))
				 (pvref new-pv i))))
		(setq new-intern-p nil)))
	    (let ((param 0))
	      (dolist (wrapper wrappers)
		(when (eq wrapper cwrapper)
		  (dotimes (i pv-size)
		    (when (consp (let ((map (svref pv-map i)))
				   (if (and map (= (car map) param))
				       (setf (pvref new-pv i) (cdr map))
				       (pvref new-pv i))))
		      (setq new-intern-p nil))))
		(incf param))))
	(when new-intern-p
	  (setq new-pv (let ((list-pv (coerce pv 'list)))
			 (or (gethash (cdr list-pv) *pvs*)
			     (setf (gethash (cdr list-pv) *pvs*)
				   (if old-intern-p
				       new-pv
				       (make-permutation-vector list-pv)))))))
	new-pv)))


(defun maybe-expand-accessor-form (form required-parameters slots env)
  (let* ((fname (car form))
	 #||(len (length form))||#
	 (gf (if (symbolp fname)
		 (unencapsulated-fdefinition fname)
		 (gdefinition fname))))
    (macrolet ((maybe-optimize-reader ()
		 `(let ((parameter
			 (can-optimize-access1 (cadr form)
					       required-parameters env)))
		   (when parameter
		     (optimize-reader slots parameter gf-name form))))
	       (maybe-optimize-writer ()
		 `(let ((parameter
			 (can-optimize-access1 (caddr form)
					       required-parameters env)))
		   (when parameter
		     (optimize-writer slots parameter gf-name form)))))
      (unless (and (consp (cadr form))
		   (eq 'instance-accessor-parameter (caadr form)))
	(or #||
	    (cond ((and (= len 2) (symbolp fname))
		   (let ((gf-name (gethash fname *gf-declared-reader-table*)))
		     (when gf-name
		       (maybe-optimize-reader))))
		  ((= len 3)
		   (let ((gf-name (gethash fname *gf-declared-writer-table*)))
		     (when gf-name
		       (maybe-optimize-writer)))))
	    ||#
	    (when (and (eq *boot-state* 'complete)
		       (generic-function-p gf))
	      (let ((methods (generic-function-methods gf)))
		(when methods
		  (let* ((gf-name (generic-function-name gf))
			 (arg-info (gf-arg-info gf))
			 (metatypes (arg-info-metatypes arg-info))
			 (nreq (length metatypes))
			 (applyp (arg-info-applyp arg-info)))
		    (when (null applyp)
		      (cond ((= nreq 1)
			     (when (some #'standard-reader-method-p methods)
			       (maybe-optimize-reader)))
			    ((and (= nreq 2)
				  (consp gf-name)
				  (eq (car gf-name) 'setf))
			     (when (some #'standard-writer-method-p methods)
			       (maybe-optimize-writer))))))))))))))

(defun optimize-generic-function-call (form required-parameters env slots calls)
  (declare (ignore required-parameters env slots calls))
  (or (and (eq (car form) 'make-instance)
	   (expand-make-instance-form form))
      #||
      (maybe-expand-accessor-form form required-parameters slots env)
      (let* ((fname (car form))
	     (len (length form))
	     (gf (if (symbolp fname)
		     (and (fboundp fname)
			  (unencapsulated-fdefinition fname))
		     (and (gboundp fname)
			  (gdefinition fname))))
	     (gf-name (and (fsc-instance-p gf)
			   (if (early-gf-p gf)
			       (early-gf-name gf)
			       (generic-function-name gf)))))
	(when gf-name
	  (multiple-value-bind (nreq restp)
	      (get-generic-function-info gf)
	    (optimize-gf-call slots calls form nreq restp env))))
      ||#
      form))



(defun can-optimize-access (form required-parameters env)
  (let ((type (ecase (car form)
		(slot-value 'reader)
		(set-slot-value 'writer)
		(slot-boundp 'boundp)))
	(var (cadr form))
	(slot-name (eval (caddr form)))) ; known to be constant
    (can-optimize-access1 var required-parameters env type slot-name)))

(defun can-optimize-access1 (var required-parameters env &optional type slot-name)
  (when (and (consp var) (eq 'the (car var)))
    (setq var (caddr var)))
  (when (symbolp var)
    (let* ((rebound? (caddr (variable-declaration 'variable-rebinding var env)))
	   (parameter-or-nil (car (memq (or rebound? var) required-parameters))))
      (when parameter-or-nil
	(let* ((class-name (caddr (variable-declaration 
				   'class parameter-or-nil env)))
	       (class (find-class class-name nil)))
	  (when (or (not (eq *boot-state* 'complete))
		    (and class (not (class-finalized-p class))))
	    (setq class nil))
	  (when (and class-name (not (eq class-name 't)))
	    (when (or (null type)
		      (not (and class
				(memq *the-class-structure-object*
				      (class-precedence-list class))))
		      (optimize-slot-value-by-class-p class slot-name type))
	      (cons parameter-or-nil (or class class-name)))))))))

(defun optimize-slot-value (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form) form
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots :read sparameter slot-name nil)))
      `(accessor-slot-value ,@(cdr form))))

(defun optimize-set-slot-value (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form new-value) form
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots :write sparameter slot-name new-value)))
      `(accessor-set-slot-value ,@(cdr form))))

(defun optimize-slot-boundp (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form new-value) form
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots :boundp sparameter slot-name new-value)))
      `(accessor-slot-boundp ,@(cdr form))))

(defun optimize-reader (slots sparameter gf-name form)
  (if sparameter
      (optimize-accessor-call slots :read sparameter gf-name nil)
      form))

(defun optimize-writer (slots sparameter gf-name form)
  (if sparameter
      (destructuring-bind (ignore ignore new-value) form
	(optimize-accessor-call slots :write sparameter gf-name new-value))
      form))
;;;
;;; The <slots> argument is an alist, the CAR of each entry is the name of
;;; a required parameter to the function.  The alist is in order, so the
;;; position of an entry in the alist corresponds to the argument's position
;;; in the lambda list.
;;; 
(defun optimize-instance-access (slots read/write sparameter slot-name new-value)
  (let ((class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	(parameter (if (consp sparameter) (car sparameter) sparameter)))
    (if (and (eq *boot-state* 'complete)
	     (classp class)
	     (memq *the-class-structure-object* (class-precedence-list class)))
	(let ((slotd (find-slot-definition class slot-name)))
	  (ecase read/write
	    (:read
	     `(,(slot-definition-defstruct-accessor-symbol slotd) ,parameter))
	    (:write
	     `(setf (,(slot-definition-defstruct-accessor-symbol slotd) ,parameter)
	       ,new-value))
	    (:boundp
	     'T)))
	(let* ((parameter-entry (assq parameter slots))
	       (slot-entry      (assq slot-name (cdr parameter-entry)))
	       (position (posq parameter-entry slots))
	       (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
	  (unless parameter-entry
	    (error "Internal error in slot optimization."))
	  (unless slot-entry
	    (setq slot-entry (list slot-name))
	    (push slot-entry (cdr parameter-entry)))
	  (push pv-offset-form (cdr slot-entry))
	  (ecase read/write
	    (:read
	     `(instance-read ,pv-offset-form ,parameter ,position 
		             ',slot-name ',class))
	    (:write
	     `(let ((.new-value. ,new-value)) 
	        (instance-write ,pv-offset-form ,parameter ,position 
		                ',slot-name ',class .new-value.)))
	    (:boundp
	     `(instance-boundp ,pv-offset-form ,parameter ,position 
		               ',slot-name ',class)))))))

(defun optimize-accessor-call (slots read/write sparameter gf-name new-value)
  (let* ((class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	 (parameter (if (consp sparameter) (car sparameter) sparameter))
	 (parameter-entry (assq parameter slots))
	 (name (case read/write
		 (:read `(reader ,gf-name))
		 (:write `(writer ,gf-name))))
	 (slot-entry      (assoc name (cdr parameter-entry) :test #'equal))
	 (position (posq parameter-entry slots))
	 (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
    (unless parameter-entry
      (error "Internal error in slot optimization."))
    (unless slot-entry
      (setq slot-entry (list name))
      (push slot-entry (cdr parameter-entry)))
    (push pv-offset-form (cdr slot-entry))
    (ecase read/write
      (:read
       `(instance-reader ,pv-offset-form ,parameter ,position ,gf-name ',class))
      (:write
       `(let ((.new-value. ,new-value)) 
	  (instance-writer ,pv-offset-form ,parameter ,position ,gf-name ',class
	                   .new-value.))))))

(defvar *unspecific-arg* '..unspecific-arg..)

(defun optimize-gf-call-internal (form slots env)
  (when (and (consp form)
	     (eq (car form) 'the))
    (setq form (caddr form)))
  (or (and (symbolp form)
	   (let* ((rebound? (caddr (variable-declaration 'variable-rebinding
							 form env)))
		  (parameter-or-nil (car (assq (or rebound? form) slots))))
	     (when parameter-or-nil
	       (let* ((class-name (caddr (variable-declaration 
					  'class parameter-or-nil env))))
		 (when (and class-name (not (eq class-name 't)))
		   (position parameter-or-nil slots :key #'car))))))
      (if (constantp form)
	  (let ((form (eval form)))
	    (if (symbolp form)
		form
		*unspecific-arg*))
	  *unspecific-arg*)))

(defun optimize-gf-call (slots calls gf-call-form nreq restp env)
  (unless (eq (car gf-call-form) 'make-instance) ; needs more work
    (let* ((args (cdr gf-call-form))
	   (all-args-p (eq (car gf-call-form) 'make-instance))
	   (non-required-args (nthcdr nreq args))
	   (required-args (ldiff args non-required-args))
	   (call-spec (list (car gf-call-form) nreq restp
			    (mapcar #'(lambda (form)
					(optimize-gf-call-internal form slots env))
				    (if all-args-p
					args
					required-args))))
	   (call-entry (assoc call-spec calls :test #'equal))
	   (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
      (unless (some #'integerp 
		    (let ((spec-args (cdr call-spec)))
		      (if all-args-p 
			  (ldiff spec-args (nthcdr nreq spec-args))
			  spec-args)))
	(return-from optimize-gf-call nil))
      (unless call-entry
	(setq call-entry (list call-spec))
	(push call-entry (cdr calls)))
      (push pv-offset-form (cdr call-entry))
      (if (eq (car call-spec) 'make-instance)
	  `(funcall (pv-ref .pv. ,pv-offset-form) ,@(cdr gf-call-form))
	  `(let ((.emf. (pv-ref .pv. ,pv-offset-form)))
	    (invoke-effective-method-function .emf. ,restp
	     ,@required-args ,@(when restp `((list ,@non-required-args)))))))))
      

(define-walker-template pv-offset) ; These forms get munged by mutate slots.
(defmacro pv-offset (arg) arg)
(define-walker-template instance-accessor-parameter)
(defmacro instance-accessor-parameter (x) x)

;; It is safe for these two functions to be wrong.
;; They just try to guess what the most likely case will be.
(defun generate-fast-class-slot-access-p (class-form slot-name-form)
  (let ((class (and (constantp class-form) (eval class-form)))
	(slot-name (and (constantp slot-name-form) (eval slot-name-form))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
	 (let ((slotd (find-slot-definition class slot-name)))
	   (and slotd (classp (slot-definition-allocation slotd)))))))

(defun skip-fast-slot-access-p (class-form slot-name-form type)
  (let ((class (and (constantp class-form) (eval class-form)))
	(slot-name (and (constantp slot-name-form) (eval slot-name-form))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
	 (let ((slotd (find-slot-definition class slot-name)))
	   (and slotd (skip-optimize-slot-value-by-class-p class slot-name type))))))

(defun skip-optimize-slot-value-by-class-p (class slot-name type)
  (let ((slotd (find-slot-definition class slot-name)))
    (and slotd
	 (eq *boot-state* 'complete)
	 (not (slot-accessor-std-p slotd type)))))

(defmacro instance-read-internal (pv slots pv-offset default &optional type)
  (unless (member type '(nil :instance :class :default))
    (error "Illegal type argument to ~S: ~S" 'instance-read-internal type))
  (if (eq type ':default)
      default
      (let* ((index (gensym))
	     (value index))
	`(locally (declare #.*optimize-speed*)
	  (let ((,index (pvref ,pv ,pv-offset)))
	    (setq ,value (typecase ,index
			   ,@(when (or (null type) (eq type ':instance))
			       `((fixnum (%instance-ref ,slots ,index))))
			   ,@(when (or (null type) (eq type ':class))
			       `((cons (cdr ,index))))
			   (t ',*slot-unbound*)))
	    (if (eq ,value ',*slot-unbound*)
		,default
		,value))))))

(defmacro instance-read (pv-offset parameter position slot-name class)
  (if (skip-fast-slot-access-p class slot-name 'reader)
      `(accessor-slot-value ,parameter ,slot-name)
      `(instance-read-internal .pv. ,(slot-vector-symbol position)
	,pv-offset (accessor-slot-value ,parameter ,slot-name)
	,(if (generate-fast-class-slot-access-p class slot-name)
	     ':class ':instance))))

(defmacro instance-reader (pv-offset parameter position gf-name class)
  (declare (ignore class))
  `(instance-read-internal .pv. ,(slot-vector-symbol position)
    ,pv-offset 
    (,gf-name (instance-accessor-parameter ,parameter))
    :instance))

(defmacro instance-write-internal (pv slots pv-offset new-value default
				      &optional type)
  (unless (member type '(nil :instance :class :default))
    (error "Illegal type argument to ~S: ~S" 'instance-write-internal type))
  (if (eq type ':default)
      default
      (let* ((index (gensym)))
	`(locally (declare #.*optimize-speed*)
	  (let ((,index (pvref ,pv ,pv-offset)))
	    (typecase ,index
	      ,@(when (or (null type) (eq type ':instance))
		  `((fixnum (setf (%instance-ref ,slots ,index) ,new-value))))
	      ,@(when (or (null type) (eq type ':class))
		  `((cons (setf (cdr ,index) ,new-value))))
	      (t ,default)))))))

(defmacro instance-write (pv-offset parameter position slot-name class new-value)
  (if (skip-fast-slot-access-p class slot-name 'writer)
      `(accessor-set-slot-value ,parameter ,slot-name ,new-value)
      `(instance-write-internal .pv. ,(slot-vector-symbol position)
	,pv-offset ,new-value
	(accessor-set-slot-value ,parameter ,slot-name ,new-value)
	,(if (generate-fast-class-slot-access-p class slot-name)
	     ':class ':instance))))

(defmacro instance-writer (pv-offset parameter position gf-name class new-value)
  (declare (ignore class))
  `(instance-write-internal .pv. ,(slot-vector-symbol position)
    ,pv-offset ,new-value
    (,(if (consp gf-name)
	  (get-setf-function-name gf-name)
	  gf-name)
     (instance-accessor-parameter ,parameter)
     ,new-value)
    :instance))

(defmacro instance-boundp-internal (pv slots pv-offset default
				       &optional type)
  (unless (member type '(nil :instance :class :default))
    (error "Illegal type argument to ~S: ~S" 'instance-boundp-internal type))
  (if (eq type ':default)
      default
      (let* ((index (gensym)))
	`(locally (declare #.*optimize-speed*)
	  (let ((,index (pvref ,pv ,pv-offset)))
	    (typecase ,index
	      ,@(when (or (null type) (eq type ':instance))
		  `((fixnum (not (eq (%instance-ref ,slots ,index) ',*slot-unbound*)))))
	      ,@(when (or (null type) (eq type ':class))
		  `((cons (not (eq (cdr ,index) ',*slot-unbound*)))))
	      (t ,default)))))))

(defmacro instance-boundp (pv-offset parameter position slot-name class)
  (if (skip-fast-slot-access-p class slot-name 'boundp)
      `(accessor-slot-boundp ,parameter ,slot-name)
      `(instance-boundp-internal .pv. ,(slot-vector-symbol position)
	,pv-offset (accessor-slot-boundp ,parameter ,slot-name)
	,(if (generate-fast-class-slot-access-p class slot-name)
	     ':class ':instance))))

;;;
;;; This magic function has quite a job to do indeed.
;;;
;;; The careful reader will recall that <slots> contains all of the optimized
;;; slot access forms produced by OPTIMIZE-INSTANCE-ACCESS.  Each of these is
;;; a call to either INSTANCE-READ or INSTANCE-WRITE.
;;;
;;; At the time these calls were produced, the first argument was specified as
;;; the symbol .PV-OFFSET.; what we have to do now is convert those pv-offset
;;; arguments into the actual number that is the correct offset into the pv.
;;;
;;; But first, oh but first, we sort <slots> a bit so that for each argument
;;; we have the slots in alphabetical order.  This canonicalizes the PV-TABLE's a
;;; bit and will hopefully lead to having fewer PV's floating around.  Even
;;; if the gain is only modest, it costs nothing.
;;;  
(defun slot-name-lists-from-slots (slots calls)
  (multiple-value-bind (slots calls)
      (mutate-slots-and-calls slots calls)
    (let* ((slot-name-lists
	    (mapcar #'(lambda (parameter-entry)
			(cons nil (mapcar #'car (cdr parameter-entry))))
		    slots))
	   (call-list
	    (mapcar #'car calls)))
      (dolist (call call-list)
	(dolist (arg (cdr call))
	  (when (integerp arg)
	    (setf (car (nth arg slot-name-lists)) t))))
      (setq slot-name-lists (mapcar #'(lambda (r+snl)
					(when (or (car r+snl) (cdr r+snl))
					  r+snl))
				    slot-name-lists))
      (let ((cvt (apply #'vector
			(let ((i -1))
			  (mapcar #'(lambda (r+snl)
				      (when r+snl (incf i)))
				  slot-name-lists)))))
	(setq call-list (mapcar #'(lambda (call)
				    (cons (car call) 
					  (mapcar #'(lambda (arg)
						      (if (integerp arg)
							  (svref cvt arg)
							  arg))
						  (cdr call))))
				call-list)))
      (values slot-name-lists call-list))))

(defun mutate-slots-and-calls (slots calls)
  (let ((sorted-slots (sort-slots slots))
	(sorted-calls (sort-calls (cdr calls)))
	(pv-offset 0))  ; index 0 is for info
    (dolist (parameter-entry sorted-slots)
      (dolist (slot-entry (cdr parameter-entry))
	(incf pv-offset)	
	(dolist (form (cdr slot-entry))
	  (setf (cadr form) pv-offset))))
    (dolist (call-entry sorted-calls)
      (incf pv-offset)
      (dolist (form (cdr call-entry))
	(setf (cadr form) pv-offset)))
    (values sorted-slots sorted-calls)))

(defun symbol-pkg-name (sym) 
  (let ((pkg (symbol-package sym)))
    (if pkg (package-name pkg) "")))

(defun symbol-lessp (a b)
  (if (eq (symbol-package a)
	  (symbol-package b))
      (string-lessp (symbol-name a)
		    (symbol-name b))
      (string-lessp (symbol-pkg-name a)
		    (symbol-pkg-name b))))

(defun symbol-or-cons-lessp (a b)
  (etypecase a
    (symbol (etypecase b
	      (symbol (symbol-lessp a b))
	      (cons t)))
    (cons   (etypecase b
	      (symbol nil)
	      (cons (if (eq (car a) (car b))
			(symbol-or-cons-lessp (cdr a) (cdr b))
			(symbol-or-cons-lessp (car a) (car b))))))))

(defun sort-slots (slots)
  (mapcar #'(lambda (parameter-entry)
	      (cons (car parameter-entry)
		    (sort (cdr parameter-entry)	;slot entries
			  #'symbol-or-cons-lessp
			  :key #'car)))
	  slots))

(defun sort-calls (calls)
  (sort calls #'symbol-or-cons-lessp :key #'car))


;;;
;;; This needs to work in terms of metatypes and also needs to work for
;;; automatically generated reader and writer functions.
;;; -- Automatically generated reader and writer functions use this stuff too.

(defmacro pv-binding ((required-parameters slot-name-lists pv-table-symbol)
		      &body body)
  (with-gathering ((slot-vars (collecting))
		   (pv-parameters (collecting)))
    (iterate ((slots (list-elements slot-name-lists))
	      (required-parameter (list-elements required-parameters))
	      (i (interval :from 0)))
      (when slots
	(gather required-parameter pv-parameters)
	(gather (slot-vector-symbol i) slot-vars)))
    `(pv-binding1 (.pv. .calls. ,pv-table-symbol ,pv-parameters ,slot-vars)
       ,@body)))

(defmacro pv-binding1 ((pv calls pv-table-symbol pv-parameters slot-vars) 
		       &body body)
  `(pv-env (,pv ,calls ,pv-table-symbol ,pv-parameters)
     (let (,@(mapcar #'(lambda (slot-var p) `(,slot-var (get-slots-or-nil ,p)))
	       slot-vars pv-parameters))
	,@body)))

;This gets used only when the default make-method-lambda is overriden.
(defmacro pv-env ((pv calls pv-table-symbol pv-parameters)
		  &rest forms)
  `(let* ((.pv-table. ,pv-table-symbol)
	  (.pv-cell. (pv-table-lookup-pv-args .pv-table. ,@pv-parameters))
	  (,pv (car .pv-cell.))
	  (,calls (cdr .pv-cell.)))
     (declare ,(make-pv-type-declaration pv))
     (declare ,(make-calls-type-declaration calls))
     ,@(when (symbolp pv-table-symbol)
	 `((declare (special ,pv-table-symbol))))
     ,pv ,calls
     ,@forms))

(defvar *non-variable-declarations*
  '(method-name method-lambda-list
    optimize ftype inline notinline))

(defvar *variable-declarations-with-argument*
  '(class
    type))

(defvar *variable-declarations-without-argument*
  '(ignore special dynamic-extent
    array atom base-char bignum bit bit-vector character common compiled-function
    complex cons double-float extended-char fixnum float function hash-table integer
    keyword list long-float nil null number package pathname random-state ratio
    rational readtable sequence short-float signed-byte simple-array
    simple-bit-vector simple-string simple-vector single-float standard-char
    stream string-char symbol t unsigned-byte vector))

(defun split-declarations (body args)
  (let ((inner-decls nil) (outer-decls nil) decl)
    (loop (when (null body) (return nil))
	  (setq decl (car body))
	  (unless (and (consp decl)
		       (eq (car decl) 'declare))
	    (return nil))
	  (dolist (form (cdr decl))
	    (when (consp form)
	      (let ((declaration-name (car form)))
		(if (member declaration-name *non-variable-declarations*)
		    (push `(declare ,form) outer-decls)
		    (let ((arg-p
			   (member declaration-name
				   *variable-declarations-with-argument*))
			  (non-arg-p
			   (member declaration-name
				   *variable-declarations-without-argument*))
			  (dname (list (pop form)))
			  (inners nil) (outers nil))
		      (unless (or arg-p non-arg-p)
			(warn "The declaration ~S is not understood by ~S.~@
                               Please put ~S on one of the lists ~S,~%~S, or~%~S.~@
                        (Assuming it is a variable declarations without argument)."
			      declaration-name 'split-declarations
			      declaration-name
			      '*non-variable-declarations*
			      '*variable-declarations-with-argument*
			      '*variable-declarations-without-argument*)
			(push declaration-name
			      *variable-declarations-without-argument*))
		      (when arg-p
			(setq dname (append dname (list (pop form)))))
		      (dolist (var form)
			(if (member var args)
			    (push var outers)
			    (push var inners)))
		      (when outers
			(push `(declare (,@dname ,@outers)) outer-decls))
		      (when inners
			(push `(declare (,@dname ,@inners)) inner-decls)))))))
	  (setq body (cdr body)))
    (values outer-decls inner-decls body)))

(defun make-method-initargs-form-internal (method-lambda initargs env)
  (declare (ignore env))
  (let (method-lambda-args lmf lmf-params)
    (if (not (and (= 3 (length method-lambda))
		  (= 2 (length (setq method-lambda-args (cadr method-lambda))))
		  (consp (setq lmf (third method-lambda)))
		  (eq 'simple-lexical-method-functions (car lmf))
		  (eq (car method-lambda-args) (cadr (setq lmf-params (cadr lmf))))
		  (eq (cadr method-lambda-args) (caddr lmf-params))))
	`(list* :function #',method-lambda
	        ',initargs)
	(let* ((lambda-list (car lmf-params))
	       (nreq 0)(restp nil)(args nil))
	  (dolist (arg lambda-list)
	    (when (member arg '(&optional &rest &key))
	      (setq restp t)(return nil))
	    (when (eq arg '&aux) (return nil))
	    (incf nreq)(push arg args))
	  (setq args (nreverse args))
	  (setf (getf (getf initargs ':plist) ':arg-info) (cons nreq restp))
	  (make-method-initargs-form-internal1
	   initargs (cddr lmf) args lmf-params restp)))))

(defun make-method-initargs-form-internal1 
    (initargs body req-args lmf-params restp)
  (multiple-value-bind (outer-decls inner-decls body)
      (split-declarations body req-args)
    (let* ((rest-arg (when restp '.rest-arg.))
	   (args+rest-arg (if restp (append req-args (list rest-arg)) req-args)))
      `(list* :fast-function
	#'(lambda (.pv-cell. .next-method-call. ,@args+rest-arg)
	    ,@outer-decls
	    .pv-cell. .next-method-call.
	    (macrolet ((pv-env ((pv calls pv-table-symbol pv-parameters)
				&rest forms)
			 (declare (ignore pv-table-symbol pv-parameters))
			 `(let ((,pv (car .pv-cell.))
				(,calls (cdr .pv-cell.)))
			   (declare ,(make-pv-type-declaration pv)
			    ,(make-calls-type-declaration calls))
			   ,pv ,calls
			   ,@forms)))
	      (fast-lexical-method-functions 
	       (,(car lmf-params) .next-method-call. ,req-args ,rest-arg
		 ,@(cdddr lmf-params))
	       ,@inner-decls
	       ,@body)))
	',initargs))))

;use arrays and hash tables and the fngen stuff to make this much better.
;It doesn't really matter, though, because a function returned by this
;will get called only when the user explicitly funcalls a result of method-function. 
;BUT, this is needed to make early methods work.
(defun method-function-from-fast-function (fmf)
  (declare (type function fmf))
  (let* ((method-function nil) (pv-table nil)
	 (arg-info (method-function-get fmf ':arg-info))
	 (nreq (car arg-info))
	 (restp (cdr arg-info)))
    (setq method-function
	  #'(lambda (method-args next-methods)
	      (unless pv-table
		(setq pv-table (method-function-pv-table fmf)))
	      (let* ((pv-cell (when pv-table
				(get-method-function-pv-cell 
				 method-function method-args pv-table)))
		     (nm (car next-methods))
		     (nms (cdr next-methods))
		     (nmc (when nm
			    (make-method-call :function (if (std-instance-p nm)
							    (method-function nm)
							    nm)
					      :call-method-args (list nms)))))
		(if restp
		    (let* ((rest (nthcdr nreq method-args))
			   (args (ldiff method-args rest)))
		      (apply fmf pv-cell nmc (nconc args (list rest))))
		    (apply fmf pv-cell nmc method-args)))))
    (let* ((fname (method-function-get fmf :name))
	   (name `(,(or (get (car fname) 'method-sym)
			(setf (get (car fname) 'method-sym)
			      (let ((str (symbol-name (car fname))))
				(if (string= "FAST-" str :end2 5)
				    (intern (subseq str 5) *the-pcl-package*)
				    (car fname)))))
		    ,@(cdr fname))))
      (set-function-name method-function name))      
    (setf (method-function-get method-function :fast-function) fmf)
    method-function))

(defun get-method-function-pv-cell (method-function method-args &optional pv-table)
  (let ((pv-table (or pv-table (method-function-pv-table method-function))))
    (when pv-table
      (let ((pv-wrappers (pv-wrappers-from-all-args pv-table method-args)))
	(when pv-wrappers
	  (pv-table-lookup pv-table pv-wrappers))))))

(defun pv-table-lookup-pv-args (pv-table &rest pv-parameters)
  (pv-table-lookup pv-table (pv-wrappers-from-pv-args pv-parameters)))

(defun pv-wrappers-from-pv-args (&rest args)
  (let* ((nkeys (length args))
	 (pv-wrappers (make-list nkeys))
	 w (w-t pv-wrappers))
    (dolist (arg args)
      (setq w (cond ((std-instance-p arg)
		     (std-instance-wrapper arg))
		    ((fsc-instance-p arg)
		     (fsc-instance-wrapper arg))
		    (t
		     #+new-kcl-wrapper
		     (built-in-wrapper-of arg)
		     #-new-kcl-wrapper
		     (built-in-or-structure-wrapper arg))))
      (unless (eq 't (wrapper-state w))
	(setq w (check-wrapper-validity arg)))
      (setf (car w-t) w))
      (setq w-t (cdr w-t))
      (when (= nkeys 1) (setq pv-wrappers (car pv-wrappers)))
      pv-wrappers))

(defun pv-wrappers-from-all-args (pv-table args)
  (let ((nkeys 0)
	(slot-name-lists (pv-table-slot-name-lists pv-table)))
    (dolist (sn slot-name-lists)
      (when sn (incf nkeys)))
    (let* ((pv-wrappers (make-list nkeys))
	   (pv-w-t pv-wrappers))
      (dolist (sn slot-name-lists)
	(when sn
	  (let* ((arg (car args))
		 (w (wrapper-of arg)))
	    (unless w ; can-optimize-access prevents this from happening.
	      (error "error in pv-wrappers-from-all-args"))
	    (setf (car pv-w-t) w)
	    (setq pv-w-t (cdr pv-w-t))))
	(setq args (cdr args)))
      (when (= nkeys 1) (setq pv-wrappers (car pv-wrappers)))
      pv-wrappers)))

(defun pv-wrappers-from-all-wrappers (pv-table wrappers)
  (let ((nkeys 0)
	(slot-name-lists (pv-table-slot-name-lists pv-table)))
    (dolist (sn slot-name-lists)
      (when sn (incf nkeys)))
    (let* ((pv-wrappers (make-list nkeys))
	   (pv-w-t pv-wrappers))
      (dolist (sn slot-name-lists)
	(when sn
	  (let ((w (car wrappers)))
	    (unless w ; can-optimize-access prevents this from happening.
	      (error "error in pv-wrappers-from-all-wrappers"))
	    (setf (car pv-w-t) w)
	    (setq pv-w-t (cdr pv-w-t))))
	(setq wrappers (cdr wrappers)))
      (when (= nkeys 1) (setq pv-wrappers (car pv-wrappers)))
      pv-wrappers)))
