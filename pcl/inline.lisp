;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-

(in-package :pcl)

;; This file contains some of the things that will have to change to support
;; inlining of methods.

(defun make-method-lambda-internal (method-lambda &optional env)
  (unless (and (consp method-lambda) (eq (car method-lambda) 'lambda))
    (error "The method-lambda argument to make-method-lambda, ~S,~
            is not a lambda form" method-lambda))
  (multiple-value-bind (documentation declarations real-body)
      (extract-declarations (cddr method-lambda) env)
    (let* ((name-decl (get-declaration 'method-name declarations))
	   (sll-decl (get-declaration 'method-lambda-list declarations))
	   (method-name (when (consp name-decl) (car name-decl)))
	   (generic-function-name (when method-name (car method-name)))
	   (specialized-lambda-list (or sll-decl (cadr method-lambda))))
      (multiple-value-bind (parameters lambda-list specializers)
	  (parse-specialized-lambda-list specialized-lambda-list)
	(let* ((required-parameters
		(mapcar #'(lambda (r s) (declare (ignore s)) r)
			parameters
			specializers))
	       (slots (mapcar #'list required-parameters))
	       (calls (list nil))
	       (parameters-to-reference
		(make-parameter-references specialized-lambda-list
					   required-parameters
					   declarations
					   method-name
					   specializers))
	       (class-declarations
		`(declare
		  ,@(remove nil
			    (mapcar #'(lambda (a s) (and (symbolp s)
							 (neq s 't)
							 `(class ,a ,s)))
				    parameters
				    specializers))))
	       (method-lambda
		  ;; Remove the documentation string and insert the
		  ;; appropriate class declarations.  The documentation
		  ;; string is removed to make it easy for us to insert
		  ;; new declarations later, they will just go after the
		  ;; cadr of the method lambda.  The class declarations
		  ;; are inserted to communicate the class of the method's
		  ;; arguments to the code walk.
		  `(lambda ,lambda-list
		     ,class-declarations
		     ,@declarations
		     (progn ,@parameters-to-reference)
		     (block ,(if (listp generic-function-name)
				 (cadr generic-function-name)
				 generic-function-name)
		       ,@real-body)))
	       (constant-value-p (and (null (cdr real-body))
				      (constantp (car real-body))))
	       (constant-value (and constant-value-p
				    (eval (car real-body))))
	       (plist (if (and constant-value-p
			       (or (typep constant-value '(or number character))
				   (and (symbolp constant-value)
					(symbol-package constant-value))))
			  (list :constant-value constant-value)
			  ()))
	       (applyp (dolist (p lambda-list nil)
			 (cond ((memq p '(&optional &rest &key))
				(return t))
			       ((eq p '&aux)
				(return nil))))))
	    (multiple-value-bind (walked-lambda call-next-method-p closurep
						next-method-p-p)
		(walk-method-lambda method-lambda required-parameters env 
				    slots calls)
	      (multiple-value-bind (ignore walked-declarations walked-lambda-body)
		  (extract-declarations (cddr walked-lambda))
		(declare (ignore ignore))
		(when (or next-method-p-p call-next-method-p)
		  (setq plist (list* :needs-next-methods-p 't plist)))
		(when (some #'cdr slots)
		  (multiple-value-bind (slot-name-lists call-list)
		      (slot-name-lists-from-slots slots calls)
		    (let ((pv-table-symbol (make-symbol "pv-table")))
		      (setq plist 
			    `(,@(when slot-name-lists 
				  `(:slot-name-lists ,slot-name-lists))
			      ,@(when call-list
				  `(:call-list ,call-list))
			      :pv-table-symbol ,pv-table-symbol
			      ,@plist))
		      (setq walked-lambda-body
			    `((pv-binding (,required-parameters ,slot-name-lists
					   ,pv-table-symbol)
			       ,@walked-lambda-body))))))
		(when (and (memq '&key lambda-list)
			   (not (memq '&allow-other-keys lambda-list)))
		  (let ((aux (memq '&aux lambda-list)))
		    (setq lambda-list (nconc (ldiff lambda-list aux)
					     (list '&allow-other-keys)
					     aux))))
		(values `(lambda (.method-args. .next-methods.)
			   (simple-lexical-method-functions
			       (,lambda-list .method-args. .next-methods.
				:call-next-method-p ,call-next-method-p 
				:next-method-p-p ,next-method-p-p
				:closurep ,closurep
				:applyp ,applyp)
			     ,@walked-declarations
			     ,@walked-lambda-body))
			`(,@(when plist 
			      `(:plist ,plist))
			  ,@(when documentation 
			      `(:documentation ,documentation)))))))))))

(define-inline-function slot-value (instance slot-name) (form closure-p env)
  :predicate (and (not closure-p) (constantp slot-name))
  :inline-arguments (required-parameters slots)
  :inline (optimize-slot-value     
	   slots
	   (can-optimize-access form required-parameters env)
	   form))

;collect information about:
; uses of the required-parameters
; uses of call-next-method and next-method-p:
;   called-p
;   apply-p
;   arglist info
;optimize calls to slot-value, set-slot-value, slot-boundp
;optimize calls to find-class
;optimize generic-function calls
(defun make-walk-function (required-parameters info slots calls)
  #'(lambda (form context env)
      (cond ((not (eq context ':eval)) form)
	    ((not (listp form)) form)
	    ((eq (car form) 'call-next-method)
	     (setq call-next-method-p 't)
	     form)
	    ((eq (car form) 'next-method-p)
	     (setq next-method-p-p 't)
	     form)
	    ((and (eq (car form) 'function)
		  (cond ((eq (cadr form) 'call-next-method)
			 (setq call-next-method-p 't)
			 (setq closurep t)
			 form)
			((eq (cadr form) 'next-method-p)
			 (setq next-method-p-p 't)
			 (setq closurep t)
			 form)
			(t nil))))
	    ((and (or (eq (car form) 'slot-value)
		      (eq (car form) 'set-slot-value)
		      (eq (car form) 'slot-boundp))
		  (constantp (caddr form)))
	     (let ((parameter
		    (can-optimize-access form
					 required-parameters env)))
	       (ecase (car form)
		 (slot-value
		  (optimize-slot-value     slots parameter form))
		 (set-slot-value
		  (optimize-set-slot-value slots parameter form))
		 (slot-boundp
		  (optimize-slot-boundp    slots parameter form)))))
	    ((and (or (symbolp (car form))
		      (and (consp (car form))
			   (eq (caar form) 'setf)))
		  (gboundp (car form))
		  (if (eq *boot-state* 'complete)
		      (standard-generic-function-p (gdefinition (car form)))
		      (funcallable-instance-p (gdefinition (car form)))))
	     (optimize-generic-function-call 
	      form required-parameters env slots calls))
	    (t form))))

(defun walk-method-lambda (method-lambda required-parameters env slots calls)
  (let* ((call-next-method-p nil)   ;flag indicating that call-next-method
				    ;should be in the method definition
	 (closurep nil)		    ;flag indicating that #'call-next-method
				    ;was seen in the body of a method
	 (next-method-p-p nil)      ;flag indicating that next-method-p
				    ;should be in the method definition
	 (walk-functions `((call-next-method-p
			    ,#'(lambda (form closure-p env)
				 (setq call-next-method-p 't)
				 (when closure-p
				   (setq closurep t))
				 form))
			   (next-method-p
			    ,#'(lambda (form closure-p env)
				 (setq next-method-p-p 't)
				 (when closure-p
				   (setq closurep t))
				 form))
			   ((slot-value set-slot-value slot-boundp)
			    ,#'(lambda (form closure-p env)
				 (if (and (not closure-p)
					  (constantp (caddr form)))
				     
    (let ((walked-lambda (walk-form method-lambda env 
				    (make-walk-function 
				     `((call-next-method-p
					,#'(lambda (form closure-p env)
					     (setq call-next-method-p 't)
					     (when closure-p
					       (setq closurep t))
					     form))
				       (next-method-p
					,#'(lambda (form closure-p env)
					     (setq next-method-p-p 't)
					     (when closure-p
					       (setq closurep t))
					     form))
				       ((slot-value set-slot-value slot-boundp)
					,#'(lambda (form closure-p env)
					     (
      (values walked-lambda
	      call-next-method-p closurep next-method-p-p)))))

(defun initialize-method-function (initargs &optional return-function-p method)
  (let* ((mf (getf initargs ':function))
	 (method-spec (getf initargs ':method-spec))
	 (plist (getf initargs ':plist))
	 (pv-table-symbol (getf plist ':pv-table-symbol))
	 (pv-table nil)
	 (mff (getf initargs ':fast-function)))
    (flet ((set-mf-property (p v)
	     (when mf
	       (setf (method-function-get mf p) v))
	     (when mff
	       (setf (method-function-get mff p) v))))
      (when method-spec
	(when mf
	  (setq mf (set-function-name mf method-spec)))
	(when mff
	  (let ((name `(,(or (get (car method-spec) 'fast-sym)
			     (setf (get (car method-spec) 'fast-sym)
				   (intern (format nil "FAST-~A"
						   (car method-spec))
					   *the-pcl-package*)))
			 ,@(cdr method-spec))))
	    (set-function-name mff name)
	    (unless mf
	      (set-mf-property :name name)))))
      (when plist
	(let ((snl (getf plist :slot-name-lists))
	      (cl (getf plist :call-list)))
	  (when (or snl cl)
	    (setq pv-table (intern-pv-table :slot-name-lists snl
					    :call-list cl))
	    (when pv-table (set pv-table-symbol pv-table))
	    (set-mf-property :pv-table pv-table)))    
	(loop (when (null plist) (return nil))
	      (set-mf-property (pop plist) (pop plist)))      
	(when method
	  (set-mf-property :method method))    
	(when return-function-p
	  (or mf (method-function-from-fast-function mff)))))))



