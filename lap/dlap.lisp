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



(defun emit-one-class-reader (class-slot-p)
  (emit-reader/writer :reader 1 class-slot-p))

(defun emit-one-class-writer (class-slot-p)
  (emit-reader/writer :writer 1 class-slot-p))

(defun emit-two-class-reader (class-slot-p)
  (emit-reader/writer :reader 2 class-slot-p))

(defun emit-two-class-writer (class-slot-p)
  (emit-reader/writer :writer 2 class-slot-p))



(defun emit-reader/writer (reader/writer 1-or-2-class class-slot-p)
  (let ((instance nil)
	(arglist  ())
	(closure-variables ())
	(field (first-wrapper-cache-number-index))) 
    ;;we need some field to do the fast obsolete check
    (ecase reader/writer
      (:reader (setq instance (dfun-arg-symbol 0)
		     arglist  (list instance)))
      (:writer (setq instance (dfun-arg-symbol 1)
		     arglist  (list (dfun-arg-symbol 0) instance))))
    (ecase 1-or-2-class
      (1 (setq closure-variables '(wrapper-0 index miss-fn)))
      (2 (setq closure-variables '(wrapper-0 wrapper-1 index miss-fn))))
    (generating-lap closure-variables
		    arglist
       (with-lap-registers ((inst t)				   ;reg for the instance
			    (wrapper #-structure-wrapper vector	   ;reg for the wrapper
				     #+structure-wrapper t)
			    #+structure-wrapper (cnv fixnum-vector)
			    (cache-no index))			   ;reg for the cache no
	  (let ((index cache-no)				   ;This register is used
								   ;for different values at
								   ;different times.
		(slots (and (null class-slot-p)
			    (allocate-register #-new-kcl-wrapper 'vector
					       #+new-kcl-wrapper t)))
		(csv   (and class-slot-p
			    (allocate-register t))))
	    (prog1 (flatten-lap
		     (opcode :move (operand :arg instance) inst)   ;get the instance
		     (opcode :std-instance-p inst 'std-instance)   ;if not either std-inst
		     (opcode :fsc-instance-p inst 'fsc-instance)   ;or fsc-instance then
		     (opcode :go 'trap)				   ;we lose

		     (opcode :label 'fsc-instance)
		     (opcode :move (operand :fsc-wrapper inst) wrapper)
		     (and slots
			  (opcode :move (operand :fsc-slots inst) slots))
		     (opcode :go 'have-wrapper)

		     (opcode :label 'std-instance)
		     (opcode :move (operand :std-wrapper inst) wrapper)
		     (and slots
			  (opcode :move (operand :std-slots inst) slots))

		     (opcode :label 'have-wrapper)
		     #-structure-wrapper
		     (opcode :move (operand :cref wrapper field) cache-no)
		     #+structure-wrapper
		     (opcode :move (emit-wrapper-cache-number-vector wrapper) cnv)
		     #+structure-wrapper
		     (opcode :move (operand :cref cnv field) cache-no)
		     (opcode :izerop cache-no 'trap)		   ;obsolete wrapper?

		     (ecase 1-or-2-class
		       (1 (emit-check-1-class-wrapper wrapper 'wrapper-0 'trap))
		       (2 (emit-check-2-class-wrapper wrapper 'wrapper-0 'wrapper-1 'trap)))
		     
		     (if class-slot-p
			 (flatten-lap
			  (opcode :move (operand :cvar 'index) csv)
			  (ecase reader/writer
			   (:reader (emit-get-class-slot csv 'trap inst))
			   (:writer (emit-set-class-slot csv (car arglist) inst))))
		       (flatten-lap
			(opcode :move (operand :cvar 'index) index)
			(ecase reader/writer
			   (:reader (emit-get-slot slots index 'trap inst))
			   (:writer (emit-set-slot slots index (car arglist) inst)))))
	      
		     (opcode :label 'trap)
		     (emit-miss 'miss-fn))
	      (when slots (deallocate-register slots))
	      (when csv (deallocate-register csv))))))))



(defun emit-one-index-readers (class-slot-p)
  (let ((arglist (list (dfun-arg-symbol 0))))
    (generating-lap '(field cache-vector mask size index miss-fn)
		    arglist
      (with-lap-registers ((slots #-new-kcl-wrapper vector #+new-kcl-wrapper t))
	(emit-dlap  arglist
		    '(standard-instance)
		    'trap
		    (with-lap-registers ((index index))
		      (flatten-lap
			(opcode :move (operand :cvar 'index) index)
			(if class-slot-p
			    (emit-get-class-slot index 'trap slots)
			    (emit-get-slot slots index 'trap))))
		    (flatten-lap
		      (opcode :label 'trap)
		      (emit-miss 'miss-fn))
		    nil
		    (and (null class-slot-p) (list slots)))))))

(defun emit-one-index-writers (class-slot-p)
  (let ((arglist (list (dfun-arg-symbol 0) (dfun-arg-symbol 1))))
    (generating-lap '(field cache-vector mask size index miss-fn)
		    arglist
      (with-lap-registers ((slots #-new-kcl-wrapper vector #+new-kcl-wrapper t))
	(emit-dlap arglist
		   '(t standard-instance)
		   'trap
		   (with-lap-registers ((index index))
		     (flatten-lap
		       (opcode :move (operand :cvar 'index) index)
		       (if class-slot-p
			   (emit-set-class-slot index (dfun-arg-symbol 0) slots)
			   (emit-set-slot slots index (dfun-arg-symbol 0)))))
		   (flatten-lap
		     (opcode :label 'trap)
		     (emit-miss 'miss-fn))
		   nil
		   (and (null class-slot-p) (list nil slots)))))))



(defun emit-n-n-readers ()
  (let ((arglist (list (dfun-arg-symbol 0))))
    (generating-lap '(field cache-vector mask size miss-fn)
		    arglist
      (with-lap-registers ((slots #-new-kcl-wrapper vector #+new-kcl-wrapper t)
			   (index index))
	(emit-dlap arglist
		   '(standard-instance)
		   'trap
		   (emit-get-slot slots index 'trap)
		   (flatten-lap
		     (opcode :label 'trap)
		     (emit-miss 'miss-fn))
		   index
		   (list slots))))))

(defun emit-n-n-writers ()
  (let ((arglist (list (dfun-arg-symbol 0) (dfun-arg-symbol 1))))
    (generating-lap '(field cache-vector mask size miss-fn)
		    arglist
      (with-lap-registers ((slots #-new-kcl-wrapper vector #+new-kcl-wrapper t)
			   (index index))
	(flatten-lap
	  (emit-dlap arglist
		     '(t standard-instance)
		     'trap
		     (emit-set-slot slots index (dfun-arg-symbol 0))
		     (flatten-lap
		       (opcode :label 'trap)
		       (emit-miss 'miss-fn))
		     index
		     (list nil slots)))))))
  


(defun emit-checking (metatypes applyp)
  (let ((dlap-lambda-list (make-dlap-lambda-list metatypes applyp)))
    (generating-lap '(field cache-vector mask size 
		      #-excl-sun4 emf #+excl-sun4 function 
		      miss-fn)
		    dlap-lambda-list
      (emit-dlap (remove '&rest dlap-lambda-list)
		 metatypes		 
		 'trap
		 (with-lap-registers ((#-excl-sun4 emf #+excl-sun4 function t))
		   (flatten-lap
		     (opcode :move (operand :cvar 
					    #-excl-sun4 'emf #+excl-sun4 'function)
			     #-excl-sun4 emf 
			     #+excl-sun4 function)
		     #-excl-sun4 (opcode :emf-call emf)
		     #+excl-sun4 (opcode :jmp function)))
		 (with-lap-registers ((miss-function t))
		   (flatten-lap
		     (opcode :label 'trap)
		     (opcode :move (operand :cvar 'miss-fn) miss-function)
		     (opcode :jmp miss-function)))
		 nil))))

(defun emit-caching (metatypes applyp)
  (let ((dlap-lambda-list (make-dlap-lambda-list metatypes applyp)))
    (generating-lap '(field cache-vector mask size miss-fn)
		    dlap-lambda-list
      (with-lap-registers ((#-excl-sun4 emf #+excl-sun4 function t))
	(emit-dlap (remove '&rest dlap-lambda-list)
		   metatypes
		   'trap
		   (flatten-lap
		    #-excl-sun4 (opcode :emf-call emf)
		    #+excl-sun4 (opcode :jmp function))
		   (with-lap-registers ((miss-function t))
		     (flatten-lap
		       (opcode :label 'trap)
		       (opcode :move (operand :cvar 'miss-fn) miss-function)
		       (opcode :jmp miss-function)))
		   #-excl-sun4 emf #+excl-sun4 function)))))

(defun emit-constant-value (metatypes)
  (let ((dlap-lambda-list (make-dlap-lambda-list metatypes nil)))
    (generating-lap '(field cache-vector mask size miss-fn)
		    dlap-lambda-list
      (with-lap-registers ((value t))
	(emit-dlap dlap-lambda-list
		   metatypes
		   'trap
		   (flatten-lap
		     (opcode :return value))
		   (with-lap-registers ((miss-function t))
		     (flatten-lap
		       (opcode :label 'trap)
		       (opcode :move (operand :cvar 'miss-fn) miss-function)
		       (opcode :jmp miss-function)))
		   value)))))



(defun emit-check-1-class-wrapper (wrapper cwrapper-0 miss-label)
  (with-lap-registers ((cwrapper #-structure-wrapper vector
				 #+structure-wrapper t))
    (flatten-lap
     (opcode :move (operand :cvar cwrapper-0) cwrapper)
     (opcode :neq wrapper cwrapper miss-label))))		;wrappers not eq, trap

(defun emit-check-2-class-wrapper (wrapper cwrapper-0 cwrapper-1 miss-label)
  (with-lap-registers ((cwrapper #-structure-wrapper vector
				 #+structure-wrapper t))
    (flatten-lap
     (opcode :move (operand :cvar cwrapper-0) cwrapper)		;This is an OR.  Isn't
     (opcode :eq wrapper cwrapper 'hit-internal)		;assembly code fun
     (opcode :move (operand :cvar cwrapper-1) cwrapper)		;
     (opcode :neq wrapper cwrapper miss-label)			;
     (opcode :label 'hit-internal))))

(defun emit-get-slot (slots index trap-label &optional temp)
  (let ((slot-unbound (operand :constant *slot-unbound*)))
    (with-lap-registers ((val t :reuse temp))
      (flatten-lap
	(opcode :move (operand :instance-ref slots index) val)  ;get slot value
	(opcode :eq val slot-unbound trap-label)		;is the slot unbound?
	(opcode :return val)))))				;return the slot value

(defun emit-set-slot (slots index new-value-arg &optional temp)
  (with-lap-registers ((new-val t :reuse temp))
    (flatten-lap
      (opcode :move (operand :arg new-value-arg) new-val)	;get new value into a reg
      (opcode :move new-val (operand :instance-ref slots index));set slot value
      (opcode :return new-val))))

(defun emit-get-class-slot (index trap-label &optional temp)
  (let ((slot-unbound (operand :constant *slot-unbound*)))
    (with-lap-registers ((val t :reuse temp))
      (flatten-lap
	(opcode :move (operand :cdr index) val)
	(opcode :eq val slot-unbound trap-label)
	(opcode :return val)))))

(defun emit-set-class-slot (index new-value-arg &optional temp)
  (with-lap-registers ((new-val t :reuse temp))
    (flatten-lap
      (opcode :move (operand :arg new-value-arg) new-val)
      (opcode :move new-val (operand :cdr index))
      (opcode :return new-val))))

(defun emit-miss (miss-fn)
  (with-lap-registers ((miss-fn-reg t))
    (flatten-lap
     (opcode :move (operand :cvar miss-fn) miss-fn-reg)		;get the miss function
     (opcode :jmp miss-fn-reg))))				;and call it



(defun dlap-wrappers (metatypes)
  (mapcar #'(lambda (x) (and (neq x 't)
			     (allocate-register #-structure-wrapper 'vector
						#+structure-wrapper t)))
	  metatypes))

(defun dlap-wrapper-moves (wrappers args metatypes miss-label slot-regs)
  (gathering1 (collecting)
    (iterate ((mt (list-elements metatypes))
	      (arg (list-elements args))
	      (wrapper (list-elements wrappers))
	      (i (interval :from 0)))
       (when wrapper
         (gather1
	   (emit-fetch-wrapper mt arg wrapper miss-label (nth i slot-regs)))))))

(defun emit-dlap (args metatypes miss-label hit miss value-reg &optional slot-regs)
  (let* ((wrappers (dlap-wrappers metatypes))
	 (nwrappers (remove nil wrappers))
	 (wrapper-moves (dlap-wrapper-moves wrappers args metatypes miss-label slot-regs)))
    (prog1 (emit-dlap-internal nwrappers
			       wrapper-moves
			       hit
			       miss
			       miss-label
			       value-reg)
	   (mapc #'deallocate-register nwrappers))))

(defun emit-dlap-internal (wrapper-regs wrapper-moves hit miss miss-label value-reg)
  (cond ((cdr wrapper-regs)
	 (emit-greater-than-1-dlap
	   wrapper-regs wrapper-moves hit miss miss-label value-reg))
	((null value-reg)
	 (emit-1-nil-dlap
	   (car wrapper-regs) (car wrapper-moves) hit miss miss-label))
	(t
	 (emit-1-t-dlap
	   (car wrapper-regs) (car wrapper-moves) hit miss miss-label value-reg))))



(defun emit-1-nil-dlap (wrapper wrapper-move hit miss miss-label)
  (with-lap-registers ((location index)
		       (primary index)
		       (cache-vector vector))
    (flatten-lap
      wrapper-move
      (opcode :move (operand :cvar 'cache-vector) cache-vector)
      (with-lap-registers ((wrapper-cache-no index))
	(flatten-lap
	  (emit-1-wrapper-compute-primary-cache-location wrapper primary wrapper-cache-no)
	  (opcode :move primary location)
	  (emit-check-1-wrapper-in-cache cache-vector location wrapper hit)	   ;inline hit code
	  (opcode :izerop wrapper-cache-no miss-label)))
      (with-lap-registers ((size index))
	(flatten-lap
	  (opcode :move (operand :cvar 'size) size)
	  (opcode :label 'loop)
	  (opcode :move (operand :i1+ location) location)
	  (opcode :fix= location primary miss-label)
	  (opcode :fix= location size 'set-location-to-min)
	  (opcode :label 'continue)
	  (emit-check-1-wrapper-in-cache cache-vector location wrapper hit) 
	  (opcode :go 'loop)
	  (opcode :label 'set-location-to-min)
	  (opcode :izerop primary miss-label)
	  (opcode :move (operand :constant (index-value->index 0)) location)
	  (opcode :go 'continue)))
      miss)))

;;;
;;; The function below implements CACHE-VECTOR-LOCK-COUNT as the first entry 
;;; in a cache (svref cache-vector 0).  This should probably be abstracted.
;;;
(defun emit-1-t-dlap (wrapper wrapper-move hit miss miss-label value)
  (with-lap-registers ((location index)
		       (primary index)
		       (cache-vector vector)
		       (initial-lock-count t))
    (flatten-lap
      wrapper-move
      (opcode :move (operand :cvar 'cache-vector) cache-vector)
      (with-lap-registers ((wrapper-cache-no index))
	(flatten-lap
	  (emit-1-wrapper-compute-primary-cache-location wrapper primary wrapper-cache-no)
	  (opcode :move primary location)
	  (opcode :move (operand :cref cache-vector 0) initial-lock-count)	   ;get lock-count
	  (emit-check-cache-entry cache-vector location wrapper 'hit-internal)
	  (opcode :izerop wrapper-cache-no miss-label)))    ;check for obsolescence
      (with-lap-registers ((size index))
	(flatten-lap
	  (opcode :move (operand :cvar 'size) size)

	  (opcode :label 'loop)
	  (opcode :move (operand :i1+ location) location)
	  (opcode :move (operand :i1+ location) location)
	  (opcode :label 'continue)
	  (opcode :fix= location primary miss-label)
	  (opcode :fix= location size 'set-location-to-min)
	  (emit-check-cache-entry cache-vector location wrapper 'hit-internal)
	  (opcode :go 'loop)

	  (opcode :label 'set-location-to-min)
	  (opcode :izerop primary miss-label)
	  (opcode :move (operand :constant (index-value->index 2)) location)
	  (opcode :go 'continue)))
      (opcode :label 'hit-internal)
      (opcode :move (operand :i1+ location) location)		   ;position for getting value
      (opcode :move (emit-cache-vector-ref cache-vector location) value)
      (emit-lock-count-test initial-lock-count cache-vector 'hit)
      miss
      (opcode :label 'hit)
      hit)))

(defun emit-greater-than-1-dlap (wrappers wrapper-moves hit miss miss-label value)
  (let ((cache-line-size (compute-line-size (+ (length wrappers) (if value 1 0)))))
    (with-lap-registers ((location index)
			 (primary index)
			 (cache-vector vector)
			 (initial-lock-count t)
			 (next-location index)
			 (line-size index))	;Line size holds a constant
						;that can be folded in if there was
						;a way to add a constant to 
						;an index register
      (flatten-lap
	(apply #'flatten-lap wrapper-moves)
	(opcode :move (operand :constant cache-line-size) line-size)
	(opcode :move (operand :cvar 'cache-vector) cache-vector)
	(emit-n-wrapper-compute-primary-cache-location wrappers primary miss-label)
	(opcode :move primary location)
	(opcode :move location next-location)
	(opcode :move (operand :cref cache-vector 0) initial-lock-count)  ;get the lock-count
	(with-lap-registers ((size index))
	  (flatten-lap
	    (opcode :move (operand :cvar 'size) size)
	    (opcode :label 'continue)
	    (opcode :move (operand :i+ location line-size) next-location)
	    (emit-check-cache-line cache-vector location wrappers 'hit)
	    (emit-adjust-location location next-location primary size 'continue miss-label)
	    (opcode :label 'hit)
	    (and value (opcode :move (emit-cache-vector-ref cache-vector location) value))
	    (emit-lock-count-test initial-lock-count cache-vector 'hit-internal)
	    miss
	    (opcode :label 'hit-internal)
	    hit))))))



;;;
;;; Cache related lap code
;;;

(defun emit-check-1-wrapper-in-cache (cache-vector location wrapper hit-code)
  (let ((exit-emit-check-1-wrapper-in-cache 
	  (make-symbol "exit-emit-check-1-wrapper-in-cache")))
    (with-lap-registers ((cwrapper #-structure-wrapper vector
				   #+structure-wrapper t))
      (flatten-lap
	(opcode :move (emit-cache-vector-ref cache-vector location) cwrapper)
	(opcode :neq cwrapper wrapper exit-emit-check-1-wrapper-in-cache)
	hit-code
	(opcode :label exit-emit-check-1-wrapper-in-cache)))))

(defun emit-check-cache-entry (cache-vector location wrapper hit-label)
  (with-lap-registers ((cwrapper #-structure-wrapper vector
				 #+structure-wrapper t))
    (flatten-lap
      (opcode :move (emit-cache-vector-ref cache-vector location) cwrapper)
      (opcode :eq cwrapper wrapper hit-label))))

(defun emit-check-cache-line (cache-vector location wrappers hit-label)
  (let ((checks
	  (flatten-lap
	    (gathering1 (flattening-lap)
	      (iterate ((wrapper (list-elements wrappers)))
		(with-lap-registers ((cwrapper #-structure-wrapper vector
					       #+structure-wrapper t))
		  (gather1
		    (flatten-lap
		      (opcode :move (emit-cache-vector-ref cache-vector location) cwrapper)
		      (opcode :neq cwrapper wrapper 'exit-emit-check-cache-line)
		      (opcode :move (operand :i1+ location) location)))))))))
    (flatten-lap
      checks
      (opcode :go hit-label)
      (opcode :label 'exit-emit-check-cache-line))))

(defun emit-lock-count-test (initial-lock-count cache-vector hit-label)
  ;;
  ;; jumps to hit-label if cache-vector-lock-count consistent, otherwise, continues
  ;; 
  (with-lap-registers ((new-lock-count t))
    (flatten-lap
      (opcode :move (operand :cref cache-vector 0) new-lock-count) ;get new cache-vector-lock-count
      (opcode :fix= new-lock-count initial-lock-count hit-label))))



(defun emit-adjust-location (location next-location primary size cont-label miss-label)
  (flatten-lap
    (opcode :move next-location location)
    (opcode :fix= location size 'at-end-of-cache)
    (opcode :fix= location primary miss-label)
    (opcode :go cont-label)
    (opcode :label 'at-end-of-cache)
    (opcode :fix= primary (operand :constant (index-value->index 1)) miss-label)
    (opcode :move (operand :constant (index-value->index 1)) location)
    (opcode :go cont-label)))
     

;; From cache.lisp
(defun emit-cache-vector-ref (cache-vector-operand location-operand)
  (operand :iref cache-vector-operand location-operand))

(defun emit-wrapper-ref (wrapper-operand field-operand)
  (operand :iref wrapper-operand field-operand))

(defun emit-wrapper-cache-number-vector (wrapper-operand)
  (operand :wrapper-cache-number-vector wrapper-operand))

(defun emit-cache-number-vector-ref (cnv-operand field-operand)
  (operand :iref cnv-operand field-operand))

(defun emit-1-wrapper-compute-primary-cache-location (wrapper primary wrapper-cache-no)
  (with-lap-registers ((mask index) 
		       #+structure-wrapper (cnv fixnum-vector))
    (let ((field wrapper-cache-no))
      (flatten-lap
        (opcode :move (operand :cvar 'mask) mask)
        (opcode :move (operand :cvar 'field) field)
	#-structure-wrapper
        (opcode :move (emit-wrapper-ref wrapper field) wrapper-cache-no)
	#+structure-wrapper
	(opcode :move (emit-wrapper-cache-number-vector wrapper) cnv)
	#+structure-wrapper
	(opcode :move (emit-cache-number-vector-ref cnv field) wrapper-cache-no)
        (opcode :move (operand :ilogand wrapper-cache-no mask) primary)))))

(defun emit-n-wrapper-compute-primary-cache-location (wrappers primary miss-label)
  (with-lap-registers ((field index)
		       (mask index))
    (let ((add-wrapper-cache-numbers
	   (flatten-lap
	    (gathering1 (flattening-lap)
	       (iterate ((wrapper (list-elements wrappers))
			 (i (interval :from 1)))
		 (gather1
		  (with-lap-registers ((wrapper-cache-no index)
				       #+structure-wrapper (cnv fixnum-vector))
		    (flatten-lap
		     #-structure-wrapper
		     (opcode :move (emit-wrapper-ref wrapper field) wrapper-cache-no)
		     #+structure-wrapper
		     (opcode :move (emit-wrapper-cache-number-vector wrapper) cnv)
		     #+structure-wrapper
		     (opcode :move (emit-cache-number-vector-ref cnv field)
			     wrapper-cache-no)
		     (opcode :izerop wrapper-cache-no miss-label)
		     (opcode :move (operand :i+ primary wrapper-cache-no) primary)
		     (when (zerop (mod i wrapper-cache-number-adds-ok))
		       (opcode :move (operand :ilogand primary mask) primary))))))))))
      (flatten-lap
       (opcode :move (operand :constant 0) primary)
       (opcode :move (operand :cvar 'field) field)
       (opcode :move (operand :cvar 'mask) mask)
       add-wrapper-cache-numbers
       (opcode :move (operand :ilogand primary mask) primary)
       (opcode :move (operand :i1+ primary) primary)))))

(defun emit-fetch-wrapper (metatype argument dest miss-label &optional slot)
  (let ((exit-emit-fetch-wrapper (make-symbol "exit-emit-fetch-wrapper")))
    (with-lap-registers ((arg t))
      (ecase metatype
	((standard-instance #+new-kcl-wrapper structure-instance)
	  (let ((get-std-inst-wrapper (make-symbol "get-std-inst-wrapper"))
		(get-fsc-inst-wrapper (make-symbol "get-fsc-inst-wrapper")))
	    (flatten-lap
	      (opcode :move (operand :arg argument) arg)
	      (opcode :std-instance-p arg get-std-inst-wrapper)	   ;is it a std wrapper?
	      (opcode :fsc-instance-p arg get-fsc-inst-wrapper)	   ;is it a fsc wrapper?
	      (opcode :go miss-label)
	      (opcode :label get-fsc-inst-wrapper)
	      (opcode :move (operand :fsc-wrapper arg) dest)	   ;get fsc wrapper
	      (and slot
		   (opcode :move (operand :fsc-slots arg) slot))
	      (opcode :go exit-emit-fetch-wrapper)
	      (opcode :label get-std-inst-wrapper)
	      (opcode :move (operand :std-wrapper arg) dest)	   ;get std wrapper
	      (and slot
		   (opcode :move (operand :std-slots arg) slot))
	      (opcode :label exit-emit-fetch-wrapper))))
	(class
	  (when slot (error "Can't do a slot reg for this metatype."))
	  (let ((get-std-inst-wrapper (make-symbol "get-std-inst-wrapper"))
		(get-fsc-inst-wrapper (make-symbol "get-fsc-inst-wrapper")))
	    (flatten-lap
	      (opcode :move (operand :arg argument) arg)
	      (opcode :std-instance-p arg get-std-inst-wrapper)
	      (opcode :fsc-instance-p arg get-fsc-inst-wrapper)
	      #-new-kcl-wrapper
	      (opcode :move (operand :built-in-or-structure-wrapper arg) dest)
	      #+new-kcl-wrapper
	      (opcode :move (operand :built-in-wrapper arg) dest)
	      (opcode :go exit-emit-fetch-wrapper)
	      (opcode :label get-fsc-inst-wrapper)
	      (opcode :move (operand :fsc-wrapper arg) dest)
	      (opcode :go exit-emit-fetch-wrapper)
	      (opcode :label get-std-inst-wrapper)
	      (opcode :move (operand :std-wrapper arg) dest)
	      (opcode :label exit-emit-fetch-wrapper))))
	((built-in-instance #-new-kcl-wrapper structure-instance)
	  (when slot (error "Can't do a slot reg for this metatype."))
	  (let ()
	    (flatten-lap
	      (opcode :move (operand :arg argument) arg)
	      (opcode :std-instance-p arg miss-label)
	      (opcode :fsc-instance-p arg miss-label)
	      #-new-kcl-wrapper
	      (opcode :move (operand :built-in-or-structure-wrapper arg) dest)
	      #+new-kcl-wrapper
	      (opcode :move (operand :built-in-wrapper arg) dest))))))))

