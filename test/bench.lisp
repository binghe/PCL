;;;-*- Mode: Lisp; Syntax: Common-lisp; Package: user -*- 

(in-package :bench :use '(:lisp :pcl))

;;;Here are a few homebrew benchmarks for testing out Lisp performance.
;;; BENCH-THIS-LISP: benchmarks for common lisp.
;;; BENCH-THIS-CLOS: benchmarks for CLOS.
;;; BENCH-FLAVORS:    ditto for Symbolics flavors.
;;; BE SURE TO CHANGE THE PACKAGE DEFINITION TO GET THE CLOS + LISP YOU WANT TO TEST.
;;;
;;;Each benchmark is reported as operations per second.  Without-interrupts is used,
;;;  so the scheduler isn't supposed to get in the way.  Accuracy is generally
;;;  between one and five percent.
;;;
;;;Elapsed time is measured using get-internal-run-time.  Because the accuracy of
;;;  this number is fairly crude, it is important to use a large number of 
;;;  iterations to get an accurate benchmark.  The function median-time may
;;;  complain to you if you didn't pick enough iterations.
;;;
;;;July 1992.  Watch out!  In some cases the instruction being timed will be
;;;  optimized away by a clever compiler.  Beware of benchmarks that are
;;;  nearly as fast as *speed-of-empty-loop*.
;;;
;;;Thanks to Ken Anderson for much of this code.
;;;
;;; jeff morrill
;;; jmorrill@bbn.com

#+Genera
(eval-when (compile load eval)
  (import '(clos-internals::allocate-instance)))

(proclaim '(optimize (speed 3) (safety 1) (space 0) #+lucid (compilation-speed 0)))

;;;*********************************************************************

(defvar *min-time* (/ 500 (float internal-time-units-per-second))
  "At least 2 orders of magnitude larger than our time resolution.")

(defmacro elapsed-time (form)
  "Returns (1) the result of form and (2) the time (seconds) it takes to evaluate form."
  ;; Note that this function is completely portable.
  (let ((start-time (gensym)) (end-time (gensym)))
    `(let ((,start-time (get-internal-run-time)))
	 (values ,form
		 (let ((,end-time (get-internal-run-time)))
		   (/ (abs (- ,end-time ,start-time))
		      ,(float internal-time-units-per-second)))))))

(defmacro without-interruption (&body forms)
  #+genera `(scl:without-interrupts ,@forms)
  #+lucid `(lcl::with-scheduling-inhibited ,@forms)
  #+allegro `(excl:without-interrupts ,@forms)
  #+(and (not genera) (not lucid) (not allegro)) `(progn ,@forms))

(defmacro median-time (form &optional (I 5))
  "Return the median time it takes to evaluate form."
  ;; I: number of samples to take.
  `(without-interruption
     (let ((results nil))
       (dotimes (ignore ,I)
	 (multiple-value-bind (ignore time) (elapsed-time ,form)
	   (declare (ignore ignore))
	   (if (< time *min-time*)
	       (format t "~% Warning.  Evaluating ~S took only ~S seconds.~
                          ~% You should probably use more iterations." ',form time))
	   (push time results)))
       (nth ,(truncate I 2) (sort results #'<)))))

#+debug
(defun test () (median-time (sleep 1.0)))

;;;*********************************************************************

;;;OPERATIONS-PER-SECOND actually does the work of computing a benchmark.  The amount
;;;  of time it takes to execute the form N times is recorded, minus the time it
;;;  takes to execute the empty loop.  OP/S = N/time.  This quantity is recomputed
;;;  five times and the median value is returned.  Variance in the numbers increases
;;;  when memory is being allocated (cons, make-instance, etc).

(defmacro repeat (form N)
  ;; Minimal loop
  (let ((count (gensym)) (result (gensym)))
    `(let ((,count ,N) ,result)
       (loop 
	 ;; If you don't use the setq, the compiler may decide that since the
	 ;; result is ignored, FORM can be "compiled out" of the loop.
	 (setq ,result ,form)
	 (if (zerop (decf ,count)) (return ,result))))))

(defun nempty (N)
  "The empty loop."
  (repeat nil N))

(defun empty-speed (N) (median-time (nempty N)))

(defun compute-empty-iterations (&optional (default 1000000))
  (format t "~%Computing speed of empty loop...")
  (let ((time nil))
    (loop
      (setq time (empty-speed default))
      (if (< time *min-time*) (setq default (* default 10)) (return)))
    (format t "done.")
    default))

(defvar *empty-iterations*)
(defvar *speed-of-empty-loop*)

(eval-when (load eval)
  (setq *empty-iterations* (compute-empty-iterations))
  (setq *speed-of-empty-loop* (/ (empty-speed *empty-iterations*)
				 (float *empty-iterations*))))

(defmacro operations-per-second (form N &optional (I 5))
  "Return the number of times FORM can evaluate in one second."
  `(let ((time (median-time (repeat ,form ,N) ,I)))
     (/ (float ,N) (- time (* *speed-of-empty-loop* N)))))

(defmacro bench (pretty-name name N &optional (stream t))
  `(format ,stream "~%~A: ~30T~S" ,pretty-name (,name ,N)))

;;;****************************************************************************

;;;BENCH-THIS-LISP

(defun Nmult (N)
  (let ((a 2.1))
    (operations-per-second (* a a) N)))

(defun Nadd (N)
  (let ((a 2.1))
    (operations-per-second (+ a a) N))) 

(defun square (x) (* x x))

(defun funcall-1 (N)
  ;; inlined
  (let ((x 2.1))
    (operations-per-second (funcall #'(lambda (a) (* a a)) x) N)))

(defun f1 (n) n)

(defun funcall-2 (N)
  (let ((f #'f1) 
	(x 2.1))
    (operations-per-second (funcall f x) N)))

(defun funcall-3 (N)
  (let ((x 2.1))
    (operations-per-second (f1 x) N)))

(defun funcall-4 (N)
  (let ((x 2.1))
    (operations-per-second (funcall #'square x) N)))

(defun funcall-5 (N)
  (let ((x 2.1)
	(f #'square))
    (let ((g #'(lambda (x) 
		 (operations-per-second (funcall f x) N))))
      (funcall g x))))

(defun Nsetf (N)
  (let ((array (make-array 15)))
    (operations-per-second (setf (aref array 5) t) N)))

(defun Nsymeval (N) (operations-per-second (eval T) N))

(defun Repeatuations (N) (operations-per-second (eval '(* 2.1 2.1)) N))

(defun n-cons (N) (let ((a 1)) (operations-per-second (cons a a) N)))

(defvar *object* t)
(Defun nspecial (N) (operations-per-second (null *object*) N))

(defun nlexical (N) 
  (let ((o t))
    (operations-per-second (null o) N)))

(defun nfree (N) 
  (let ((o t))
    (let ((g #'(lambda ()
		 #+genera (declare (sys:downward-function))
		 (operations-per-second (null o) N))))
      (funcall g))))

(defun nfree2 (N) 
  (let ((o t))
    (let ((g #'(lambda ()
		 (let ((f #'(lambda ()
			      #+genera (declare (sys:downward-function))
			      (operations-per-second (null o) N))))
		   (funcall f)))))
      (funcall g))))

(defun ncompilations (N)
  (let ((lambda-expression
	  '(lambda (bar) (let ((baz t)) (if baz (cons bar nil))))))
    (operations-per-second (compile 'bob lambda-expression) N)))

(defun bench-this-lisp ()
  (let ((N (/ *empty-iterations* 10)))
    (bench "(* 2.1 2.1)" nmult N)
    (bench "(+ 2.1 2.1)" nadd N)
    (bench "funcall & (* 2.1 2.1)" funcall-3 N)
    (bench "special reference" nspecial *empty-iterations*)
    (bench "lexical reference" nlexical *empty-iterations*)
    ;;  (bench "ivar reference" n-ivar-ref N)
    (bench "(setf (aref array 5) t)" nsetf N)
    (bench "(funcall lexical-f x)" funcall-2 N)
    (bench "(f x)" funcall-3 N) 
    ;;  (Bench "(eval t)" nsymeval 10000)
    ;;  (bench "(eval '(* 2.1 2.1))" repeatuations 10000)
    ;;  (bench "(cons 1 2)" n-cons 100000)
    ;;  (bench "compile simple function" ncompilations 50)
    ))

;(bench-this-lisp)

;;;**************************************************************

#+genera
(progn
  
(scl:defflavor bar (a b) ()
  :initable-instance-variables
  :writable-instance-variables)

(scl:defflavor frob (c) (bar)
  :initable-instance-variables
  :writable-instance-variables)

(scl:defmethod (hop bar) ()
  a)

(scl:defmethod (set-hop bar) (n)
  (setq a n))

(scl:defmethod (nohop bar) ()
  5)

(defun n-ivar-ref (N)
  (let ((i (scl:make-instance 'bar :a 0 :b 0)))
    (ivar-ref i N)))

(scl:defmethod (ivar-ref bar) (N)
  (operations-per-second b N))


(defun Ninstances (N) (operations-per-second (flavor:make-instance 'bar) N))

(defun n-svref (N)
  (let ((instance (flavor:make-instance 'bar :a 1)))
    (operations-per-second (scl:symbol-value-in-instance instance 'a) N)))
(defun n-hop (N)
  (let ((instance (flavor:make-instance 'bar :a 1)))
    (operations-per-second (hop instance) n)))
(defun n-gf (N)
  (let ((instance (flavor:make-instance 'bar :a 1)))
    (operations-per-second (nohop instance) n)))
(defun n-set-hop (N)
  (let ((instance (flavor:make-instance 'bar :a 1)))
    (operations-per-second (set-hop instance) n)))
(defun n-type-of (N)
  (let ((instance (flavor:make-instance 'bar)))
    (operations-per-second (flavor::%instance-flavor instance) N)))

(defun n-bar-b (N)
  (let ((instance (flavor:make-instance 'bar :a 0 :b 0)))
    (operations-per-second (bar-b instance) N)))

(defun n-frob-bar-b (N)
  (let ((instance (flavor:make-instance 'frob :a 0 :b 0)))
    (operations-per-second (bar-b instance) N)))

(defun bench-flavors ()
  (bench "flavor:make-instance (2 slots)" ninstances 5000)
  (bench "flavor:symbol-value-in-instance" n-svref 100000)
  (bench "1 method, 1 dispatch" n-gf 100000)
  (bench "slot symbol in method (access)" n-hop 100000)
  (bench "slot symbol in method (modify)" n-hop 100000)
  (bench "slot accessor bar" n-bar-b 100000)
  (bench "slot accessor frob" n-frob-bar-b 100000) 
  (bench "instance-flavor" n-type-of 500000))

) ; end of #+genera

;;;**************************************************************

;;;BENCH-THIS-CLOS
;;; (evolved from Ken Anderson's tests of Symbolics CLOS)

(defmethod strange ((x t)) t)			; default method
(defmethod area ((x number)) 'green)		; builtin class

(defclass point
	  ()
    ((x :initform 0 :accessor x :initarg :x)
     (y :initform 0 :accessor y :initarg :y)))

(defmethod color ((thing point)) 'red)
(defmethod address ((thing point)) 'boston)
(defmethod area ((thing point)) 0)
(defmethod move-to ((p1 point) (p2 point)) 0)

(defmethod x-offset ((thing point))
  (with-slots (x y) thing x))

(defmethod set-x-offset ((thing point) new-x)
  (with-slots (x y) thing (setq x new-x)))

(defclass box
	  (point)
    ((width :initform 10 :accessor width :initarg :width)
     (height :initform 10 :accessor height :initarg :height)))

(defmethod area ((thing box)) 0)
(defmethod move-to ((box box) (point point)) 0)
(defmethod address :around ((thing box)) (call-next-method))	

(defvar p (make-instance 'point))
(defvar b (make-instance 'box))

(defun n-strange (N) (operations-per-second (strange 5) N))
(defun n-accesses (N)
  (let ((instance p))
    (operations-per-second (x instance) N)))
(defun n-color (N)
  (let ((instance p))
    (operations-per-second (color instance) n)))
(defun n-call-next-method (N)
  (let ((instance b))
    (operations-per-second (address instance) n)))
(defun n-area-1 (N)
  (let ((instance p))
    (operations-per-second (area instance) n)))
(defun n-area-2 (N)
  (operations-per-second (area 5) n))
(defun n-move-1 (N)
  (let ((instance p))
    (operations-per-second (move-to instance instance) n)))
(defun n-move-2 (N)
  (let ((x p) (y b))
    (operations-per-second (move-to x y) n)))
(defun n-off (N)
  (let ((instance p))
    (operations-per-second (x-offset instance) n)))
(defun n-setoff (N)
  (let ((instance p))
    (operations-per-second (set-x-offset instance 500) n)))
(defun n-slot-value (N)
  (let ((instance p))
    (operations-per-second (slot-value instance 'x) n)))

(defun n-class-of-1 (N)
  (let ((instance p))
    (operations-per-second (class-of instance) n)))
(defun n-class-of-2 (N)
  (operations-per-second (class-of 5) n))

(defun n-alloc (N)
  (let ((c (find-class 'point)))
    (operations-per-second (allocate-instance c) n)))

(defun n-make (N)
  (operations-per-second (make-instance 'point) n))

(defun n-make-initargs (N)
  ;; Much slower than n-make!
  (operations-per-second (make-instance 'point :x 0 :y 5) n))

(defun n-make-variable-initargs (N)
  ;; Much slower than n-make!
  (let ((x 0)
	(y 5))
    (operations-per-second (make-instance 'point :x x :y y) n)))

(pcl::expanding-make-instance-top-level

(defun n-make1 (N)
  (operations-per-second (make-instance 'point) n))

(defun n-make-initargs1 (N)
  ;; Much slower than n-make!
  (operations-per-second (make-instance 'point :x 0 :y 5) n))

(defun n-make-variable-initargs1 (N)
  ;; Much slower than n-make!
  (let ((x 0)
	(y 5))
    (operations-per-second (make-instance 'point :x x :y y) n)))

)

(pcl::precompile-random-code-segments)

(defun bench-this-clos ()
  (let ((N (/ *empty-iterations* 10)))
    (bench "1 default method" n-strange N)
    (bench "1 dispatch, 1 method" n-color N)
    (bench "1 dispatch, :around + primary" n-call-next-method N)
    (bench "1 dispatch, 3 methods, instance" n-area-1 N)
    (bench "1 dispatch, 3 methods, noninstance" n-area-2 N)
    (bench "2 dispatch, 2 methods" n-move-1 N)
    (bench "slot reader method" n-accesses N)
    (bench "with-slots (1 access)" n-off N)
    (bench "with-slots (1 modify)" n-setoff N)
    (bench "naked slot-value" n-slot-value N)
    (bench "class-of instance" n-class-of-1 N)
    (bench "class-of noninstance" n-class-of-2 N)
    (bench "allocate-instance (2 slots)" n-alloc
	   #+pcl 5000
	   #+allegro 100000
	   #+(and Genera (not pcl)) 100000
	   #+(and Lucid (not pcl)) 10000)
    (bench "make-instance (2 slots)" n-make
	   #+pcl 5000
	   #+allegro 100000
	   #+(and Genera (not pcl)) 100000
	   #+(and Lucid (not pcl)) 10000)
    (bench "make-instance (2 constant initargs)" n-make-initargs
	   #+pcl 1000
	   #+allegro 100000
	   #+(and Genera (not pcl)) 100000
	   #+(and Lucid (not pcl)) 10000)
    (bench "make-instance (2 variable initargs)" n-make-variable-initargs
	   #+pcl 1000
	   #+allegro 100000
	   #+(and Genera (not pcl)) 100000
	   #+(and Lucid (not pcl)) 10000)
    
    (bench "make-instance (2 slots)" n-make1
	   #+pcl 5000
	   #+allegro 100000
	   #+(and Genera (not pcl)) 100000
	   #+(and Lucid (not pcl)) 10000)
    (bench "make-instance (2 constant initargs)" n-make-initargs1
	   #+pcl 1000
	   #+allegro 100000
	   #+(and Genera (not pcl)) 100000
	   #+(and Lucid (not pcl)) 10000)
    (bench "make-instance (2 variable initargs)" n-make-variable-initargs1
	   #+pcl 1000
	   #+allegro 100000
	   #+(and Genera (not pcl)) 100000
	   #+(and Lucid (not pcl)) 10000)) )

;(bench-this-clos)
