;;;-*-Mode:LISP; Package:(PCL Lisp 1000); Base:10; Syntax:Common-lisp -*-
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
;;; The version of low for Kyoto Common Lisp (KCL)
(in-package "SI")
(export '(%structure-name
          %compiled-function-name
          %set-compiled-function-name
	  %instance-ref
	  %set-instance-ref))
(in-package 'pcl)

(shadow 'lisp:dotimes)

(defmacro dotimes ((var form &optional (val nil)) &rest body &environment env)
  (multiple-value-bind (doc decls bod)
      (extract-declarations body env)
    (declare (ignore doc))
    (let ((limit (gensym))
          (label (gensym)))
      `(let ((,limit ,form)
             (,var 0))
         (declare (fixnum ,limit ,var))
         ,@decls
         (block nil
           (tagbody
            ,label
              (when (>= ,var ,limit) (return-from nil ,val))
              ,@bod
              (setq ,var (the fixnum (1+ ,var)))
              (go ,label)))))))

(defun memq (item list) (member item list :test #'eq))
(defun assq (item list) (assoc item list :test #'eq))
(defun posq (item list) (position item list :test #'eq))

(si:define-compiler-macro memq (item list) 
  (let ((var (gensym)))
    (once-only (item)
      `(let ((,var ,list))
         (loop (unless ,var (return nil))
               (when (eq ,item (car ,var))
                 (return ,var))
               (setq ,var (cdr ,var)))))))

(si:define-compiler-macro assq (item list) 
  (let ((var (gensym)))
    (once-only (item)
      `(dolist (,var ,list nil)
         (when (eq ,item (car ,var))
           (return ,var))))))

(si:define-compiler-macro posq (item list) 
  (let ((var (gensym)) (index (gensym)))
    (once-only (item)
      `(let ((,var ,list) (,index 0))
         (declare (fixnum ,index))
         (dolist (,var ,list nil)
           (when (eq ,item ,var)
             (return ,index))
           (incf ,index))))))

(defun printing-random-thing-internal (thing stream)
  (format stream "~X" (si:address thing)))

(defmacro %svref (vector index)
  `(svref (the simple-vector ,vector) (the fixnum ,index)))

(defsetf %svref (vector index) (new-value)
  `(setf (svref (the simple-vector ,vector) (the fixnum ,index))
         ,new-value))


;;;
;;; std-instance-p
;;;
#-akcl
(si:define-compiler-macro std-instance-p (x)
  (once-only (x)
    `(and (si:structurep ,x)
          (eq (si:%structure-name ,x) 'std-instance))))

#+akcl
(progn

#-new-kcl-wrapper
;; declare that std-instance-p may be computed simply, and will not change.
(si::freeze-defstruct 'std-instance)

(si::freeze-defstruct 'method-call)
(si::freeze-defstruct 'fast-method-call)

(defvar *pcl-funcall* 
  `(lambda (loc)
     (compiler::wt-nl
      "{object _funobj = " loc ";"
      "if(Rset&&type_of(_funobj)!=t_symbol)funcall_no_event(_funobj);
       else super_funcall(_funobj);}")))

(setq compiler::*super-funcall* *pcl-funcall*)

(defmacro fmc-funcall (fn pv-cell next-method-call &rest args)
  `(funcall ,fn ,pv-cell ,next-method-call ,@args))

)

;;;
;;; turbo-closure patch.  See the file kcl-mods.text for details.
;;;
#-turbo-closure-env-size
(clines "
object cclosure_env_nthcdr (n,cc)
int n; object cc;
{  object env;
   if(n<0)return Cnil;
   if(type_of(cc)!=t_cclosure)return Cnil;
   env=cc->cc.cc_env;
   while(n-->0)
     {if(type_of(env)!=t_cons)return Cnil;
      env=env->c.c_cdr;}
   return env;
}")

#+turbo-closure-env-size
(clines "
object cclosure_env_nthcdr (n,cc)
int n; object cc;
{  object env,*turbo;
   if(n<0)return Cnil;
   if(type_of(cc)!=t_cclosure)return Cnil;
   if((turbo=cc->cc.cc_turbo)==NULL)
     {env=cc->cc.cc_env;
      while(n-->0)
        {if(type_of(env)!=t_cons)return Cnil;
         env=env->c.c_cdr;}
      return env;}
   else
     {if(n>=fix(*(turbo-1)))return Cnil;
      return turbo[n];}
}")

;; This is the completely safe version.
(defentry cclosure-env-nthcdr (int object) (object cclosure_env_nthcdr))
;; This is the unsafe but fast version.
(defentry %cclosure-env-nthcdr (int object) (object cclosure_env_nthcdr))

;;; #+akcl means this is an AKCL newer than 5/11/89 (structures changed)
(eval-when (compile load eval)

#+new-kcl-wrapper
(progn

(defun instance-ref (slots index)
  (si:structure-ref1 slots index))

(defun set-instance-ref (slots index value)
  (si:structure-set1 slots index value))

(defsetf instance-ref set-instance-ref)
(defsetf %instance-ref %set-instance-ref)
)

(defsetf structure-def set-structure-def)

;;((name args-type result-type side-effect-p new-object-p c-expression) ...)
(defparameter *kcl-function-inlines*
  '((%fboundp (t) compiler::boolean nil nil "(#0)->s.s_gfdef!=OBJNULL")
    (%symbol-function (t) t nil nil "(#0)->s.s_gfdef")
    #-akcl (si:structurep (t) compiler::boolean nil nil "type_of(#0)==t_structure")
    #-akcl (si:%structure-name (t) t nil nil "(#0)->str.str_name")
    #+akcl (si:%structure-name (t) t nil nil "(#0)->str.str_def->str.str_self[0]")
    #+new-kcl-wrapper
    (si:%instance-ref (t t) t nil nil "(#0)->str.str_self[fix(#1)]")
    #+new-kcl-wrapper
    (si:%set-instance-ref (t t t) t t nil "(#0)->str.str_self[fix(#1)]=(#2)")
    (si:%compiled-function-name (t) t nil nil "(#0)->cf.cf_name")
    (si:%set-compiled-function-name (t t) t t nil "((#0)->cf.cf_name)=(#1)")
    (cclosurep (t) compiler::boolean nil nil "type_of(#0)==t_cclosure")
    #+akcl (sfun-p (t) compiler::boolean nil nil "type_of(#0)==t_sfun")
    (%cclosure-env (t) t nil nil "(#0)->cc.cc_env")
    (%set-cclosure-env (t t) t t nil "((#0)->cc.cc_env)=(#1)")
    #+turbo-closure
    (%cclosure-env-nthcdr (fixnum t) t nil nil "(#1)->cc.cc_turbo[#0]")
    
    (logxor (fixnum fixnum) fixnum nil nil "((#0) ^ (#1))")))
  
(defun make-function-inline (inline)
  (setf (get (car inline) 'compiler::inline-always)
        (list (if (fboundp 'compiler::flags)
                  (let ((opt (cdr inline)))
                    (list (first opt) (second opt)
                          (logior (if (fourth opt) 1 0) ; allocates-new-storage
                                  (if (third opt) 2 0)  ; side-effect
                                  (if nil 4 0) ; constantp
                                  (if (eq (car inline) 'logxor)
                                      8 0)) ;result type from args
                          (fifth opt)))
                  (cdr inline)))))

(defmacro define-inlines ()
  `(progn
    ,@(mapcan #'(lambda (inline)
                  (let* ((*package* *the-pcl-package*)
			 (name (intern (format nil "~S inline" (car inline))))
			 (vars (mapcar #'(lambda (type)
					   (declare (ignore type))
					   (gensym))
				       (cadr inline))))
                    `((make-function-inline ',(cons name (cdr inline)))
                      ,@(when (or (every #'(lambda (type) (eq type 't))
                                         (cadr inline))
                                  (char= #\% (aref (symbol-name (car inline)) 0)))
                          `((defun ,(car inline) ,vars
                              ,@(mapcan #'(lambda (var var-type)
                                            (unless (eq var-type 't)
                                              `((declare (type ,var-type ,var)))))
                                        vars (cadr inline))
                              (,name ,@vars))
                            (make-function-inline ',inline))))))
              *kcl-function-inlines*)))

(define-inlines)
)

(defsetf si:%compiled-function-name si:%set-compiled-function-name)
(defsetf %cclosure-env %set-cclosure-env)

(defun set-function-name-1 (fn new-name ignore)
  (declare (ignore ignore))
  (cond ((compiled-function-p fn)
	 (si::turbo-closure fn)
	 ;;(when (symbolp new-name) (proclaim-defgeneric new-name nil))
         (setf (si:%compiled-function-name fn) new-name))
        ((and (listp fn)
              (eq (car fn) 'lambda-block))
         (setf (cadr fn) new-name))
        ((and (listp fn)
              (eq (car fn) 'lambda))
         (setf (car fn) 'lambda-block
               (cdr fn) (cons new-name (cdr fn)))))
  fn)


#+akcl (clines "#define AKCL206") 

(clines "
#ifdef AKCL206
use_fast_links();
#endif

object set_cclosure (result_cc,value_cc,available_size)
  object result_cc,value_cc; int available_size;
{
  object result_env_tail,value_env_tail; int i;
#ifdef AKCL206
  /* If we are currently using fast linking,     */
  /* make sure to remove the link for result_cc. */
  use_fast_links(3,Cnil,result_cc);
#endif
  result_env_tail=result_cc->cc.cc_env;
  value_env_tail=value_cc->cc.cc_env;
  for(i=available_size;
      result_env_tail!=Cnil && i>0;
      result_env_tail=CMPcdr(result_env_tail), value_env_tail=CMPcdr(value_env_tail))
    CMPcar(result_env_tail)=CMPcar(value_env_tail), i--;
  result_cc->cc.cc_self=value_cc->cc.cc_self;
  result_cc->cc.cc_data=value_cc->cc.cc_data;
#ifndef AKCL206
  result_cc->cc.cc_start=value_cc->cc.cc_start;
  result_cc->cc.cc_size=value_cc->cc.cc_size;
#endif
  return result_cc;
}")

(defentry %set-cclosure (object object int) (object set_cclosure))


(defun structure-functions-exist-p ()
  t)

(si:define-compiler-macro structure-instance-p (x)
  (once-only (x)
    `(and (si:structurep ,x)
          (not (eq (si:%structure-name ,x) 'std-instance)))))

(defun structure-type (x)
  (and (si:structurep x)
       (si:%structure-name x)))

(si:define-compiler-macro structure-type (x)
  (once-only (x)
    `(and (si:structurep ,x)
          (si:%structure-name ,x))))

(defun structure-type-p (type)
  (or (not (null (gethash type *structure-table*)))
      (let (#+akcl(s-data nil))
        (and (symbolp type)
             #+akcl (setq s-data (get type 'si::s-data))
             #-akcl (get type 'si::is-a-structure)
             (null #+akcl (si::s-data-type s-data)
                   #-akcl (get type 'si::structure-type))))))

(defun structure-type-included-type-name (type)
  (or (car (gethash type *structure-table*))
      #+akcl (let ((includes (si::s-data-includes (get type 'si::s-data))))
	       (when includes
		 (si::s-data-name includes)))
      #-akcl (get type 'si::structure-include)))

(defun structure-type-internal-slotds (type)
  #+akcl (si::s-data-slot-descriptions (get type 'si::s-data))
  #-akcl (get type 'si::structure-slot-descriptions))

(defun structure-type-slot-description-list (type)
  (or (cdr (gethash type *structure-table*))
      (mapcan #'(lambda (slotd)
		  #-new-kcl-wrapper
                  (when (and slotd (car slotd))
                    (let ((offset (fifth slotd)))
                      (let ((reader #'(lambda (x)
                                        #+akcl (si:structure-ref1 x offset)
                                        #-akcl (si:structure-ref x type offset)))
                            (writer #'(lambda (v x)
                                        (si:structure-set x type offset v))))
                        #+turbo-closure (si:turbo-closure reader)
                        #+turbo-closure (si:turbo-closure writer)
                        (let* ((reader-sym 
				(let ((*package* *the-pcl-package*))
				  (intern (format nil "~s SLOT~D" type offset))))
			       (writer-sym (get-setf-function-name reader-sym))
			       (slot-name (first slotd))
			       (read-only-p (fourth slotd)))
                          (setf (symbol-function reader-sym) reader)
                          (setf (symbol-function writer-sym) writer)
                          (do-standard-defsetf-1 reader-sym)
                          (list (list slot-name
                                      reader-sym
				      reader
                                      (and (not read-only-p) writer)))))))
		  #+new-kcl-wrapper
		  (list slotd))
              (let ((slotds (structure-type-internal-slotds type))
                    (inc (structure-type-included-type-name type)))
                (if inc
                    (nthcdr (length (structure-type-internal-slotds inc))
                            slotds)
                    slotds)))))
            
#+new-kcl-wrapper
(defun si::slot-reader-function (slot)
  (let ((offset (si::slot-offset slot)))
    (si:turbo-closure #'(lambda (x)
			  (si::structure-ref1 x offset)))))				

#+new-kcl-wrapper
(defun si::slot-writer-function (slot)
  (let ((offset (si::slot-offset slot)))
    (si:turbo-closure #'(lambda (x)
			  (si::structure-set1 x offset)))))

(mapcar #'(lambda (fname value)
	    (setf (symbol-function fname) (symbol-function value)))
	'(structure-slotd-name
	  structure-slotd-accessor-symbol
	  structure-slotd-reader-function
	  structure-slotd-writer-function
	  structure-slotd-type
	  structure-slotd-init-form)
	#-new-kcl-wrapper
	'(first second third fourth function-returning-nil function-returning-nil)
	#+new-kcl-wrapper
	'(si::slot-name si::slot-accessor-name 
	  si::slot-reader-function si::slot-writer-function
	  si::slot-type si::slot-default-init))


;; Construct files sys-proclaim.lisp and sys-package.lisp
;; The file sys-package.lisp must be loaded first, since the
;; package sys-proclaim.lisp will refer to symbols and they must
;; be in the right packages.   sys-proclaim.lisp contains function
;; declarations and declarations that certain things are closures.

(defun renew-sys-files()
  ;; packages:
  (compiler::get-packages "sys-package.lisp")
  (with-open-file (st "sys-package.lisp"
		      :direction :output
		      :if-exists :append)
    (format st "(in-package 'SI)
(export '(%structure-name
          %compiled-function-name
          %set-compiled-function-name))
(in-package 'pcl)
"))

  ;; proclaims
  (compiler::make-all-proclaims "*.fn")
  (let ((*package* (find-package 'user)))
  (with-open-file (st "sys-proclaim.lisp"
		      :direction :output
		      :if-exists :append)
    ;;(format st "~%(IN-PACKAGE \"PCL\")~%")
    (print
     `(dolist (v ',
     
	       (sloop::sloop for v in-package "PCL"
			     when (get v 'compiler::proclaimed-closure)
			     collect v))
	(setf (get v 'compiler::proclaimed-closure) t))
     st)
    (format st "~%")
    )))

	
