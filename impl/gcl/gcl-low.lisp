(in-package "SI")
(export '(%structure-name
          %compiled-function-name
          %set-compiled-function-name))
(in-package 'pcl)
(eval-when (compile eval load)
(setq  *EVAL-WHEN-COMPILE* t)
)

(defmacro memq (item list) `(member ,item ,list :test #'eq))
(defmacro assq (item list) `(assoc ,item ,list :test #'eq))
(defmacro posq (item list) `(position ,item ,list :test #'eq))

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

(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (si:address thing)))

(eval-when (compile load eval)
(pushnew :turbo-closure *features*)
(pushnew :turbo-closure-env-size *features*))
)

(defmacro %svref (vector index)
  `(svref (the simple-vector ,vector) (the fixnum ,index)))

(defsetf %svref (vector index) (new-value)
  `(setf (svref (the simple-vector ,vector) (the fixnum ,index))
         ,new-value))

(si::freeze-defstruct 'pcl::std-instance)

(si::freeze-defstruct 'method-call)
(si::freeze-defstruct 'fast-method-call)

(defvar *pcl-funcall*  `(lambda (loc)
          (compiler::wt-nl
           "{object _funobj = " loc ";"
           "if(type_of(_funobj)==t_cclosure && (_funobj->cc.cc_turbo))
                   (*(_funobj->cc.cc_self))(_funobj->cc.cc_turbo);
               else if (type_of(_funobj)==t_cfun) (*(_funobj->cc.cc_self))();
               else super_funcall_no_event(_funobj);}")))

(setq compiler::*super-funcall* *pcl-funcall*)

(defmacro fmc-funcall (fn pv-cell next-method-call &rest args)
  `(funcall ,fn ,pv-cell ,next-method-call ,@args))

(defun pcl::proclaim-defmethod (x y) y
  (and (symbolp x)
       (setf (get x 'compiler::proclaimed-closure ) t)))



;#+turbo-closure-env-size
(clines "
static
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

(defentry cclosure-env-nthcdr (int object) (object cclosure_env_nthcdr))
;; This is the unsafe but fast version.
(defentry %cclosure-env-nthcdr (int object) (object cclosure_env_nthcdr))

(eval-when (compile eval load)
(defparameter *gcl-function-inlines*
  '( (%fboundp (t) compiler::boolean nil nil "(#0)->s.s_gfdef!=OBJNULL")
     (%symbol-function (t) t nil nil "(#0)->s.s_gfdef")
     (si:%structure-name (t) t nil nil "(#0)->str.str_def->str.str_self[0]")
    (si:%compiled-function-name (t) t nil nil "(#0)->cf.cf_name")
    (si:%set-compiled-function-name (t t) t t nil "((#0)->cf.cf_name)=(#1)")
    (cclosurep (t) compiler::boolean nil nil "type_of(#0)==t_cclosure")
    (sfun-p (t) compiler::boolean nil nil "type_of(#0)==t_sfun")
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
)


(defmacro define-inlines ()
  `(progn
    ,@(mapcan #'(lambda (inline)
                  (let ((name (intern (format nil "~S inline" (car inline))))
                        (vars (mapcar #'(lambda (type)
                                          (declare (ignore type))
                                          (gensym))
                                      (cadr inline))))
                    `((eval-when (compile eval load)
				 (make-function-inline
				  ',(cons name (cdr inline))))
                      ,@(when (or (every #'(lambda (type) (eq type 't))
                                         (cadr inline))
                                  (char= #\% (aref (symbol-name (car inline)) 0)))
                          `((defun ,(car inline) ,vars
                              ,@(mapcan #'(lambda (var var-type)
                                            (unless (eq var-type 't)
                                              `((declare (type ,var-type ,var)))))
                                        vars (cadr inline))
                              (the ,(caddr inline) (,name ,@vars)))
                            (make-function-inline ',inline))))))
              *gcl-function-inlines*)))

(define-inlines)

(defsetf si:%compiled-function-name si:%set-compiled-function-name)
(defsetf %cclosure-env %set-cclosure-env)

(defun set-function-name-1 (fn new-name ignore)
  (declare (ignore ignore))
  (cond ((compiled-function-p fn)
	 (si::turbo-closure fn)
	 (when (symbolp new-name) (pcl::proclaim-defmethod new-name nil))
         (setf (si:%compiled-function-name fn) new-name))
        ((and (listp fn)
              (eq (car fn) 'lambda-block))
         (setf (cadr fn) new-name))
        ((and (listp fn)
              (eq (car fn) 'lambda))
         (setf (car fn) 'lambda-block
               (cdr fn) (cons new-name (cdr fn)))))
  fn)


(clines "



object fSuse_fast_links();
static
object set_cclosure (result_cc,value_cc,available_size)
  object result_cc,value_cc; int available_size;
{
  object result_env_tail,value_env_tail; int i;

  /* If we are currently using fast linking,     */
  /* make sure to remove the link for result_cc. */
  (VFUN_NARGS=2,fSuse_fast_links(sLnil,result_cc));

/*  use_fast_links(3,Cnil,result_cc); */

  result_env_tail=result_cc->cc.cc_env;
  value_env_tail=value_cc->cc.cc_env;
  for(i=available_size;
      result_env_tail!=Cnil && i>0;
      result_env_tail=CMPcdr(result_env_tail), value_env_tail=CMPcdr(value_env_tail))
    CMPcar(result_env_tail)=CMPcar(value_env_tail), i--;
  result_cc->cc.cc_self=value_cc->cc.cc_self;
  result_cc->cc.cc_data=value_cc->cc.cc_data;


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
              (setq s-data (get type 'si::s-data))
             
             (null  (si::s-data-type s-data)
                   )))))


(defun structure-type-included-type-name (type)
  (or (car (gethash type *structure-table*))
      (let ((includes (si::s-data-includes (get type 'si::s-data))))
	(when includes
	  (si::s-data-name includes)))))

(defun structure-type-internal-slotds (type)
   (si::s-data-slot-descriptions (get type 'si::s-data))
  )

(defun structure-type-slot-description-list (type)
  (or (cdr (gethash type *structure-table*))
      (mapcan #'(lambda (slotd)
                  (when (and slotd (car slotd))
                    (let ((offset (fifth slotd)))
                      (let ((reader #'(lambda (x)
                                         (si:structure-ref1 x offset)
                                         ))
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
                                      (and (not read-only-p) writer))))))))
              (let ((slotds (structure-type-internal-slotds type))
                    (inc (structure-type-included-type-name type)))
                (if inc
                    (nthcdr (length (structure-type-internal-slotds inc))
                            slotds)
                    slotds)))))

(defun structure-slotd-name (slotd)
  (first slotd))

(defun structure-slotd-accessor-symbol (slotd)
  (second slotd))

;(defun structure-slotd-writer-function (slotd)
;  (third slotd))

(defun structure-slotd-reader-function (slotd)
  (third slotd))

(defun structure-slotd-writer-function (slotd)
  (fourth slotd))

(defun renew-sys-files()
  ;; packages:
  (compiler::get-packages "sys-package.lisp")
  (with-open-file (st "sys-package.lisp"
			  :direction :output
			  :if-exists :append)
	(format st "(lisp::in-package \"SI\")
(export '(%structure-name
          %compiled-function-name
          %set-compiled-function-name))
(in-package \"PCL\")
"))

  ;; proclaims
  (compiler::make-all-proclaims "*.fn")
  (with-open-file (st "sys-proclaim.lisp"
		      :direction :output
		      :if-exists :append)
    (format st "~%(IN-PACKAGE \"PCL\")~%")
    (print
     `(dolist (v ',
     
	       (sloop::sloop for v in-package "PCL"
			     when (get v 'compiler::proclaimed-closure)
			     collect v))
	(setf (get v 'compiler::proclaimed-closure) t))
     st)
    (format st "~%")
))

	
		 
