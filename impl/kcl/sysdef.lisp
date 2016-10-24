;;; -*- Mode: Lisp; Base: 10; Syntax: Common-Lisp; Package: DSYS -*-
;;; File: sysdef.lisp 
;;; Author: Richard Harris

(in-package "DSYS")

(defvar *pcl-compiled-p* nil)
(defvar *pcl-loaded-p* nil)

(unless (boundp 'pcl::*redefined-functions*)
  (setq pcl::*redefined-functions* nil))

(defun reset-pcl-package ()
  (pcl::reset-pcl-package)
  (let ((defsys (subfile '("pcl") :name "defsys")))
    (setq pcl::*pcl-directory* defsys)
    (load-file defsys))
  (mapc #'(lambda (path)
	    (setf (lfi-fwd (get-loaded-file-info path)) 0))
	(pcl-binary-files)))

(defun pcl-binary-files ()
  (pcl::system-binary-files 'pcl::pcl))

(defun maybe-load-defsys (&optional compile-defsys-p)
  (let ((defsys (subfile '("pcl") :name "defsys"))
	(*use-default-pathname-type* nil)
	(*skip-load-if-loaded-p* t)
	(*skip-compile-file-fwd* 0))
    (set 'pcl::*pcl-directory* defsys)
    (when compile-defsys-p
      (compile-file defsys))
    (let ((b-s 'pcl::*boot-state*))
      (when (and (boundp b-s) (symbol-value b-s))
	#+ignore (reset-pcl-package)))
    (load-file defsys)))  

(defun maybe-load-pcl (&optional force-p)
  (unless (and (null force-p)
	       (fboundp 'pcl::system-binary-files)
	       (every #'(lambda (path)
			  (let* ((path-fwd (file-write-date path))
				 (lfi (get-loaded-file-info path)))
			    (and lfi path-fwd (= path-fwd (lfi-fwd lfi)))))
		      (pcl-binary-files)))
    (let ((b-s 'pcl::*boot-state*))
      (when (and (boundp b-s) (symbol-value b-s))
	(reset-pcl-package)))
    (pcl::load-pcl)))

(defsystem pcl
    (:pretty-name "PCL")
  #+akcl
  (:forms 
   :compile (let ((cfn (subfile '("pcl") :name "collectfn" :type "lisp")))
	      (unless (probe-file cfn)
		(run-unix-command 
		 (format nil "ln -s ~A ~A"
			 (namestring (merge-pathnames "../cmpnew/collectfn.lsp" 
						      si::*system-directory*))
			 (namestring cfn))))))
				     
  #+akcl
  "collectfn"
  (:forms 
   :compile
   (progn
     (maybe-load-defsys t)
     (if (and (fboundp 'pcl::operation-transformations)
	      (or (null (probe-file (subfile '("pcl") :name "defsys" :type "lisp")))
		  (every #'(lambda (trans)
			     (eq (car trans) :load))
			 (pcl::operation-transformations 'pcl::pcl :compile))))
	 (maybe-load-pcl)
	 (let ((b-s 'pcl::*boot-state*))
	   (when (and (boundp b-s) (symbol-value b-s))
	     (reset-pcl-package))
	   #+akcl (compiler::emit-fn t)
	   #+akcl (load (merge-pathnames "../lsp/sys-proclaim.lisp" 
					 si::*system-directory*))
	   (#+cmu with-compilation-unit #-cmu progn
	    #+cmu (:optimize 
		   '(optimize (user::debug-info #+(and small (not testing)) .5
			                        #-(and small (not testing)) 2)
		              (speed #+testing 1 #-testing 2)
		              (safety #+testing 3 #-testing 0)
		              #+ignore (user::inhibit-warnings 2))
		   :context-declarations
		   '(#+ignore
		     (:external (declare (user::optimize-interface 
					  (safety 2) (debug-info 1))))))
	     (proclaim #+testing *testing-declaration* 
		       #-testing *fast-declaration*)
	     (pcl::compile-pcl))
	   (reset-pcl-package)
	   (maybe-load-pcl t)))
     #+cmu (purify))
   :load
   (progn 
     (maybe-load-pcl)
     #+cmu (purify))))

(defparameter *pcl-files*
  '((("systems") "lisp"
     "pcl")
    (("pcl") "lisp"
     "sysdef"
     "boot" "braid" "cache" "cloe-low" "cmu-low" "combin" "compat"
     "construct" "coral-low" "cpatch" "cpl" "ctypes" "defclass" "defcombin"
     "defs" "defsys" "dfun" "dlap" "env" "excl-low" "fin" "fixup" "fngen" "fsc"
     "gcl-patches" "genera-low" "gold-low" "hp-low" "ibcl-low" "ibcl-patches"
     "init" "iterate" "kcl-low" "kcl-patches" "lap" "low" "lucid-low" "macros"
     "methods" "pcl-env-internal" "pcl-env" "pkg" "plap" "precom1" "precom2"
     "precom4" "pyr-low" "pyr-patches" "quadlap" "rel-7-2-patches" "rel-8-patches"
     "slots" "std-class" "sys-proclaim" "ti-low" "ti-patches" "vaxl-low" "vector" "walk"
     "xerox-low" "xerox-patches")
    (("pcl") "text"
     "12-7-88-notes" "3-17-88-notes" "3-19-87-notes" "4-21-87-notes"
     "4-29-87-notes" "5-22-87-notes" "5-22-89-notes" "8-28-88-notes"
     "get-pcl" "kcl-mods" "kcl-notes" "lap" "notes" "pcl-env" "readme")))

