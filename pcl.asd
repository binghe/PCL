(in-package :cl-user)

(defpackage pcl-system (:use :common-lisp :asdf))
 
(in-package :pcl-system)

(defsystem #:pcl
  :description "Portable Common Loops"
  :author "Xerox Corporation"
  :maintainer "Chun Tian (binghe) <binghe.lisp@gmail.com>"
  :license "MIT"
  :components
  ())
