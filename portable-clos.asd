(in-package :cl-user)

(defpackage pclos-system (:use :common-lisp :asdf))
 
(in-package :pclos-system)

(asdf:defsystem #:portable-clos
  :description "Portable CLOS"
  :author "Chun Tian (binghe) <binghe.lisp@gmail.com>"
  :license "MIT"
  :components
  ((:module "closette"
    :components
    ((:file "package")
     (:file "utils"             :depends-on ("package"))
     (:file "closette"          :depends-on ("package" "utils"))
     (:file "bootstrap"         :depends-on ("closette"))
     (:file "closette-tests"    :depends-on ("closette" "bootstrap"))))))
