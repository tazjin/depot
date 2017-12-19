#|
  This file is a part of gemma project.
  Copyright (c) 2017 Vincent Ambo
|#

#|
  Author: Vincent Ambo
|#

(defsystem "gemma"
  :version "0.1.0"
  :author "Vincent Ambo"
  :license "GPLv3"
  :depends-on (alexandria local-time hunchentoot yason)
  :components ((:module "src"
                :components
                ((:file "gemma"))))
  :description "Gemma is a household task management system"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "gemma-test"))))
