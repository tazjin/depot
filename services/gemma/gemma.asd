#|
  This file is part of Gemma.

  Gemma is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Copyright (c) 2017 Vincent Ambo
|#

(require 'sb-posix)

(defsystem "gemma"
  :version "0.1.0"
  :author "Vincent Ambo"
  :license "GPLv3"
  :depends-on (local-time
               hunchentoot
               cl-json
               cl-prevalence)
  :components ((:module "src"
                :components
                ((:file "gemma"))))
  :build-operation program-op
  :build-pathname #.(or (pathname  (sb-posix:getenv "GEMMA_BIN_TARGET"))
                        "gemma")
  :entry-point "gemma::entrypoint"
  :description "Gemma is a household task management system"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "gemma-test"))))
