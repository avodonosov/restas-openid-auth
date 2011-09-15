;;;; restas-simple-auth.lisp
;;;;
;;;; This file is part of the restas-openid-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Anton Vodonosov <avodonosov@yandex.com>

(defsystem restas-openid-auth
  :depends-on (#:restas #:cl-openid #:closure-template #:alexandria)
  :components ((:module "src"
                        :components ((:file "defmodule")
                                     (:file "openid-auth" :depends-on ("defmodule"))))))
