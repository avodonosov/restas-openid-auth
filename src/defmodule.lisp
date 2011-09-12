;;;; defmodule.lisp
;;;;
;;;; This file is part of the restas-openid-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Anton Vodonosov <avodonosov@yandex.com>

(restas:define-module #:restas.openid-auth
  (:use #:cl)
  (:export #:cur-user
           #:*finalize-page*))

(in-package #:restas.openid-auth)

(defparameter *finalize-page* #'closure-template.standard:xhtml-strict-frame)

