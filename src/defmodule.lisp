;;;; defmodule.lisp
;;;;
;;;; This file is part of the restas-openid-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Anton Vodonosov <avodonosov@yandex.com>

(restas:define-module #:restas.openid-auth
  (:use #:cl)
  (:export #:cur-user
           #:*finalize-page*
           #:*host-port*))

(in-package #:restas.openid-auth)

(defparameter *finalize-page* #'closure-template.standard:xhtml-strict-frame)

(defparameter *host-port* "localhost:8080" 
  "The hostname and (optionaly) port parts of the URL where
this site is hosted.")

