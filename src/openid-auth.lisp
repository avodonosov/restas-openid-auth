;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: :restas.openid-auth; Base: 10; indent-tabs-mode: nil; coding: utf-8; -*-

;;;; openid-auth.lisp
;;;;
;;;; This file is part of the restas-openid-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Anton Vodonosov <avodonosov@yandex.com>

(in-package #:restas.openid-auth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module private state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *relying-party*)

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (restas:context-add-variable context
                               '*relying-party*
                               (make-instance 'cl-openid:relying-party
                                              ;; todo: get rid of puri
                                              :root-uri (puri:uri (format nil 
                                                                          "http://~A/openid-rp"
                                                                          *host-port*))
                                              :realm (puri:uri (format nil "http://~A"
                                                                       *host-port*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module web UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun finalize-page (content title)
  (funcall *finalize-page*
           (list :title title :body content)))

(defparameter *resources-dir*
  (merge-pathnames "resources/"
                   (asdf:component-pathname (asdf:find-system '#:restas-openid-auth))))

(defun resource-path (path)
  (merge-pathnames path *resources-dir*))

(defun image-path (img-file)
  (merge-pathnames img-file
                   (resource-path "images/")))

(restas:define-route openid-resources ("openid/:(file)")
  (resource-path file))

(restas:define-route openid-images ("openid/images/:(file)")
  (image-path file))

(restas:define-route openid-login ("openid/login" :method :get)
  (funcall *finalize-page*
           (list :title "OpenID Login"
                 :css (list (restas:genurl 'openid-resources :file "openid.css"))
                 :js (list "http://ajax.googleapis.com/ajax/libs/jquery/1.3.1/jquery.min.js"
                           (restas:genurl 'openid-resources :file "jquery.openid.js"))
                 :body (alexandria:read-file-into-string (merge-pathnames "login-form.html"
                                                                             *resources-dir*)))))

(restas:define-route openid-login/post ("openid/login"
                                 :method :post)
  (let ((openid (hunchentoot:post-parameter "openid_identifier")))
    (hunchentoot:REDIRECT            
     (cl-openid:initiate-authentication *relying-party* 
                                        openid))))

(restas:define-route openid-rp ("openid-rp"
                                :method :get)
  ;; todo: the parameter "done" name and meaning is the part 
  ;; of the API. If we want the rulisp code to be able to 
  ;; switch the auth implementation, then the parameter
  ;; name must be taken to some common auth interface
  (let* ((done (hunchentoot:get-parameter "done"))
         
         ;; hunchentoot GET paremeters have the same 
         ;; representation as open-id message: an alist
         (message (hunchentoot:get-parameters hunchentoot:*request*)) 
         (absolute-reply-uri (puri:merge-uris (hunchentoot:request-uri hunchentoot:*request*) 
                                              (cl-openid:root-uri *relying-party*)))
         (user-id-url))
    (handler-case 
        (setf user-id-url 
              (princ-to-string
                (cl-openid:handle-indirect-response *relying-party* 
                                                    message
                                                    absolute-reply-uri)))
      (cl-openid:openid-assertion-error (e)
        (RETURN-FROM openid-rp (finalize-page (format nil "Error: ~A ~A"
                                                      (cl-openid:code e)
                                                      e)
                                              "Error" )))
      (t (e) (RETURN-FROM openid-rp (finalize-page (format nil "Error: ~A" e) 
                                                   "Error"))))
    (if user-id-url
        (progn         
          (format t "response message: ~% ~{~a~^~% ~}~%" message)
          (format t "(type-of user-id-url): ~A~%" (type-of user-id-url))

          (setf (hunchentoot:session-value 'cur-user)
                user-id-url)

;;          (funcall *on-login* user-id-url)
          
;;          (restas.simple-auth::run-login user-id-url "unknown")

;;          (setf (session-value 'cur-user) 
;;                (ensure-openid-user-registered (princ-to-string user-id-url)))
          (hunchentoot:REDIRECT (or done "/")))
        "Access denied")))

(defun cur-user ()
  (hunchentoot:session-value 'cur-user))

(restas:define-route logout ("logout" :requirement #'cur-user)
  (hunchentoot:delete-session-value 'cur-user)
  (restas:redirect (or (hunchentoot:header-in :referer hunchentoot:*request*)
                       'openid-login)))
