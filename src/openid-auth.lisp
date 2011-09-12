;;;; openid-auth.lisp
;;;;
;;;; This file is part of the restas-openid-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Anton Vodonosov <avodonosov@yandex.com>

(in-package #:restas.openid-auth)

(defun finalize-page (content title)
  (funcall *finalize-page*
           (list :title title :body content)))

;; todo: move into defmodule.lisp
(defparameter *my-host-port* "173.230.157.202:8080")

;; todo: move into the initialization
(defparameter *relying-party* 
  (make-instance 'cl-openid:relying-party
                 ;; todo: get rid of puri
                 :root-uri (puri:uri (format nil 
                                             "http://~A/openid-rp"
                                             *my-host-port*))
                 :realm (puri:uri (format nil "http://~A"
                                          *my-host-port*))))

(restas:define-route openid-login ("openid-login" :method :get)
  (finalize-page 
   "<form method=\"POST\">
  <fieldset>
    <legend>OpenID Login</legend>
    <input type=\"text\" 
           name=\"openid_identifier\" 
           value=\"\" 
           style=\"background-image: url('http://openid.net/wp-content/uploads/2007/10/openid_small_logo.png');background-position: 0px 0px;background-repeat: no-repeat;padding-left: 20px;\">

    <input type=\"submit\" name=\"openid_action\" value=\"Login\">
  </fieldset>
</form>"
   "OpenID Login"))

(restas:define-route openid-login/post ("openid-login"
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
              (cl-openid:handle-indirect-response *relying-party* 
                                                  message
                                                  absolute-reply-uri))
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
