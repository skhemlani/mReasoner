;;; -*- lisp -*-
;;; Copyright (c) 2006-2012 Henrik Hjelte
;;; Copyright (c) 2008 Hans Hübner (code from the program YASON)
;;; All rights reserved.
;;; See the file LICENSE for terms of use and distribution.

(in-package #:cl-user)

#+:lispworks (setf (logical-pathname-translations "JSOWN-MASTER")
               (list (list "**;*.*" 
                           (concatenate 'string
                             (format nil "~A" (make-pathname
                                               :host 
                                               (pathname-host *load-truename*)
                                               :directory 
                                               (pathname-directory 
                                                *load-truename*))) 
                             "**/*.*"))))

(load (file-path "packages.lisp"))
(load (file-path "accessors.lisp"))
(load (file-path "reader.lisp"))
(load (file-path "writer.lisp"))

(pushnew :yason *features*)
