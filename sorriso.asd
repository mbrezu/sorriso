;;;; sorriso.asd

(asdf:defsystem #:sorriso
  :serial t
  :depends-on (#:alexandria
               #:hunchentoot
               #:cl-who
               #:parenscript
               #:css-lite
               #:drakma
               #:closure-html
               #:mbrezu-utils)
  :components ((:file "package")
               (:file "sorriso")))
