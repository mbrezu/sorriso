;;;; sorriso.asd

(asdf:defsystem #:sorriso
  :serial t
  :depends-on (#:alexandria
               #:hunchentoot
               #:cl-who
               #:cl-ppcre
               #:parenscript
               #:css-lite
               #:drakma
               #:closure-html
               #:mbrezu-utils
               #:babel
               #:split-sequence)
  :components ((:file "package")
               (:file "sorriso")))
