;;;; sorriso.lisp

(in-package #:sorriso)

(in-package #:sorriso)

(defvar *acceptor* nil)

(defvar *sorriso-page* nil)

(defvar *sorriso-data-time* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *translation-hash*)
  (setf *translation-hash* nil)
  (defun slurp-file-as-bytes (file-name)
    (with-open-file (str (asdf:system-relative-pathname :sorriso file-name)
                         :element-type '(mod 256))
      (let ((bytes (make-array (list (file-length str)) :element-type '(mod 256))))
        (read-sequence bytes str)
        bytes)))
  (defun slurp-file-as-string (file-name)
    (babel:octets-to-string (slurp-file-as-bytes file-name)))
  (defun translate (str)
    (when (not *translation-hash*)
      (setf *translation-hash* (make-hash-table :test #'equal))
      (dolist (pair (-> "lang.txt"
                        slurp-file-as-string
                        (split-sequence:split-sequence #\Newline $ :remove-empty-subseqs t)
                        (group $ 2)))
        (setf (gethash (first pair) *translation-hash*)
              (second pair))))
    (gethash str *translation-hash*))
  (set-dispatch-macro-character
   #\# #\"
   #'(lambda (s c1 c2)
       (declare (ignore c2))
       (unread-char c1 s)
       (translate (read s)))))

(defun get-sorriso-data ()
  (labels ((string-extract (parsed-html)
             (cond ((stringp parsed-html) (list parsed-html))
                   ((atom parsed-html) nil)
                   (t (mapcan #'string-extract (cddr parsed-html)))))
           (group-into-categories (lines)
             (let (result last-category current-items)
               (dolist (line lines)
                 (cond ((alexandria:ends-with-subseq "Lei" line)
                        (push line current-items))
                       ((not (string-equal line last-category))
                        (when last-category
                          (push (cons last-category
                                      current-items)
                                result))
                        (setf current-items nil)
                        (setf last-category line))))
               (when last-category
                 (push (cons last-category
                             current-items)
                       result))
               (reverse result))))
    (when (or (not *sorriso-data-time*)
              (not *sorriso-page*)
              (> (- (get-universal-time) *sorriso-data-time*)
                 3600))
      (setf *sorriso-page* (drakma:http-request "http://www.sorriso.ro"))
      (setf *sorriso-data-time* (get-universal-time)))
    (-> *sorriso-page*
        (chtml:parse $ (chtml:make-lhtml-builder))
        (muh-fp:find-path '((:div :id "gh1") :tr) $)
        (mapcar #'string-extract $)
        (remove-if #'null $)
        (mapcar (lambda (list)
                  (string-trim " " (apply #'concatenate 'string list)))
                $)
        group-into-categories)))

(defun files-to-load ()
  (let (result)
    (cl-fad:walk-directory (asdf:system-relative-pathname :sorriso "static")
                           (lambda (file-path)
                             (let* ((file-name (-> file-path
                                                   namestring
                                                   #+ccl (cl-ppcre:regex-replace-all "\\\\\\." $ ".")))
                                    (static-pos (search "/static/" file-name)))
                               (when static-pos
                                 (push (subseq file-name static-pos)
                                       result)))))
    result))

(defun load-files ()
  (let ((result (make-hash-table :test #'equal)))
    (dolist (file (files-to-load))
      (setf (gethash file result)
            (slurp-file-as-bytes (asdf:system-relative-pathname :sorriso
                                                                (subseq file 1)))))
    result))

(defvar *files-in-memory*)

(setf *files-in-memory* (load-files))

(defun make-app (request-handler)
  (setf tbnl:*dispatch-table* (last tbnl:*dispatch-table*))
  (push (tbnl:create-prefix-dispatcher "/" request-handler)
        tbnl:*dispatch-table*))

(defun start ()
  (when (not *acceptor*)
    (setf *acceptor* (make-instance 'tbnl:easy-acceptor :port 8900))
    (make-app 'handler))
  (tbnl:start *acceptor*))

(defun stop ()
  (tbnl:stop *acceptor*))

(defun get-content-type (path)
  (labels ((ends-with (str)
             (alexandria:ends-with-subseq str path)))
    (cond ((ends-with ".png") "image/png")
          ((ends-with ".js") "text/javascript")
          ((ends-with ".css") "text/css")
          ((ends-with ".jpg") "image/jpeg")
          (t "application/binary"))))

(defun handler ()
  (let ((path (tbnl:script-name tbnl:*request*)))
    (labels ((path-is (str)
               (string-equal path str))
             (path-starts-with (str)
               (alexandria:starts-with-subseq str path)))
      (cond ((path-starts-with "/static/")
             (let ((content (gethash path *files-in-memory*)))
               (cond (content
                      (setf (tbnl:content-type*) (get-content-type path))
                      (let ((stream (tbnl:send-headers)))
                        (write-sequence content stream)))
                     (t ""))))
            ((path-is "/")
             (generate-root-page))
            ((path-is "/style.css")
             (generate-css))
            ((path-is "/code/script.js")
             (generate-script))))))

(defun generate-root-page ()
  (labels ((output-category (category stream)
             (let ((category-name (first category))
                   (category-content (rest category)))
               (with-html-output (stream)
                 (:h2 :class "section-title" (str category-name)))
               (dolist (item category-content)
                 (with-html-output (stream)
                   (:p :class "item" (str item)))))))
    (with-html-output-to-string (hs nil :indent 0)
      (:html
       (:head
        (:title "Sorriso")
        (:link :type "text/css"
               :href "/static/css/ui-lightness/jquery-ui-1.8.17.custom.css"
               :rel "stylesheet")
        (:script :type "text/javascript" :src "/static/js/jquery-1.7.1.min.js")
        (:script :type "text/javascript" :src "/static/js/jquery-ui-1.8.17.custom.min.js")
        (:script :type "text/javascript" :src "/code/script.js")
        (:link :rel "stylesheet" :type "text/css" :media "screen" :href "style.css"))
       (:body
        (:div :id "content"
              (:h1 "Sorriso")
              (:p :class "phones" "Telefon:"
                  (:ul (:li "021-316-53-58")
                       (:li "021-317-65-35")
                       (:li "0724-376-471")))
              (dolist (category (get-sorriso-data))
                (output-category category hs)))
        (:div :id "order"
              (:h2 (str #"current-command"))
              (:div :id "drag-n-drop-here"
                    :class "drag-n-drop-here"
                    (str #"drag-n-drop-here")))
        (:div :id "add-client-form"
              :title (str #"add-client")
              (:p :id "validateTips" (str #"enter-a-name"))
              (:form
               (:label :for "name" "Nume")
               (:input :type "text" :name "name" :id "name")
               (:label :for "notes" "Note")
               (:input :type "text" :name "notes" :id "notes"))))))))

(defun generate-css ()
  (css-lite:css
    (("h2") (:background-color "yellow"))
    (("#content")
     (:-moz-column-count "3"
                         :-moz-column-gap "20px"
                         :-webkit-column-count "3"
                         :-webkit-column-gap "20px"
                         :column-count "3"
                         :column-gap "20px"
                         :width "80%"
                         ))
    ((".item") (:font-size "small"))
    ((".order-item") (:font-size "small"))
    ((".order-item-button") (:font-size "x-small"))
    ((".small-print") (:font-size "x-small"))
    ((".section-title") (:font-size "small"
                                    :font-weight "bold"))
    ((".client-name") (:font-weight "bold"))
    ((".client-order") (:padding "5px"
                                 :margin "5px"
                                 :background-color "#ffb040"
                                 :border-radius "6px"
                                 :-webkit-border-radius "6px"
                                 :-moz-border-radius "6px"))
    ((".drag-n-drop-here") (:padding "5px"
                                     :margin "5px"
                                     :background-color "#ffb040"
                                     :border-radius "6px"
                                     :-webkit-border-radius "6px"
                                     :-moz-border-radius "6px"))
    (("#order") (:background-color "white"
                                   :width "20%"
                                   :height "100%"
                                   :padding-left "10px"
                                   :position "absolute"
                                   :top "0px"
                                   :right "0px"))))

(defmacro defunlist (name &body body)
  `(defun ,name ()
     (list
      ,@body)))

(defunlist script-chunks
  (ps (defun make-button (text action)
        (chain ($ "<span/>")
               (text text)
               (add-class "order-item-button")
               (button)
               (click action))))
  (ps (defun make-close-button (element)
        (let* ((confirm (chain ($ "<span/>")
                               (add-class "confirmation")))
               (prompt (chain ($ "<span/>")
                              (text #"sure?")
                              (add-class "small-print")))
               (yes-button (make-button #"yes" (lambda ()
                                                 (chain element (remove)))))
               (no-button (make-button #"no" (lambda ()
                                               (chain confirm
                                                      (css "display" "none")))))
               (button (make-button "X" (lambda ()
                                          (chain ($ ".confirmation")
                                                 (css "display" "none"))
                                          (chain confirm
                                                 (css "display" "inline"))))))
          (chain confirm
                 (append prompt yes-button no-button)
                 (css "display" "none"))
          (chain ($ "<span/>")
                 (append button confirm)))))
  (ps (defun make-edit-button (element)
        (chain (make-button #"edit" (lambda ()
                                      (chain ($ "#name")
                                             (val (chain element
                                                         (attr "client-name"))))
                                      (chain ($ "#notes")
                                             (val (chain element
                                                         (attr "client-notes"))))
                                      (setf (@ sorriso-data should-edit) true)
                                      (setf (@ sorriso-data element-to-edit) element)
                                      (open-dialog))))))
  (ps (defun make-order-item (order-item)
        (let ((element ($ "<div/>"))
              (text (chain ($ "<span/>")
                           (text order-item))))
          (chain element
                 (add-class "order-item")
                 (append (make-close-button element) text)))))
  (ps (defun make-client-area (client-name client-notes)
        (let ((element ($ "<div/>")))
          (chain element
                 (attr "client-name" client-name)
                 (attr "client-notes" client-notes)
                 (append (make-close-button element)
                         (make-edit-button element)
                         (chain ($ "<span/>")
                                (attr "id" "client-name")
                                (text client-name)
                                (add-class "client-name"))
                         (chain ($ "<p>")
                                (attr "id" "client-notes")
                                (text client-notes)))
                 (add-class "client-order")
                 (droppable (create "drop" (lambda (event ui)
                                             (chain ($ this)
                                                    (append (make-order-item
                                                             (chain ui helper (text))))))
                                    "tolerance" "pointer"))))))
  (ps (defun new-client ()
        (let ((name-element ($ "#name"))
              (notes-element ($ "#notes"))
              (valid true))
          (chain name-element
                 (remove-class "ui-state-error"))
          (when (eql 0 (length (chain name-element (val))))
            (setf valid false)
            (chain name-element
                   (add-class "ui-state-error"))
            (chain ($ "#validateTips")
                   (add-class "ui-state-highlight"))
            (set-timeout (lambda ()
                           (chain ($ "#validateTips")
                                  (remove-class "ui-state-highlight")))
                         1500))
          (when valid
            (when (not (@ sorriso-data should-edit))
              (let ((new-client-area (make-client-area (chain name-element (val))
                                                       (chain notes-element (val)))))
                (chain ($ "#order")
                       (append new-client-area))
                (when (@ sorriso-data dropped-data)
                  (chain new-client-area
                         (append (make-order-item (@ sorriso-data dropped-data))))
                  (chain ($ "#drag-n-drop-here")
                         (text #"same-command-new-client"))
                  (setf (@ sorriso-data dropped-data) nil))))
            (when (@ sorriso-data should-edit)
              (chain (@ sorriso-data element-to-edit)
                     (attr "client-name" (chain name-element (val)))
                     (attr "client-notes" (chain notes-element (val))))
              (chain (@ sorriso-data element-to-edit)
                     (find "#client-name")
                     (text (chain name-element (val))))
              (chain (@ sorriso-data element-to-edit)
                     (find "#client-notes")
                     (text (chain notes-element (val)))))
            (chain ($ "#add-client-form")
                   (dialog "close"))))))
  (ps (defun -sorriso-data ()
        (setf (@ this dialog-is-open) false)
        (setf (@ this should-edit) false)
        (setf (@ this element-to-edit) null)
        (setf (@ this dropped-data) null)))
  (ps (defvar sorriso-data (new -sorriso-data)))
  (ps (defun open-dialog ()
        (setf (@ sorriso-data dialog-is-open) true)
        (chain ($ "#add-client-form")
               (dialog "open"))))
  (ps
    (chain ($ document)
           (ready (lambda ()
                    (chain ($ "#add-client-form")
                           (dialog (create "autoOpen" false
                                           "modal" t
                                           "buttons" (create #"give-up"
                                                             (lambda ()
                                                               (chain ($ this)
                                                                      (dialog "close")))
                                                             #"add" new-client)
                                           "close" (lambda (event ui)
                                                     (setf (@ sorriso-data dialog-is-open) false)))))
                    (chain ($ "#drag-n-drop-here")
                           (droppable (create "drop"
                                              (lambda (event ui)
                                                (chain ($ "#name") (val ""))
                                                (chain ($ "#notes") (val ""))
                                                (setf (@ sorriso-data should-edit) false)
                                                (setf (@ sorriso-data dropped-data)
                                                      (chain ui helper (text)))
                                                (open-dialog))
                                              "tolerance" "pointer")))
                    (chain ($ "#add-client-form")
                           (keydown (lambda (e)
                                      (when (= 13 (@ e key-code))
                                        (when (@ sorriso-data dialog-is-open)
                                          (new-client))
                                        (chain e (prevent-default))))))
                    (chain ($ ".item")
                           (draggable (create "helper" "clone"
                                              "revert" "invalid"
                                              "zIndex" 3000))))))))

(defun generate-script ()
  (let ((*ps-print-pretty* t))
    (apply #'concatenate 'string (script-chunks))))
