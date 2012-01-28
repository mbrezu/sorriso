;;;; sorriso.lisp

(in-package #:sorriso)

(in-package #:sorriso)

(defvar *acceptor* nil)

(defvar *sorriso-page* nil)

(defvar *sorriso-data-time* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *translation-hash* nil)
  (defun slurp-file-as-string (file-name)
    (with-open-file (str (asdf:system-relative-pathname :sorriso file-name)
                         :element-type '(mod 256))
      (let ((bytes (make-array (list (file-length str)) :element-type '(mod 256))))
        (read-sequence bytes str)
        (babel:octets-to-string bytes))))
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

(defun make-app (request-handler)
  (setf tbnl:*dispatch-table* (last tbnl:*dispatch-table*))
  (push (tbnl:create-prefix-dispatcher "/" request-handler)
        tbnl:*dispatch-table*)
  (push (tbnl:create-folder-dispatcher-and-handler
         "/static/"
         (asdf:system-relative-pathname :sorriso "static/"))
        tbnl:*dispatch-table*))

(defun start ()
  (when (not *acceptor*)
    (setf *acceptor* (make-instance 'tbnl:easy-acceptor :port 8900))
    (make-app 'handler))
  (tbnl:start *acceptor*))

(defun stop ()
  (tbnl:stop *acceptor*))

(defun handler ()
  (labels ((path-is (str)
             (string-equal (tbnl:script-name tbnl:*request*)
                           str)))
    (cond ((path-is "/")
           (generate-root-page))
          ((path-is "/style.css")
           (generate-css))
          ((path-is "/code/script.js")
           (generate-script)))))

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
              (:div :id "add-client" (str #"add-client")))
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
    ((".order-item-button") (:font-size "6px !important"))
    ((".section-title") (:font-size "small"
                                    :font-weight "bold"))
    ((".client-name") (:font-weight "bold"))
    ((".client-order") (:padding "5px"
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

(defun generate-script ()
  (let ((*ps-print-pretty* t))
    (ps
      (defun make-close-button (element)
        (chain ($ "<span/>")
               (attr "id" "close-button")
               (text "X")
               (add-class "order-item-button")
               (button)
               (click (lambda ()
                        ((@ element remove))))))
      (defun make-edit-button (element)
        (chain ($ "<span/>")
               (attr "id" "edit-button")
               (text "Modifica")
               (add-class "order-item-button")
               (button)
               (click (lambda ()
                        (chain ($ "#name")
                               (val (chain element
                                           (attr "client-name"))))
                        (chain ($ "#notes")
                               (val (chain element
                                           (attr "client-notes"))))
                        (setf should-edit true)
                        (setf element-to-edit element)
                        (open-dialog)))))
      (defun make-order-item (ui)
        (let ((element ($ "<div/>"))
              (text ($ "<span/>")))
          ((@ element add-class) "order-item")
          ((@ text text) ((@ ui helper text)))
          ((@ element append) (make-close-button element) text)
          element))
      (defun make-client-area (client-name client-notes)
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
                                             ((@ ($ this) append)
                                              (make-order-item ui)))
                                    "tolerance" "pointer")))))
      (defun new-client ()
        (let ((name-element ($ "#name"))
              (notes-element ($ "#notes"))
              (valid true))
          ((@ name-element remove-class) "ui-state-error")
          (when (eql 0 (length ((@ name-element val))))
            (setf valid false)
            ((@ name-element add-class) "ui-state-error")
            ((@ ($ "#validateTips") add-class) "ui-state-highlight")
            (set-timeout (lambda ()
                           ((@ ($ "#validateTips") remove-class) "ui-state-highlight"))
                         1500))
          (when valid
            (when (not should-edit)
              ((@ ($ "#order") append)
               (make-client-area ((@ name-element val))
                                 ((@ notes-element val)))))
            (when  should-edit
              (chain element-to-edit
                     (attr "client-name" ((@ name-element val)))
                     (attr "client-notes" ((@ notes-element val))))
              (chain element-to-edit
                     (find "#client-name")
                     (text ((@ name-element val))))
              (chain element-to-edit
                     (find "#client-notes")
                     (text ((@ notes-element val)))))
            ((@ ($ "#add-client-form") dialog) "close"))))
      (defvar dialog-is-open false)
      (defvar should-edit false)
      (defvar element-to-edit null)
      (defun open-dialog ()
        (setf dialog-is-open true)
        ((@ ($ "#add-client-form") dialog) "open"))
      ((@ ($ document) ready)
       (lambda ()
         ((@ ($ "#add-client-form") dialog)
          (create "autoOpen" false
                  "modal" t
                  "buttons" (create #"give-up"
                                    (lambda ()
                                      ((@ ($ this) dialog) "close"))
                                    #"add" new-client)
                  "close" (lambda (event ui)
                            (setf dialog-is-open false))))
         (chain ((@ ($ "#add-client") button))
                (click (lambda ()
                         ((@ ($ "#name") val) "")
                         ((@ ($ "#notes") val) "")
                         (setf should-edit false)
                         (open-dialog))))
         ((@ ($ "#add-client-form") keydown)
          (lambda (e)
            (when (= 13 (@ e key-code))
              (when dialog-is-open
                (new-client))
              ((@ e prevent-default)))))
         ((@ ($ ".item") draggable)
          (create "helper" "clone"
                  "revert" "invalid"
                  "zIndex" 3000)))))))
