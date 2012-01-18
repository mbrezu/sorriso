
(ql:quickload :sorriso)

(defun top ()
  (sorriso:start)
  (princ "Press ENTER to exit...")
  (finish-output)
  (read-char))

(ccl:save-application "sorriso.exe" :toplevel-function #'top :purify t :prepend-kernel t)
