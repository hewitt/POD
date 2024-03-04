(setq pod-process-plist '())
(setq pod-timer-plist '(key timer-object))

;; 
(defun pod-get-entry-list (key)
  "Return the list associated with this key in the property list."
  (plist-get pod-process-plist key))

;; define a few functions that identify the list contents in a clearer way
(defun pod-process-name (key)
  "Return the 0th entry from the list associated with this KEY. This
is a unique NAME to identify this process' details in the plist."
  (nth 0 (pod-get-entry-list key)))

(defun pod-process-exe (key)
  "Return the 1st entry from the list associated with this KEY. This
is a string EXE that identifies the executable/path for this
process."  
  (nth 1 (pod-get-entry-list key)))
  
(defun pod-process-args (key)
  "Return the 2nd entry from the list associated with this KEY. This
is the ARGUMENTS string passed to the process."
  (nth 2 (pod-get-entry-list key)))

(defun pod-process-timeout (key)
  "Return the 3rd entry from the list associated with this KEY. This
is the TIMEOUT passed to the process."
  (nth 3 (pod-get-entry-list key)))

(defun pod-process-p (key)
  "Return the 3rd entry from the list associated with this KEY. This
is a predicate function CONTINUE-P, which when false identifies
that the process can be halted via the timer."
  (nth 4 (pod-get-entry-list key)))

(defun pod-process-stop (key)
  "Stop previously started process if pod-continue-p(KEY) is not
true and pod-process-name(KEY) is not nil (e.g. is running)."
  (interactive)
  (if (or (not (process-status (pod-process-name key))) (funcall (pod-process-p key)))
      (message "[debug] pod process not running or pod-process-p is true for this process.")    
    (kill-process (pod-process-name key))
    (cancel-timer (plist-get pod-timer-plist key))
    (plist-put pod-timer-plist key nil)
  ))

(defun pod-process-start (key)
  "Start the process identified by KEY if it is not already running.
Any associated output is directed to the *pod* buffer."
  (interactive)
  (if (process-status (pod-process-name key))
      (message "[debug] pod process already running")
    ( let ((default-directory "~/"))
      (start-process (pod-process-name key) "*pod*" (pod-process-exe key) (pod-process-args key))
      )
    (setq timer-object (run-with-timer 10 (* (pod-process-timeout key) 60) 'pod-process-stop key)) 
    (plist-put pod-timer-plist key timer-object)
    ))

;; example 
;(add-hook 'mu4e-main-mode-hook (lambda () ( pod-process-start 'davmail)))

(provide 'pod)


