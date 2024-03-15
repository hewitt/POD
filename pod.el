(setq pod-process-plist '())
;; when a process is started, we also start a timer to periodically
;; check if this started process should be subsequently stopped
(setq pod-timer-plist '(process-key timer-object))

(defun pod-get-entry (process-key sub-key)
  "The processes are each defined by a plist, and there is a
 plist of processes. Here we return the entry in this plist of
 plists associated with the keys of PROCESS-KEY and SUB-KEY.
 Where SUB-KEY is one of :NAME, :EXE, :ARGS, :MINS or :PRED."
  (plist-get (plist-get pod-process-plist process-key) sub-key))

(defun pod-process-stop (process-key)
  "Stop previously started process if pod-continue-p(PROCESS-KEY) is
not true and pod-process-name(PROCESS-KEY) is not nil (e.g. is
running)."
  (interactive)
  (if (or (not (process-status (pod-get-entry process-key :name)))
	  (funcall (pod-get-entry process-key :pred))
	  )
      (message "[debug] pod process not running or pod-process-p is true for this process.")    
    ;; ELSE stop the process
    (kill-process (pod-get-entry process-key :name))
    ;; kill the timer associated with this process
    (cancel-timer (plist-get pod-timer-plist process-key))
    ;; remove the stored timer object assoiated with this process
    (plist-put pod-timer-plist process-key nil)
  ))

(defun pod-process-start (process-key)
  "Start the process identified by PROCESS-KEY if it is not already
running. Any associated output is directed to the *pod* buffer."
  (interactive)
  (if (process-status (pod-get-entry process-key :name))
      (message "[debug] pod process already running")
    ;; ELSE
    ( let ((default-directory "~/"))
      (setq-local process-id (start-process (pod-get-entry process-key :name) "*pod*" (pod-get-entry process-key :exe) (pod-get-entry process-key :args)))
      ;; for this process, don't ask user to kill y/n on exiting
      ;; emacs before the process has been stopped by pod-process-stop
      (set-process-query-on-exit-flag process-id nil)
      )
    (setq timer-object (run-with-timer 10 (* (pod-get-entry process-key :mins) 60) 'pod-process-stop process-key)) 
    (plist-put pod-timer-plist process-key timer-object)
    ))

;; example 
;(add-hook 'mu4e-main-mode-hook (lambda () ( pod-process-start 'davmail)))

(provide 'pod)


