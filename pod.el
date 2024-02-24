(defcustom pod-process-exe ""
  "The executable for the process to be run by pod-process-start."
  :type 'string
  :group 'pod)

(defcustom pod-process-options ""
  "Arguments to be passed to the process executable."
  :type 'string
  :group 'pod)

(defcustom pod-timeout-minutes 5
  "The process will be checked and stopped every pod-timeout-minutes only if
pod-continue-p is not true."
  :type 'integer
  :group 'pod)

(defcustom pod-continue-symbol ()
  "When this returns false, the process will be killed via a timer that
runs every pod-timeout-minutes."
  :type 'symbol
  :group 'pod)

(setq pod-wrapped-function (symbol-function pod-continue-symbol))

(defun pod-continue-p ()
  "Continue process if t but kill process every pod-timeout-minutes otherwise."
  (funcall pod-wrapped-function))

(defun pod-process-running-p ()
  "Returns true if the process is running."
  (interactive)
  (if (process-status "pod-process") ; don't check explicitly for 'run'
      (setq found t)
    (setq found nil)
    )
  found)
  
(defun pod-process-start ()
  "Start process if not already running."
  (interactive)
  (if (pod-process-running-p) 
      (message "[debug] pod-process is already running.") ; don't start more than one davmail process
    ( let ((default-directory "~/"))
      ;; start-process <my-name-for-process> <buffer name> <program name> <program arguments>
      (start-process "pod-process" "*pod-process*" pod-process-exe pod-process-options)
      )
    ))

(defun pod-process-stop ()
  "Stop previously started process if pod-continue-p is not true."
  (interactive)
  ;; 
  (if (pod-continue-p)
      ;; pod-continue-p is still true, not stopping process.
      (message "[debug] pod-continue-p is still true, not stopping process.")
    ;; ELSE stop the process IF it is still running
    (if (pod-process-running-p)
	(kill-process "pod-process"))
    ))

;; I can't find any suitable exit hooks in mu4e so a quick hack is to
;; stop 'davmail' every 5 mins if mu4e is not active
(run-with-timer 0 (* pod-timeout-minutes 60) 'pod-process-stop)

(provide 'pod)
