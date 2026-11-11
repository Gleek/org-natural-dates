;;; org-natural-dates-duckling-test.el --- Duckling integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-natural-dates-duckling)
(defvar org-time-was-given)

(ert-deftest org-natural-dates-duckling-release-platforms ()
  (dolist (case '((darwin "aarch64-apple-darwin" "aarch64-apple-darwin")
                  (darwin "x86_64-apple-darwin" "x86_64-apple-darwin")
                  (gnu/linux "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-gnu")
                  (gnu/linux "x86_64-pc-linux-gnu" "x86_64-unknown-linux-gnu")
                  (windows-nt "x86_64-w64-mingw32" "x86_64-pc-windows-msvc")))
    (let ((system-type (nth 0 case)) (system-configuration (nth 1 case)))
      (should (equal (org-natural-dates-duckling--platform) (nth 2 case)))))
  (let ((system-configuration "riscv64-linux-gnu"))
    (should-error (org-natural-dates-duckling--platform) :type 'user-error)))

(ert-deftest org-natural-dates-duckling-managed-path ()
  (let ((org-natural-dates-duckling-directory "/tmp/custom-cache/")
        (org-natural-dates-duckling-program nil)
        (system-type 'darwin)
        (system-configuration "aarch64-apple-darwin"))
    (should (equal (org-natural-dates-duckling--program)
                   "/tmp/custom-cache/org-natural-dates-duckling-0.1.0-aarch64-apple-darwin"))
    (setq org-natural-dates-duckling-program "/custom/helper")
    (should (equal (org-natural-dates-duckling--program) "/custom/helper"))))

(defun org-natural-dates-duckling-test-install (valid &optional cancel)
  "Exercise a deferred download with VALID checksum, optionally CANCEL it."
  (let* ((directory (make-temp-file "duckling-install-" t))
         (org-natural-dates-duckling-directory directory)
         (org-natural-dates-duckling-program nil)
         (org-natural-dates-duckling--install-process nil)
         (program (org-natural-dates-duckling--program))
         (payload "test helper executable\n")
         (spawn (symbol-function 'make-process))
         process)
    (unwind-protect
        (progn
          (unless valid
            (with-temp-file program (insert "original helper"))
            (set-file-modes program #o700))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (let* ((command (plist-get args :command))
                              (outputs (cl-loop for (arg next) on command
                                                when (equal arg "--output") collect next)))
                         (should (= (length outputs) 2))
                         (with-temp-file (car outputs) (insert payload))
                         (with-temp-file (cadr outputs)
                           (insert (if valid (secure-hash 'sha256 payload) (make-string 64 ?0)) "  helper\n"))
                         ;; Keep the real sentinel; delay only the transport.
                         (apply spawn (plist-put args :command (list "sh" "-c" "sleep 0.1")))))))
            (setq process (org-natural-dates-duckling-install t))
            (should (process-live-p process))
            (when valid (should-not (file-exists-p program)))
            (should (eq process (org-natural-dates-duckling-install t))))
          (when cancel (org-natural-dates-duckling-cancel-install))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (or (process-get process 'installed) (process-get process 'install-error)))
                        (< (float-time) deadline))
              (accept-process-output process 0.1)))
          (if (or cancel (not valid))
              (progn
                (should (process-get process 'install-error))
                (unless valid
                  (should (equal (with-temp-buffer (insert-file-contents program) (buffer-string)) "original helper"))))
            (should (equal (process-get process 'installed) program))
            (should (file-executable-p program))
            (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Unexpected repeat download"))))
              (should (equal (org-natural-dates-duckling-install) program))))
          (should-not (directory-files directory nil "\\`\\.duckling-")))
      (when (process-live-p process) (delete-process process))
      (delete-directory directory t))))

(ert-deftest org-natural-dates-duckling-install-is-asynchronous ()
  (org-natural-dates-duckling-test-install t))

(ert-deftest org-natural-dates-duckling-install-checksum-preserves-existing ()
  (org-natural-dates-duckling-test-install nil))

(ert-deftest org-natural-dates-duckling-install-cancellation-preserves-existing ()
  (org-natural-dates-duckling-test-install nil t))

(defmacro org-natural-dates-duckling-test-with-clock (&rest body)
  "Run BODY with a fixed clock and reference date."
  (declare (indent 0) (debug t))
  `(let* ((now (encode-time 0 0 12 11 11 2026))
          (org-natural-dates-duckling-program
           (or (getenv "ORG_NATURAL_DATES_DUCKLING_PROGRAM")
               org-natural-dates-duckling-program
               (let ((built (expand-file-name "duckling-cli/target/release/org-natural-dates-duckling" default-directory)))
                 (when (file-executable-p built) built))))
          (org-natural-dates-duckling--reference now)
          (org-natural-dates-duckling--cache (make-hash-table :test #'equal)))
     (unless (file-executable-p (org-natural-dates-duckling--program))
       (ert-skip "Build duckling-cli before running integration tests"))
     (cl-letf (((symbol-function 'current-time) (lambda () now))) ,@body)))

(ert-deftest org-natural-dates-duckling-task-extraction ()
  (org-natural-dates-duckling-test-with-clock
    (dolist (case '(("Family dinner on 19th next month at 8pm" "Family dinner" "2026-12-19" "20:00")
                    ("Lunch in two weeks at noon" "Lunch" "2026-11-25" "12:00")
                    ("Call mum tomorrow at eight pm" "Call mum" "2026-11-12" "20:00")
                    ("Plan next week" "Plan" "2026-11-16" nil)
                    ("Dentist at 3pm next Friday" "Dentist" "2026-11-20" "15:00")
                    ("Family dinner on the nineteenth of next month at eight pm" "Family dinner" "2026-12-19" "20:00")
                    ("Résumé review tomorrow at 8pm" "Résumé review" "2026-11-12" "20:00")))
      (let ((result (org-natural-dates-duckling-parse (car case))))
        (should (equal (plist-get result :text) (nth 1 case)))
        (should (equal (plist-get result :date) (nth 2 case)))
        (should (equal (plist-get result :time) (nth 3 case)))))))

(ert-deftest org-natural-dates-duckling-keeps-ordinary-titles ()
  (org-natural-dates-duckling-test-with-clock
    (dolist (text '("Buy milk" "Read chapter 8" "Look at the screenshot" "Check in changes" "Dinner with family"))
      (let ((result (org-natural-dates-duckling-parse text)))
        (should (equal (plist-get result :text) text))
        (should-not (plist-get result :org-timestamp))
        (let ((org-natural-dates-last-result result))
          (should (equal (org-natural-dates-get-timestamp-line) "")))))))

(ert-deftest org-natural-dates-duckling-offsets ()
  (org-natural-dates-duckling-test-with-clock
    (dolist (case '(("+2d" "2026-11-13" nil) ("+2" "2026-11-13" nil)
                    ("+1w" "2026-11-18" nil) ("-2d" "2026-11-09" nil)
                    ("+2h" "2026-11-11" "14:00") ("." "2026-11-11" nil)
                    ("+2d at 8pm" "2026-11-13" "20:00")))
      (let ((result (org-natural-dates-duckling--parse (car case) t)))
        (should (equal (plist-get result :date) (nth 1 case)))
        (should (equal (plist-get result :time) (nth 2 case)))))
    (let ((org-natural-dates-duckling--reference (encode-time 0 0 12 19 12 2026)))
      (should (equal (plist-get (org-natural-dates-duckling--parse "++2d" t) :date) "2026-12-21"))
      (should (equal (plist-get (org-natural-dates-duckling--parse "--1w" t) :date) "2026-12-12"))
      (should (equal (plist-get (org-natural-dates-duckling--parse "+2d" t) :date) "2026-11-13")))))

(ert-deftest org-natural-dates-duckling-rejects-incomplete-and-unsafe-input ()
  (org-natural-dates-duckling-test-with-clock
    (dolist (text '("+2d rubbish" "in 2d" "tomorrow at 8" "at 3 next Friday"
                    "12/11/2026" "February 30, 2027" "tomorrow at 25:00"
                    "tomorrow at quarter past eight in the evening"
                    "every second Tuesday at 10am" "every other Sunday at 6pm"
                    "tomorrow from 10pm to 2am" "after 3pm tomorrow" "in a year"))
      (ert-info ((format "Input: %s" text))
        (should-error (org-natural-dates-duckling--parse text t) :type 'user-error)))))

(ert-deftest org-natural-dates-duckling-recurrences-and-deadlines ()
  (org-natural-dates-duckling-test-with-clock
    (dolist (case '(("Brush teeth every day at 5pm" "Brush teeth" "+1d")
                    ("Weekly review every Friday" "Weekly review" "+1w")
                    ("Pay rent on the 1st of every month" "Pay rent" "+1m")
                    ("Review every 2 weeks" "Review" "+2w")))
      (let ((result (org-natural-dates-duckling-parse (car case))))
        (should (equal (plist-get result :text) (nth 1 case)))
        (should (equal (plist-get result :repeater) (nth 2 case)))))
    (let ((result (org-natural-dates-duckling-parse "Submit report due tomorrow")))
      (should (eq (plist-get result :type) :deadline))
      (should (equal (plist-get result :text) "Submit report")))))

(ert-deftest org-natural-dates-duckling-canonical-timestamps ()
  (org-natural-dates-duckling-test-with-clock
    (should (equal (plist-get (org-natural-dates-duckling--parse "<2026-12-19 Sat 20:00 +1w>" t) :org-timestamp)
                   "<2026-12-19 20:00 +1w>"))
    (should (equal (plist-get (org-natural-dates-duckling--parse "[2026-12-19 Sat 20:00]" t) :date)
                   "2026-12-19"))))

(ert-deftest org-natural-dates-duckling-shared-org-prompt ()
  (org-natural-dates-duckling-test-with-clock
    (unwind-protect
        (let ((org-time-was-given nil))
          (org-natural-dates-duckling-mode 1)
          (should (equal (org-read-date nil nil "tomorrow at 8pm" nil now) "2026-11-12 20:00"))
          (should org-time-was-given)
          (should (equal (org-read-date nil nil "+2d" nil now) "2026-11-13"))
          (should (equal (org-read-date nil nil "next week" nil now) "2026-11-16"))
          (should (equal (org-read-date nil nil "in one week" nil now) "2026-11-18"))
          (should-error (org-read-date nil t "in 2d" nil now) :type 'user-error)
          (should-error (org-read-date nil t "every Friday" nil now) :type 'user-error)
          (should (equal (org-read-date nil nil "" nil now) "2026-11-11")))
      (org-natural-dates-duckling-mode -1))))

(ert-deftest org-natural-dates-duckling-native-timestamp-commands ()
  (org-natural-dates-duckling-test-with-clock
    (unwind-protect
        (progn
          (org-natural-dates-duckling-mode 1)
          (dolist (inactive '(nil t))
            (with-temp-buffer
              (org-mode)
              (let ((org-read-date-popup-calendar nil))
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "tomorrow at 8pm")))
                  (if inactive (org-time-stamp-inactive nil) (org-time-stamp nil))))
              (should (string-match-p (if inactive
                                         (regexp-quote "[2026-11-12 Thu 20:00]")
                                       (regexp-quote "<2026-11-12 Thu 20:00>"))
                                     (buffer-string))))))
      (org-natural-dates-duckling-mode -1))))

(ert-deftest org-natural-dates-duckling-scheduling-commands ()
  (org-natural-dates-duckling-test-with-clock
    (unwind-protect
        (progn
          (org-natural-dates-duckling-mode 1)
          (dolist (command '(org-schedule org-deadline))
            (with-temp-buffer
              (org-mode)
              (insert "* TODO Test\n")
              (goto-char (point-min))
              (let ((org-read-date-popup-calendar nil))
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "tomorrow at 8pm")))
                  (funcall command nil)))
              (should (string-match-p (regexp-quote "<2026-11-12 Thu 20:00>") (buffer-string))))))
      (org-natural-dates-duckling-mode -1))))

(ert-deftest org-natural-dates-duckling-calendar-selection ()
  (org-natural-dates-duckling-test-with-clock
    (unwind-protect
        (let ((org-read-date-popup-calendar nil))
          (org-natural-dates-duckling-mode 1)
          (should (equal (org-read-date t nil "08:00 2026-12-19" nil now) "2026-12-19 08:00"))
          (should (equal (org-read-date t nil "8am" nil now) "2026-11-11 08:00")))
      (org-natural-dates-duckling-mode -1))))

(ert-deftest org-natural-dates-duckling-timegrid-date-reader ()
  (org-natural-dates-duckling-test-with-clock
    (unless (require 'org-timegrid-calendar nil t)
      (ert-skip "org-timegrid is optional"))
    (unwind-protect
        (let ((org-read-date-popup-calendar nil))
          (org-natural-dates-duckling-mode 1)
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "tomorrow at 8pm")))
            (should (equal (org-timegrid-calendar-read-date nil t nil nil "Time" now)
                           "2026-11-12 20:00"))))
      (org-natural-dates-duckling-mode -1))))

(ert-deftest org-natural-dates-duckling-capture-preserves-title ()
  (org-natural-dates-duckling-test-with-clock
    (let ((org-natural-dates-parser-function #'org-natural-dates-duckling-parse))
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Lunch in two weeks at noon\n")
        (goto-char (point-min))
        (org-natural-dates-capture-finalize-hook)
        (should (string-match-p (regexp-quote "* TODO Lunch\nSCHEDULED: <2026-11-25 Wed 12:00>") (buffer-string))))
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Read chapter 8\n")
        (goto-char (point-min))
        (org-natural-dates-capture-finalize-hook)
        (should (equal (buffer-string) "* TODO Read chapter 8\n"))))))

(ert-deftest org-natural-dates-duckling-mode-restores-parser ()
  (let ((org-natural-dates-parser-function #'ignore))
    (org-natural-dates-duckling-mode 1)
    (org-natural-dates-duckling-mode 1)
    (org-natural-dates-duckling-mode -1)
    (should (eq org-natural-dates-parser-function #'ignore))))

(provide 'org-natural-dates-duckling-test)
;;; org-natural-dates-duckling-test.el ends here
