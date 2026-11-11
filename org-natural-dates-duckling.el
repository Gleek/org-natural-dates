;;; org-natural-dates-duckling.el --- Local Duckling backend -*- lexical-binding: t; -*-

;; Author: Umar Ahmad
;; Package-Requires: ((emacs "28.1") (org "9.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Install the helper with org-natural-dates-duckling-install.  Enable the mode
;; to interpret text in Org's existing calendar prompt, including callers
;; such as org-timegrid.  The calendar and timestamp insertion stay in Org;
;; unsupported text never falls back to Org's permissive date parser.

;;; Code:

(require 'org-natural-dates)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defgroup org-natural-dates-duckling nil
  "Local natural language dates for Org."
  :group 'org)

(defcustom org-natural-dates-duckling-directory
  (expand-file-name "org-natural-dates/" user-emacs-directory)
  "Directory for downloaded Duckling executables."
  :type 'directory)

(defcustom org-natural-dates-duckling-program nil
  "Optional path to a separately installed Duckling executable.
Nil uses the versioned binary in `org-natural-dates-duckling-directory'."
  :type '(choice (const nil) file))

(defconst org-natural-dates-duckling--version "0.1.0")

(defun org-natural-dates-duckling--platform ()
  "Return the release target for this Emacs, or signal an error."
  (let ((arch (cond ((string-match-p "\\`\\(?:aarch64\\|arm64\\)" system-configuration) "aarch64")
                    ((string-match-p "\\`\\(?:x86_64\\|amd64\\)" system-configuration) "x86_64")
                    (t (user-error "No Duckling download for %s" system-configuration)))))
    (concat arch
            (pcase system-type
              ('darwin "-apple-darwin")
              ('gnu/linux "-unknown-linux-gnu")
              ('windows-nt (if (equal arch "x86_64") "-pc-windows-msvc"
                             (user-error "No Windows ARM Duckling download")))
              (_ (user-error "No Duckling download for %s" system-type))))))

(defun org-natural-dates-duckling--asset ()
  "Return the platform-specific, versioned release filename."
  (concat "org-natural-dates-duckling-" org-natural-dates-duckling--version
          "-" (org-natural-dates-duckling--platform)
          (when (eq system-type 'windows-nt) ".exe")))

(defun org-natural-dates-duckling--program ()
  "Return the custom executable or its managed installation path."
  (or org-natural-dates-duckling-program
      (expand-file-name (org-natural-dates-duckling--asset)
                        org-natural-dates-duckling-directory)))

(defvar org-natural-dates-duckling--install-process nil)

(defun org-natural-dates-duckling--verify-install (program binary checksum)
  "Verify BINARY against CHECKSUM and install it at PROGRAM."
  (let ((expected (with-temp-buffer
                    (insert-file-contents checksum)
                    (unless (looking-at (rx (group (= 64 xdigit)) (or space eos)))
                      (error "Invalid Duckling checksum file"))
                    (downcase (match-string 1))))
        (actual (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally binary)
                  (secure-hash 'sha256 (current-buffer)))))
    (unless (equal expected actual) (error "Duckling checksum mismatch")))
  (set-file-modes binary #o700)
  (org-natural-dates-duckling-stop)
  (rename-file binary program t))

;;;###autoload
(defun org-natural-dates-duckling-install (&optional force)
  "Download the helper asynchronously, returning its process.
An existing executable is reused unless FORCE is non-nil (prefix argument).
Progress and errors appear in *org-natural-dates-install*.  A failed or
cancelled download leaves the installed executable untouched.  Requires curl."
  (interactive "P")
  (cond
   ((process-live-p org-natural-dates-duckling--install-process)
    (message "Duckling installation is already running")
    org-natural-dates-duckling--install-process)
   ((and (not force) (file-executable-p (org-natural-dates-duckling--program)))
    (org-natural-dates-duckling--program))
   (t
    (let* ((curl (or (executable-find "curl") (user-error "Install curl to download Duckling")))
           (program (org-natural-dates-duckling--program))
           (directory (file-name-directory (expand-file-name program)))
           (url (concat "https://github.com/Gleek/org-natural-dates/releases/download/duckling-v"
                        org-natural-dates-duckling--version "/" (org-natural-dates-duckling--asset)))
           (buffer (get-buffer-create "*org-natural-dates-install*"))
           (options '("--ipv4" "--fail" "--location" "--progress-bar"
                      "--connect-timeout" "10" "--max-time" "120")))
      (make-directory directory t)
      (let ((binary (make-temp-file (expand-file-name ".duckling-" directory)))
            (checksum (make-temp-file (expand-file-name ".duckling-sha256-" directory))))
        (with-current-buffer buffer (erase-buffer))
        (condition-case error
            (setq org-natural-dates-duckling--install-process
                  (make-process
                   :name "org-natural-dates-download" :buffer buffer :noquery t
                   :connection-type 'pipe
                   :command (append (list curl "--fail-early") options
                                    (list "--output" binary url "--next") options
                                    (list "--output" checksum (concat url ".sha256")))
                   :sentinel
                   (lambda (process _event)
                     (when (memq (process-status process) '(exit signal))
                       (unwind-protect
                           (condition-case error
                               (progn
                                 (unless (and (eq (process-status process) 'exit)
                                              (zerop (process-exit-status process)))
                                   (error "Duckling download failed/cancelled; see %s" (buffer-name buffer)))
                                 (org-natural-dates-duckling--verify-install program binary checksum)
                                 (process-put process 'installed program)
                                 (message "Duckling installed: %s" program))
                             (error
                              (process-put process 'install-error (error-message-string error))
                              (message "%s" (error-message-string error))))
                         (when (file-exists-p binary) (delete-file binary))
                         (when (file-exists-p checksum) (delete-file checksum)))))))
          (error
           (delete-file binary)
           (delete-file checksum)
           (signal (car error) (cdr error)))))
      (when (called-interactively-p 'interactive) (display-buffer buffer))
      (message "Downloading Duckling in the background")
      org-natural-dates-duckling--install-process))))

(defun org-natural-dates-duckling-cancel-install ()
  "Cancel the background helper download."
  (interactive)
  (when (process-live-p org-natural-dates-duckling--install-process)
    (delete-process org-natural-dates-duckling--install-process)))

(defvar org-natural-dates-duckling--process nil)
(defvar org-natural-dates-duckling--output "")
(defvar org-natural-dates-duckling--reading nil)
(defvar org-natural-dates-duckling--reference nil)
(defvar org-natural-dates-duckling--cache nil)
(defvar org-natural-dates-duckling--saved-parser nil)

(declare-function org-read-date-analyze "org" (ans def defdecode))
(defvar org-def)
(defvar org-defdecode)
(defvar org-ans0)
(defvar org-ans1)
(defvar org-ans2)
(defvar org-read-date-overlay)
(defvar org-time-was-given)
(defvar org-end-time-was-given)

(defun org-natural-dates-duckling-stop ()
  "Stop the local parser process."
  (interactive)
  (when (process-live-p org-natural-dates-duckling--process)
    (delete-process org-natural-dates-duckling--process))
  (setq org-natural-dates-duckling--process nil))

(defun org-natural-dates-duckling--request (text reference)
  "Parse TEXT relative to REFERENCE using the local executable."
  (let* ((request (json-encode
                   `((text . ,text)
                     (reference . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z" reference)))))
         (cached (and org-natural-dates-duckling--cache
                      (gethash request org-natural-dates-duckling--cache))))
    (or cached
        (progn
          (unless (process-live-p org-natural-dates-duckling--process)
            (unless (file-executable-p (org-natural-dates-duckling--program))
              (user-error "Install the helper with M-x org-natural-dates-duckling-install"))
            (setq org-natural-dates-duckling--process
                  (make-process
                   :name "org-natural-dates-duckling" :noquery t
                   :command (list (org-natural-dates-duckling--program))
                   :connection-type 'pipe :coding 'utf-8-unix
                   :filter (lambda (_process output)
                             (setq org-natural-dates-duckling--output
                                   (concat org-natural-dates-duckling--output output))))))
          (setq org-natural-dates-duckling--output "")
          (process-send-string org-natural-dates-duckling--process (concat request "\n"))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (string-match-p "\n" org-natural-dates-duckling--output))
                        (process-live-p org-natural-dates-duckling--process)
                        (< (float-time) deadline))
              (accept-process-output org-natural-dates-duckling--process 0.05)))
          (unless (string-match-p "\n" org-natural-dates-duckling--output)
            (org-natural-dates-duckling-stop)
            (user-error "Duckling did not return a complete response"))
          (let ((result (json-parse-string org-natural-dates-duckling--output
                                           :object-type 'alist :array-type 'list)))
            (when-let ((error (alist-get 'error result)))
              (user-error "Duckling: %s" error))
            (when org-natural-dates-duckling--cache
              (puthash request result org-natural-dates-duckling--cache))
            result)))))

(defconst org-natural-dates-duckling--offset-regexp
  (rx bos (group (or "++" "--" "+" "-")) (group (+ digit))
      (group (? (any "hdwmy"))) (? (+ space) (group (* any))) eos))

(defun org-natural-dates-duckling--normalize (input reference)
  "Translate explicit shorthand in INPUT, returning (TEXT . REFERENCE).
Single signs use today; double signs use the supplied REFERENCE."
  (let ((text (string-trim input))
        (case-fold-search t))
    (when (string-match (rx bos (any "<[") (group (* any)) (any ">]") eos) text)
      (setq text (match-string 1 text)))
    ;; Org passes canonical timestamps back through org-read-date internally.
    (when (string-match (rx bos (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)
                            (+ space) (group (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                            (or space eos)) text)
      (setq text (concat (substring text 0 (match-beginning 1))
                         (substring text (match-end 1)))))
    (cond
     ((string= text ".") (setq text "today" reference (current-time)))
     ((string-match org-natural-dates-duckling--offset-regexp text)
      (let* ((sign (match-string 1 text))
             (number (match-string 2 text))
             (unit (match-string 3 text))
             (tail (match-string 4 text))
             (word (cdr (assoc unit '(("" . "days") ("h" . "hours")
                                      ("d" . "days") ("w" . "weeks")
                                      ("m" . "months") ("y" . "years"))))))
        (when (= (length sign) 1) (setq reference (current-time)))
        (setq text (concat (if (string-prefix-p "+" sign)
                              (format "in %s %s" number word)
                            (format "%s %s ago" number word))
                           (when tail (concat " " tail))))))
     ((string-match-p (rx bos (+ digit) eos) text)
      (setq text (concat "on the " text "th"))))
    (cons (string-trim text) reference)))

(defun org-natural-dates-duckling--clock-p (text)
  "Return non-nil when TEXT explicitly includes a clock time."
  (string-match-p
   (rx (or ":" (seq digit (* space) (or "am" "pm"))
           (seq bow (or "am" "pm") eow)
           "noon" "midnight" "morning" "evening" "afternoon"
           "hour" "minute" "second"))
   (downcase text)))

(defun org-natural-dates-duckling--parse (input strict)
  "Parse INPUT, requiring the entire date expression when STRICT."
  (let* ((case-fold-search t)
         (reference (or org-natural-dates-duckling--reference (current-time)))
         (rep (org-natural-dates--parse-repeater input))
         (repeater (car rep))
         (text (cdr rep))
         (type :scheduled)
         title-prefix)
    ;; Duckling treats meal names as time expressions.  Retain these words
    ;; when they are the task title instead of subtracting the entire match.
    (when (and (not strict)
               (string-match (rx bos (group (or "Breakfast" "Lunch" "Dinner")) (+ space)) text))
      (setq title-prefix (match-string 1 text)
            text (substring text (match-end 0))))
    ;; Preserve repeaters in canonical timestamps passed back by Org itself.
    (when (and (string-match-p (rx bos (? (any "<[")) (= 4 digit) "-") text)
               (string-match (rx (+ space) (group (or "+" "++" ".+"))
                                 (group (+ digit) (any "hdwmy")) (? (any ">]")) eos) text))
      (setq repeater (concat (match-string 1 text) (match-string 2 text))
            text (concat (substring text 0 (match-beginning 0))
                         (when (string-prefix-p "<" text) ">")
                         (when (string-prefix-p "[" text) "]"))))
    (when (or (string-match-p (rx bow "every" eow) text)
              (and (not repeater)
                   (string-match-p (rx (+ space) (or "daily" "weekly" "monthly" "yearly") eow) text)))
      (user-error "Unsupported recurrence; use every day/week/month or every N weeks"))
    (when repeater (setq text (string-trim (replace-regexp-in-string (rx bow "of" (* space) eos) "" text))))
    (when (string-match (rx bos (or "due" "by" "deadline") (+ space)) text)
      (setq text (substring text (match-end 0)) type :deadline))
    (pcase-let* ((`(,normalized . ,base) (org-natural-dates-duckling--normalize text reference))
                 (clock-only (and strict
                                  (string-match-p
                                   (rx bos (? "at ") (+ digit) (? ":" (= 2 digit))
                                       (* space) (? (or "am" "pm")) eos)
                                   normalized)))
                 ;; Clock-only edits keep the calendar's selected day.
                 (normalized (if clock-only
                                 (concat (format-time-string "%Y-%m-%d" base) " at "
                                         (string-remove-prefix "at " normalized))
                               normalized))
                 (base (if clock-only
                           (let ((date (decode-time base)))
                             (encode-time 0 0 0 (nth 3 date) (nth 4 date) (nth 5 date)))
                         base))
                 (response (unless (string-empty-p normalized)
                             (org-natural-dates-duckling--request normalized base)))
                 (entities (alist-get 'entities response))
                 (entity (car entities)))
      (when (> (length entities) 1)
        (user-error "More than one date found; enter one scheduling expression"))
      (when (and (null entity) (or strict
                                  (string-match-p (rx (or (seq bow (or "tomorrow" "next" "due") eow)
                                                         (seq bow (or "at" "in") (+ space) digit))) text)))
        (unless (or (string-empty-p normalized) repeater)
          (user-error "Date expression was not understood")))
      (when (and entity (not (equal (alist-get 'kind entity) "single")))
        (user-error "Time ranges and day periods are not supported yet"))
      (when (and entity (not (member (alist-get 'grain entity) '("Week" "Day" "Hour" "Minute" "Second"))))
        (user-error "Specify a particular day, not just a month or year"))
      (let* ((start (and entity (alist-get 'start entity)))
             (end (and entity (alist-get 'end entity)))
             (before (if entity (substring normalized 0 start) normalized))
             (after (if entity (substring normalized end) ""))
             (body (and entity (alist-get 'body entity)))
             (time (and entity (org-natural-dates-duckling--clock-p body)
                        (alist-get 'time entity))))
        (when (and time (equal (alist-get 'grain entity) "Day"))
          (user-error "Recognized the day but not the clock time"))
        (when (string-match (rx (or "due" "by" "deadline") (+ space) eos) before)
          (setq before (substring before 0 (match-beginning 0)) type :deadline))
        (when (and strict (not (string-empty-p (string-trim (concat before after)))))
          (user-error "Unparsed date text: %s" (string-trim (concat before " " after))))
        (when (and (not strict) entity
                   (string-match-p
                    (rx (or (seq bow (or "next" "every" "at" "due" "ago") eow)
                            (seq digit (or ":" "/" "-"))))
                    (concat before " " after)))
          (user-error "Part of the scheduling expression was not understood"))
        ;; Duckling currently chooses AM without retaining the PM candidate.
        (when (and body (not (org-natural-dates-duckling--clock-p body))
                   (string-match-p (rx bow "at" (+ space) (or (any "123456789") "10" "11" "12") (or space eos)) body))
          (user-error "Specify am or pm for the hour"))
        ;; The Rust port currently ignores the UK ordering for slash dates.
        (when (string-match-p (rx digit "/" digit) normalized)
          (user-error "Use an ISO date or a month name instead of slash dates"))
        (when (and body (string-match-p (rx bow (or "after" "before") eow) body))
          (user-error "Before/after constraints need an explicit date and time"))
        (let* ((date (if entity (alist-get 'date entity) (format-time-string "%Y-%m-%d" base)))
               (stamp (when (or entity repeater strict)
                        (concat "<" date
                                (when time (concat " " time))
                                (when repeater (concat " " repeater)) ">"))))
          (list :text (string-join (split-string (concat title-prefix " " before " " after)) " ")
                :date (and entity date) :time time :repeater repeater :type type
                :org-timestamp stamp))))))

(defun org-natural-dates-duckling-parse (input)
  "Extract a task title and scheduling expression from INPUT."
  (org-natural-dates-duckling--parse input nil))

(defun org-natural-dates-duckling--read-date (original &rest args)
  "Keep Org's calendar UI while interpreting its text with Duckling."
  (let ((org-natural-dates-duckling--reading t)
        (org-natural-dates-duckling--cache (make-hash-table :test #'equal))
        (minibuffer-setup-hook
         (cons #'org-natural-dates-duckling--setup minibuffer-setup-hook)))
    (apply original args)))

(defun org-natural-dates-duckling--analyze (original answer default decoded)
  "Interpret ANSWER relative to DEFAULT, or call ORIGINAL outside the prompt."
  (if (not org-natural-dates-duckling--reading)
      (funcall original answer default decoded)
    (let* ((org-natural-dates-duckling--reference default)
           (parsed (org-natural-dates-duckling--parse (string-trim answer) t))
           (date (substring (plist-get parsed :org-timestamp) 1 11))
           (time (or (plist-get parsed :time) (format-time-string "%H:%M" default))))
      (when (plist-get parsed :repeater)
        (user-error "Use natural language capture for recurrence; the date prompt cannot return a repeater"))
      (setq org-time-was-given (and (plist-get parsed :time) t)
            org-end-time-was-given nil
            org-read-date-analyze-futurep nil
            org-read-date-analyze-forced-year nil)
      (decode-time (encode-time 0 (string-to-number (substring time 3 5))
                                (string-to-number (substring time 0 2))
                                (string-to-number (substring date 8 10))
                                (string-to-number (substring date 5 7))
                                (string-to-number (substring date 0 4)))))))

(defun org-natural-dates-duckling--display (original)
  "Display partial-input errors without interrupting minibuffer editing."
  (if (not org-natural-dates-duckling--reading)
      (funcall original)
    (condition-case error
        (funcall original)
      (user-error
       (when (and (minibufferp) org-read-date-display-live)
         (when (overlayp org-read-date-overlay) (delete-overlay org-read-date-overlay))
         (setq org-read-date-overlay (make-overlay (point-max) (point-max)))
         (overlay-put org-read-date-overlay 'after-string
                      (propertize (concat "  => " (error-message-string error)) 'face 'warning)))))))

(defun org-natural-dates-duckling--exit ()
  "Validate input before leaving Org's date minibuffer."
  (interactive)
  (org-read-date-analyze
   (concat (minibuffer-contents-no-properties) " " (or org-ans1 org-ans2))
   org-def org-defdecode)
  (exit-minibuffer))

(defun org-natural-dates-duckling--setup ()
  "Validate RET in the shared Org date prompt."
  (when org-natural-dates-duckling--reading
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key (kbd "RET") #'org-natural-dates-duckling--exit)))

;;;###autoload
(define-minor-mode org-natural-dates-duckling-mode
  "Use Duckling in Org's date picker and natural language capture.
This covers scheduling, deadlines, active/inactive timestamps, and other
packages calling `org-read-date'.  It does not alter standalone Calendar."
  :global t :group 'org-natural-dates-duckling
  (if org-natural-dates-duckling-mode
      (progn
        (unless (eq org-natural-dates-parser-function #'org-natural-dates-duckling-parse)
          (setq org-natural-dates-duckling--saved-parser org-natural-dates-parser-function))
        (setq org-natural-dates-parser-function #'org-natural-dates-duckling-parse)
        (advice-add 'org-read-date :around #'org-natural-dates-duckling--read-date)
        (advice-add 'org-read-date-analyze :around #'org-natural-dates-duckling--analyze)
        (advice-add 'org-read-date-display :around #'org-natural-dates-duckling--display))
    (setq org-natural-dates-parser-function org-natural-dates-duckling--saved-parser)
    (advice-remove 'org-read-date #'org-natural-dates-duckling--read-date)
    (advice-remove 'org-read-date-analyze #'org-natural-dates-duckling--analyze)
    (advice-remove 'org-read-date-display #'org-natural-dates-duckling--display)
    (org-natural-dates-duckling-stop)))

(provide 'org-natural-dates-duckling)
;;; org-natural-dates-duckling.el ends here
