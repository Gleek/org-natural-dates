;;; org-natural-dates.el --- Natural language date parsing for Org mode -*- lexical-binding: t; -*-

;; Author: Edd Wilder-James <edd@ewilderj.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.0"))
;; Keywords: calendar, org
;; URL: https://github.com/ewilderj/org-natural-dates

;;; Commentary:

;; Natural language date parsing for Org mode capture, scheduling, and deadlines.
;;
;; This package provides interactive wrappers around `org-schedule' and
;; `org-deadline' that allow you to type natural language dates like
;; "every Friday at 3pm" or "next Tuesday".  It also provides a capture
;; helper `org-natural-dates' for full task parsing.

;;; Code:

(require 'org)

(declare-function org-agenda-schedule "org-agenda")
(declare-function org-agenda-deadline "org-agenda")

;;; Constants

(defconst org-natural-dates--repeater-regexp
  (rx (seq (group "every" (1+ space))
           (group (or "day" "daily"
                      "week" "weekly"
                      "month" "monthly"
                      "year" "yearly"
                      (seq "other" (1+ space) (or "day" "week" "month" "year"))
                      (seq (1+ digit) (1+ space) (or "day" "days" "week" "weeks" "month" "months" "year" "years"))
                      "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"
                      "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
  "Regex for recurring intervals.  Matched case-insensitively.")

(defconst org-natural-dates--time-regexp
  (rx (or
       ;; Require "at" for simple numbers to avoid date collisions
       (seq (or "at" "@") (1+ space)
            (1+ digit) (optional (seq ":" (1+ digit)))
            (optional (optional space) (or "am" "pm")))
       ;; Without "at", must have colon or am/pm
       (seq (1+ digit) ":" (1+ digit) (optional (optional space) (or "am" "pm")))
       (seq (1+ digit) (optional (seq ":" (1+ digit))) (optional space) (or "am" "pm"))))
  "Regex for time expressions.  Matched case-insensitively.")

(defconst org-natural-dates--date-regexp
  (rx (seq bow
           (optional (seq (group (or "on" "in" "due" "by" "this" "next" "last" "deadline")) (1+ space)))
           (optional (seq (or "on" "in" "at" "of" "the") (1+ space)))
           (or "today" "tomorrow" "yesterday"
               (seq (1+ digit) (1+ space) (or "day" "days" "week" "weeks" "month" "months" "year" "years"))
               "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"
               (seq (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") eow)
               (seq (1+ digit) (optional (or "st" "nd" "rd" "th")) (1+ space)
                    (optional (seq "of" (1+ space)))
                    (or "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
                        "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
               (seq (1+ digit) (optional (or "st" "nd" "rd" "th"))
                    (optional (seq (1+ space) "of"))))))
  "Regex for date expressions.  Matched case-insensitively.")

;;; Parsing Functions

(defcustom org-natural-dates-parser-function nil
  "Optional function parsing tasks into `org-natural-dates-parse' results.
Nil uses the built-in Elisp parser.  An external parser must reject
unsupported scheduling expressions rather than silently guessing."
  :type '(choice (const nil) function)
  :group 'org)

(defun org-natural-dates--parse-repeater (str)
  "Extract repeater from STR.  Return (REPEATER . CLEANED-STR)."
  (let ((repeater nil)
        (clean-str str)
        (case-fold-search t))
    (when (string-match org-natural-dates--repeater-regexp str)
      (let* ((unit-part (match-string 2 str))
             (is-weekday nil)
             (interval (save-match-data
                         (cond
                          ((string-match-p (rx bos (or "day" "daily") eos) unit-part) "+1d")
                          ((string-match-p (rx bos (or "week" "weekly") eos) unit-part) "+1w")
                          ((string-match-p (rx bos (or "month" "monthly") eos) unit-part) "+1m")
                          ((string-match-p (rx bos (or "year" "yearly") eos) unit-part) "+1y")
                          ((string-match (rx bos "other " (group (or "day" "week" "month" "year")) eos) unit-part)
                           (concat "+2" (substring (match-string 1 unit-part) 0 1)))
                          ((string-match (rx (group (1+ digit)) " " (group (or "day" "days" "week" "weeks" "month" "months" "year" "years"))) unit-part)
                           (concat "+" (match-string 1 unit-part) (substring (match-string 2 unit-part) 0 1)))
                          ((string-match-p (rx (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")) unit-part)
                           (setq is-weekday t)
                           "+1w")
                          (t "+1d")))))
        (setq repeater interval)
        (if is-weekday
            (setq clean-str (string-trim (replace-match unit-part t nil str)))
          (setq clean-str (string-trim (replace-match "" nil nil str))))))
    (cons repeater clean-str)))

(defun org-natural-dates--parse-time (str)
  "Extract time from STR.  Return (TIME . CLEANED-STR)."
  (let ((time nil)
        (clean-str str)
        (case-fold-search t))
    (when (string-match org-natural-dates--time-regexp str)
      (let* ((match (match-string 0 str))
             (time-res (save-match-data
                         (let ((time-part (if (string-match (rx (or "at" "@") (1+ space)) match)
                                              (substring match (match-end 0))
                                            match)))
                           (if (not (string-match-p (rx alpha) time-part))
                               (if (string-match (rx bos (group (1+ digit)) (optional ":" (1+ digit)) eos) time-part)
                                   (let ((hour (string-to-number (match-string 1 time-part))))
                                     (cond
                                      ((<= 7 hour 11) (concat time-part "am"))
                                      ((<= 1 hour 6) (concat time-part "pm"))
                                      ((= hour 12) (concat time-part "pm"))
                                      (t time-part)))
                                 time-part)
                             time-part)))))
        (setq time time-res)
        (setq clean-str (string-trim (replace-match "" nil nil str)))))
    (cons time clean-str)))

(defun org-natural-dates--parse-date (str)
  "Extract date expression from STR.
Return a list (DATE TYPE CLEANED-STR) where TYPE is :scheduled or :deadline."
  (let ((date nil)
        (type :scheduled)
        (clean-str str)
        (case-fold-search t))
    (when (string-match org-natural-dates--date-regexp str)
      (let* ((match (match-string 0 str))
             (preposition (match-string 1 str)))

        ;; Determine type based on preposition
        (when (and preposition
                   (string-match-p (rx bos (or "due" "by" "deadline") eos) preposition))
          (setq type :deadline))

        ;; Extract the actual date part (strip "on"/"in"/"due"/"by"/"this"/"deadline",
        ;; but preserve "next"/"last" as date qualifiers)
        (let ((date-part (save-match-data
                           (let ((dp (if (string-match (rx (or "on" "in" "due" "by" "this" "deadline") (1+ space)) match)
                                         (substring match (match-end 0))
                                       match)))
                             ;; Strip ordinals ("the 15th of March" -> "15 March")
                             (if (string-match (rx bos (optional "the ") (group (1+ digit))
                                                   (optional (or "st" "nd" "rd" "th"))
                                                   (optional (seq (1+ space) "of"))
                                                   (optional (seq (1+ space) (group (1+ alpha))))
                                                   eos)
                                               dp)
                                 (if (match-string 2 dp)
                                     (concat (match-string 1 dp) " " (match-string 2 dp))
                                   (match-string 1 dp))
                               dp)))))
          (setq date date-part))
        (setq clean-str (string-trim (replace-match "" nil nil str)))))
    (list date type clean-str)))

;;; Main Function

(defconst org-natural-dates--day-numbers
  '(("sunday" . 0) ("monday" . 1) ("tuesday" . 2) ("wednesday" . 3)
    ("thursday" . 4) ("friday" . 5) ("saturday" . 6)
    ("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3)
    ("thu" . 4) ("fri" . 5) ("sat" . 6))
  "Map of day names to `decoded-time-weekday' numbers (0=Sun).")

(defun org-natural-dates--date-to-org (date)
  "Translate natural DATE string to `org-read-date' compatible syntax.
\"tomorrow\" → \"+1d\", \"next Friday\" → \"Friday\" (upcoming),
\"last Tuesday\" → \"-Nd\" (most recent past), \"3 days\" → \"+3d\"."
  (let ((case-fold-search t))
    (pcase (and date (downcase date))
      ("today" ".")
      ("tomorrow" "+1d")
      ("yesterday" "-1d")
      ;; "next <day>" — upcoming occurrence, same as bare day name
      ((and (pred stringp)
            (guard (string-match (rx bos "next " (group (1+ alpha)) eos) date)))
       (match-string 1 date))
      ;; "last <day>" — most recent past occurrence
      ((and (pred stringp)
            (guard (string-match (rx bos "last " (group (1+ alpha)) eos) date)))
       (let* ((day-name (downcase (match-string 1 date)))
              (target-dow (cdr (assoc day-name org-natural-dates--day-numbers)))
              (today-dow (decoded-time-weekday (decode-time)))
              (days-ago (mod (- today-dow target-dow) 7)))
         ;; Same day-of-week means 7 days ago, not today
         (when (zerop days-ago) (setq days-ago 7))
         (format "-%dd" days-ago)))
      ;; "in N days/weeks/months/years" or bare "N days/weeks/..."
      ((and (pred stringp)
            (guard (string-match (rx bos (optional "in ") (group (1+ digit)) " "
                                     (group (or "day" "days" "week" "weeks" "month" "months" "year" "years")) eos)
                                 date)))
       (concat "+" (match-string 1 date) (substring (match-string 2 date) 0 1)))
      (_ (or date ".")))))

(defun org-natural-dates--parse-elisp (input)
  "Parse natural language INPUT into a property list.
Returns (:text :repeater :time :date :type :org-timestamp)."
  (let* ((res-rep (org-natural-dates--parse-repeater input))
         (repeater (car res-rep))
         (text (cdr res-rep))

         (res-time (org-natural-dates--parse-time text))
         (time (car res-time))
         (text (cdr res-time))

         (res-date (org-natural-dates--parse-date text))
         (date (nth 0 res-date))
         (type (nth 1 res-date))
         (text (nth 2 res-date))

         (base-date (org-natural-dates--date-to-org date))
         (full-ts-str (concat base-date " " (or time "")))

         (parsed-time (condition-case err
                          (org-read-date nil t full-ts-str)
                        (error
                         (message "org-natural-dates: failed to parse %S: %S" full-ts-str err)
                         (current-time))))

         (normalized-time (when time
                            (format-time-string "%H:%M" parsed-time)))

         (final-ts (format-time-string
                    (concat "<%Y-%m-%d %a"
                            (if normalized-time " %H:%M" "")
                            (if repeater (concat " " repeater) "")
                            ">")
                    parsed-time)))

    (list :text text
          :repeater repeater
          :time normalized-time
          :date date
          :type type
          :org-timestamp final-ts)))

(defun org-natural-dates-parse (input)
  "Parse INPUT using `org-natural-dates-parser-function' when configured."
  (if org-natural-dates-parser-function
      (funcall org-natural-dates-parser-function input)
    (org-natural-dates--parse-elisp input)))

;;; Interactive Command

(defvar org-natural-dates-last-result nil
  "Stores the last parse result for use in capture templates.")

(defun org-natural-dates-get (key)
  "Get KEY from `org-natural-dates-last-result`.
KEY can be :text, :org-timestamp, :repeater, :time, :date, :type."
  (plist-get org-natural-dates-last-result key))

(defun org-natural-dates-get-timestamp-line ()
  "Return the appropriate timestamp line (SCHEDULED or DEADLINE)."
  (let ((ts (org-natural-dates-get :org-timestamp))
        (type (org-natural-dates-get :type)))
    (if ts
        (concat (if (eq type :deadline) "DEADLINE: " "SCHEDULED: ") ts)
      "")))

;;;###autoload
(defun org-natural-dates (&optional string)
  "Prompt for a natural language task and capture it.
If STRING is provided, use it as the task input instead of prompting.
The capture template should use %(org-natural-dates-get :text)
and %(org-natural-dates-get-timestamp-line)."
  (interactive)
  (let* ((input (or string (read-string "Task: ")))
         (result (org-natural-dates-parse input)))
    (setq org-natural-dates-last-result result)
    (org-capture)))

;;;###autoload
(defun org-natural-dates-capture-finalize-hook ()
  "Parse the capture headline and apply natural dates.
Add this to `org-capture-before-finalize-hook' to automatically
parse natural language dates from task titles when finishing a capture."
  (save-excursion
    ;; Move to the heading being captured
    (condition-case nil
        (org-back-to-heading t)
      (error (goto-char (point-min))))

    (when (looking-at org-complex-heading-regexp)
      (let* ((title (match-string-no-properties 4))
             (mb (match-beginning 4))
             (me (match-end 4))
             (parsed (org-natural-dates-parse title)))
        (when (and title
                   (or (plist-get parsed :date)
                       (plist-get parsed :time)
                       (plist-get parsed :repeater)))
          ;; Replace the title with the cleaned text
          (let ((new-title (plist-get parsed :text)))
            (goto-char mb)
            (delete-region mb me)
            (insert new-title))
          ;; Add the schedule or deadline
          (let ((ts (plist-get parsed :org-timestamp))
                (type (plist-get parsed :type)))
            (if (eq type :deadline)
                (org-deadline nil ts)
              (org-schedule nil ts))))))))

;;; Interactive Scheduling Wrappers

(defun org-natural-dates--apply (prompt org-fn arg)
  "Prompt with PROMPT, parse natural date, and call ORG-FN.
If ARG is non-nil, fall back to ORG-FN's default behavior."
  (if arg
      (funcall org-fn arg)
    (let* ((input (read-string prompt))
           (result (org-natural-dates-parse input)))
      (funcall org-fn nil (plist-get result :org-timestamp)))))

;;;###autoload
(defun org-natural-dates-schedule (arg)
  "Schedule the item at point using natural language.
With prefix ARG, fall back to standard `org-schedule'."
  (interactive "P")
  (org-natural-dates--apply "Scheduled to: " #'org-schedule arg))

;;;###autoload
(defun org-natural-dates-deadline (arg)
  "Set deadline at point using natural language.
With prefix ARG, fall back to standard `org-deadline'."
  (interactive "P")
  (org-natural-dates--apply "Deadline to: " #'org-deadline arg))

;;;###autoload
(defun org-natural-dates-agenda-schedule (arg)
  "Schedule the agenda item at point using natural language.
With prefix ARG, fall back to standard `org-agenda-schedule'."
  (interactive "P")
  (org-natural-dates--apply "Scheduled to: " #'org-agenda-schedule arg))

;;;###autoload
(defun org-natural-dates-agenda-deadline (arg)
  "Set deadline for agenda item at point using natural language.
With prefix ARG, fall back to standard `org-agenda-deadline'."
  (interactive "P")
  (org-natural-dates--apply "Deadline to: " #'org-agenda-deadline arg))

(provide 'org-natural-dates)

;; Local Variables:
;; ispell-buffer-session-localwords: ("CLEANED" "STR" "ORG" "FN")
;; End:
;;; org-natural-dates.el ends here
