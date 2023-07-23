;;; aws.el --- AWS CLI-*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2") (dash "2.19.1") (s "1.13.0") (ht "2.3") (transient "0.4.1"))
;;
;; Created: 21 Sep 2022
;;
;; URL: https://github.com/licht1stein
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Interface for AWS CLI.
;;
;;; Code:
(require 'dash)
(require 's)
(require 'xht)
(require 'comint)
(require 'cl-lib)
(require 'transient)

(defgroup aws nil "AWS.el group." :group 'convenience)

(defcustom aws-logs-default-output-format "short"
  "Default output format for Cloudwatch logs."
  :type '(radio (const :tag "short" "short")
                (const :tag "json" "json")
                (const :tag "detailed" "detailed"))
  :group 'aws)

;;  === COMMON FUNCTIONS ===
(defmacro aws-comment (&rest _) "Ignore body return nil." nil)

(cl-defun aws--format-ts (ts &key (format "%F %R"))
  "Format AWS TS to human readable string.

Accepted keword args:
:format FORMAT - format-string to pass to `format-time-string'"
  (if (integerp ts)
      (format-time-string format (seconds-to-time (/ ts 1000)))
    "N/A"))

(defun aws--selected-row ()
  "Read first column from the selected row of table."
  (interactive)
  (if (derived-mode-p 'tabulated-list-mode)
      (aref (tabulated-list-get-entry) 0)
    (error "Not in AWS list")))

(defun aws--prepare-columns (data)
  "Prepare columns from DATA."
  (->> (--map (list (car it) (cadr it)) (car data))
       (apply #'vector)))

(defun aws--prepare-rows (data)
  "Prepare rows from DATA."
  (let ((lists   (-map (lambda (el) (->> (-flatten (-map 'cddr el)) )) data)))
    (-map (lambda (el) `(nil [,@el])) lists)))


(defmacro define-aws-list-mode (name description fetch-fn)
  "Define a mode derived from `tabulated-list-mode'.

NAME - new mode name
DESCRIPTION - new mode description
FETCH-FN - arity 0 function that returns formatted rows for the mode"
  `(define-derived-mode ,name tabulated-list-mode ,description
     (let* ((data (,fetch-fn))
            (columns (aws--prepare-columns data))
            (rows (aws--prepare-rows data)))
      (setq tabulated-list-format columns)
      (setq tabulated-list-entries rows)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (hl-line-mode))))

(defun aws--sort-table (original-list column &optional reverse)
  "Sort ORIGINAL-LIST by COLUMN by alphabet. REVERSE if not nil."
  (let* ((sorted (sort original-list
                       (lambda (a b)
                         (string< (caddr (assoc column b))
                                  (caddr (assoc column a)))))))
    (if reverse
        (reverse sorted)
      sorted)))
;; === END COMMON FUNCTIONS ===

;; CLI COMMANDS
;; =====================================================
(defun aws--command-prepare (cmd &rest args)
  "Prepare a command string for AWS CLI using CMD and ARGS.
CMD is the AWS command to execute. ARGS is the list of additional arguments for the command."
  (let ((command (->> (cons cmd args)
                      (s-join " ")
                      (s-replace "aws " ""))))
    (format "aws %s --output json --no-cli-auto-prompt"
            (if (s-starts-with-p "aws" command)
                (substring command 4)
              command))))

(defun aws--command-to-string (cmd &rest args)
  "Run an aws cli CMD with ARGS and return string output."
  (->> (apply #'aws--command-prepare cmd args)
       shell-command-to-string))

(defun aws--error-p (string)
  "Check if STRING string is an AWS CLI error."
  (or (s-contains-p "An error occurred" string)
      (s-contains-p "Could not" string)))

(defun aws-command-to-map(cmd &rest args)
  "Run a cli CMD with ARGS and return output as a hash-map."
  (let* ((output (apply #'aws--command-to-string cmd args)))
    (if (aws--error-p output)
        (error (s-trim output))
      (condition-case err
          (h<-json* output)
        (error (message "Failed to parse CLI output: %S" err))))))

(defun aws--describe-log-groups ()
  "Get available log groups."
  (message "Getting log groups...")
  (h-get (aws-command-to-map"logs describe-log-groups") "logGroups"))

(cl-defun aws--describe-log-streams
    (log-group &key (order-by "LastEventTime") (max-items 500) prepare-only)
  "Run describe log streams for the specified LOG-GROUP.

Accept the following keyword args:

:order-by ORDER-BY (default \"LastEventTime\")
          If the value is LogStreamName ,  the  results  are  ordered  by  log
          stream name. If the value is LastEventTime , the results are ordered
          by the event time. The default value is LogStreamName .

          If you order the results by  event  time,  you  cannot  specify  the
          logStreamNamePrefix parameter.
              lastEventTimestamp  represents  the  time of the most recent log
              event in the log stream  in  CloudWatch  Logs.  This  number  is
              expressed  as  the  number  of  milliseconds  after  Jan 1, 1970
              00:00:00 UTC . lastEventTimestamp updates on an eventual consis-
              tency  basis.  It  typically  updates  in less than an hour from
              ingestion, but in rare situations might take longer.

:max-items MAX-ITEMS (default 500)
          The  total number of items to return in the command's output. If the
          total number of items available is more than the value specified,  a
          NextToken is provided in the command's output. To resume pagination,
          provide the NextToken value in the starting-token argument of a sub-
          sequent  command. Do not use the NextToken response element directly
          outside of the AWS CLI.

Other:
:prepare-only PREPARE-ONLY
          Returns the final string command to run in shell and adds it to kill
          ring."
  (message "Getting log streams for %s..." log-group)
  (let* ((command (aws--command-prepare
                   "logs describe-log-streams --log-group-name" log-group
                   "--order-by" order-by
                   "--descending"
                   (when max-items (s-concat "--max-items " (format "%s" max-items)) ))))
    (if prepare-only
        (progn
          (kill-new command)
          command)
      (h-get (aws-command-to-map command) "logStreams"))))

;; END CLI COMMANDS
;; =====================================================

;; WORKING WITH AWS CLOUDWATCH LOGS (aws logs)
;; === Log Streams ===
(defvar aws--selected-log-group nil "Currently selected AWS log group.")
(defvar aws--selected-stream nil "Currently selected log group stream.")

(define-derived-mode aws-logs-mode comint-mode "AWS Logs" (read-only-mode))

;;;###autoload
(cl-defun aws-logs (log-group &key streams follow)
  "Get AWS logs for LOG-GROUP.

Accept keyword arguments:
:streams STREAMS to filter by
:follow FOLLOW - tail logs"
  (interactive (list (read-from-minibuffer "Log group to stream: ")))
  (let* ((buffer (format "*AWS Logs: %s*" log-group))
         (follow-arg (when follow '("--follow")))
         (streams-arg (when streams (list "--log-stream-names" streams)))
         (args (append follow-arg streams-arg)))
    (message "Getting AWS logs: %s..." log-group)
    (apply #'make-comint-in-buffer "aws-logs" buffer "aws" nil "logs" "tail" log-group args)
    (with-current-buffer buffer
      (aws-logs-mode)
      (pop-to-buffer-same-window buffer))))

(defun aws--log-stream-row (stream)
  "Turn a log STREAM into table row."
  (h-let stream
    `(("Name" 80 ,.logStreamName)
      ("Created" 20 ,(aws--format-ts .creationTime))
      ("First Event" 20 ,(aws--format-ts .firstEventTimestamp))
      ("Last Event" 20 ,(aws--format-ts .lastEventTimestamp))
      ("Last Ingest" 20 ,(aws--format-ts .lastIngestionTime)))))

(defun aws--prepare-log-streams-selected-log-group ()
  "Run `aws--describe-log-streams' for `aws--selected-log-group', then prepare with `aws--log-streams-table'."
  (if aws--selected-log-group
      (aws--log-streams-table (aws--describe-log-streams aws--selected-log-group))
    (error "`aws--selected-log-group' is nil")))

(defun aws--log-streams-table (streams)
  "Turn STREAMS produced by `aws--describe-log-streams' into table."
  (mapcar #'aws--log-stream-row streams))

(define-aws-list-mode aws-log-streams-mode "AWS - Log Streams" aws--prepare-log-streams-selected-log-group)

(defun aws--logs-stream-list-tail-logs ()
  "Tail logs of the currently selected AWS Log Stream in the AWS Log Group from the custom AWS Log Viewer interface."
  (interactive)
  (let ((stream (aws--selected-row)))
    (aws-logs aws--selected-log-group :streams stream :follow t)))

(bind-keys
 :map aws-log-streams-mode-map
 ("RET" . aws--logs-stream-list-tail-logs))

;; ==== Log Groups ====
(defun aws--log-group-row (group)
  "Turn hash table GROUP produced by `aws--describe-log-groups' into table row."
  (h-let group
    `(("Name" 80 ,.logGroupName)
      ("Created Date" 14 ,(aws--format-ts .creationTime :format "%F"))
      ("Created Time" 14 ,(aws--format-ts .creationTime :format "%T"))
      ("Retention" 7 ,(format "%s" (or .retentionInDays 0))))))

(defun aws--log-groups-table (groups)
  "Turn GROUPS produced by `aws--describe-log-groups' into table."
  (aws--sort-table (mapcar #'aws--log-group-row groups) "Name"))

(defun aws--prepare-log-groups-table ()
  "Run `aws--describe-log-groups' then prepare with `aws--log-groups-table'."
  (->> (aws--describe-log-groups) aws--log-groups-table))

(define-aws-list-mode aws-log-groups-mode "AWS - Log Groups" aws--prepare-log-groups-table)

(defun aws--log-group-list-streams ()
  "Open log streams list for selected log group."
  (interactive)
  (setq aws--selected-log-group (aws--selected-row))
  (aws-log-streams-mode))

(defun aws--logs-from-transient (&optional args)
  "Get AWS logs for log group using ARGS."
  (interactive (list (transient-args 'aws-log-groups-transient)))
  (message "Transient args: %s" args)
  (let* ((buffer (format "*AWS Logs: %s*" aws--selected-log-group)))
    (message "Getting AWS logs for %s..." aws--selected-log-group)
    (apply #'make-comint-in-buffer "aws-logs" buffer "aws" nil "logs" "tail" aws--selected-log-group args)
    (with-current-buffer buffer
      (aws-logs-mode)
      (pop-to-buffer-same-window buffer))))

(defun aws--select-log-stream (&rest _)
  "Get options and prompt user for log stream name."
  (let* ((res (aws--describe-log-streams aws--selected-log-group))
         (stream-names (mapcar (lambda (m) (h-get m "logStreamName")) res))
         (choices (->> stream-names -distinct)))
    (completing-read "Select log stream: " choices)))

(defun aws--format-json-filter-pattern (key value)
  "Format json filter pattern for aws as KEY = VALUE."
  (format "{$.%s = %s}" key value))

(defun aws--prompt-for-log-filter-pattern (&rest _)
  "Prompt the user to define a filter pattern."
  (let* ((json-p (y-or-n-p "Would you like to define a JSON based filter?")))
    (if json-p
        (aws--format-json-filter-pattern
         (read-from-minibuffer "JSON Key: ")
         (read-from-minibuffer "JSON Value: "))
      (read-from-minibuffer "Enter filter: "))))

(defun aws--bold (s)
  (propertize s 'face 'transient-argument))

;; ==================== TRANSIENT =====================
(transient-define-prefix aws-log-groups-main ()
  "AWS Log groups transient."
  [["Commands"
    ("l" "Logs" aws-log-groups-tail-log)
    ("s" "List streams" aws--log-group-list-streams)
    ("g" "Refresh log groups" aws-log-groups)]])

(transient-define-prefix aws-log-streams-main ()
  "AWS Log groups transient."
  [["Commands"
    ("l" "Logs" aws-log-streams-tail-log)
    ("^" "Back to log groups" aws-log-groups)]])

(transient-define-argument aws-log-group-stream-names ()
  :description "Select log stream (will fetch from AWS)"
  :class 'transient-option
  :argument "--log-stream-names="
  :choices #'aws--select-log-stream)

(transient-define-argument aws-log-group-filter-pattern ()
  :description "Set filter pattern to stream, use dot notation."
  :class 'transient-option
  :argument "--filter-pattern="
  :reader #'aws--prompt-for-log-filter-pattern)

(transient-define-argument aws-log-format ()
  :description "Format to display logs"
  :class 'transient-option
  :argument "--format="
  :choices '("short" "detailed" "json"))

(transient-define-prefix aws-log-groups-tail-log ()
  "AWS tail logs transient."
  :value (lambda () (list "--follow" (format "--format=%s" aws-logs-default-output-format)))
  [:description (lambda () (s-concat "Tail Logs for " (aws--bold aws--selected-log-group) "\n"))
                ["Options"
                 ("-f" "filter pattern to use (for json use dot-notation)" aws-log-group-filter-pattern)
                 ("-n" "log stream names (fetch options)" aws-log-group-stream-names)
                 ("-N" "prefix to filter logs by" "--log-stream-name-prefix=")
                 ("-o" "output format" aws-log-format)
                 ("-s" "since (s, m, h, d, w) e.g. 10m" "--since=")
                 ("-F" "continually stream logs" "--follow")]
                ["Global options"
                 ("-d" "debug" "--debug")]]
  ["Execute"
   ("l" "tail logs" aws--logs-from-transient)])

(transient-define-prefix aws-log-streams-tail-log ()
  "AWS tail logs transient."
  :value (lambda () (list "--follow"
                          (format "--format=%s" aws-logs-default-output-format)
                          (format "--log-stream-names=%s" aws--selected-stream)))
  [:description (lambda () (format "Tail Logs for %s stream %s\n" (aws--bold aws--selected-log-group) (aws--bold aws--selected-stream)))
                ["Options"
                 ("-f" "filter pattern to use (for json use dot-notation)" aws-log-group-filter-pattern)
                 ("-n" "log stream name" "--log-stream-names=")
                 ("-o" "output format" aws-log-format)
                 ("-s" "since (s, m, h, d, w) e.g. 10m" "--since=")
                 ("-F" "continually stream logs" "--follow")]
                ["Global options"
                 ("-d" "debug" "--debug")]]
  ["Execute"
   ("l" "tail logs" aws--logs-from-transient)])


;; ==================== END TRANSIENT =====================

;; ==================== KEY BINDINGS ======================

(bind-keys
 :map aws-log-streams-mode-map
 ("?" . aws-log-streams-main)
 ("RET" . aws-log-streams-main)
 ("^" .  aws-log-groups-mode)
 ("l" . (lambda () (interactive)
          (setq aws--selected-stream (aws--selected-row))
          (aws-log-streams-tail-log))))

(bind-keys
 :map aws-log-groups-mode-map
 ("RET" . aws-log-groups-main)
 ("?" . aws-log-groups-main)
 ("l" . (lambda () (interactive)
          (setq aws--selected-log-group (aws--selected-row))
          (aws-log-groups-tail-log)))
 ("s" . aws--log-group-list-streams)
 ("g" . aws-log-groups))

;; ==================== USER COMMANDS =====================
;;;###autoload
(defun aws-log-groups ()
  "List all CloudWatch log groups."
  (interactive)
  (let ((buff "*AWS - Log Groups"))
    (switch-to-buffer buff)
    (aws-log-groups-mode)))

;;;###autoload
(defun aws-log-streams (log-group)
  "List all log streams for LOG-GROUP."
  (interactive (list (read-from-minibuffer "Log group to read streams: " aws--selected-log-group)))
  (let ((buff "*AWS - Log Streams"))
    (setq aws--selected-log-group log-group)
    (switch-to-buffer buff)
    (aws-log-streams-mode)))

(provide 'aws)
;;; aws.el ends here
