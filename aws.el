;;; aws.el --- AWS CLI-*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2") (dash "2.19.1") (s "1.13.0") (xht "2.0.0"))
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

(defgroup aws nil "AWS.el group." :group 'convenience)

;;  === COMMON FUNCTIONS ===
(defmacro aws-comment (&rest _) "Ignore body return nil." nil)

(defun aws--format-ts (ts)
  "Format AWS TS to human readable string."
  (if (integerp ts)
      (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time (/ ts 1000)))
    "N/A"))

(defun aws--selected-row ()
  "Read first column from the selected row of table."
  (interactive)
  (if (derived-mode-p 'tabulated-list-mode)
      (aref (tabulated-list-get-entry) 0)
    (error "Not in AWS list")))

(defmacro define-aws-list-mode (name description fetch-fn)
  "Define a mode derived from `tabulated-list-mode'.

NAME - new mode name
DESCRIPTION - new mode description
FETCH-FN - arity 0 function that returns formatted rows for the mode"
  `(define-derived-mode ,name tabulated-list-mode ,description
     "Heroku app list mode."
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
(defun aws--command (cmd &rest cmds)
  "Run a cli CMD and return output as a hash-map, concat CMDS."
  (let* ((output (->> (cons cmd cmds)
                      (s-join " ")
                      (format "aws %s --output json")
                      shell-command-to-string)))
    (if (s-contains-p "An error occurred" output)
        (error "CLI Error: %s" (s-trim output))
      (condition-case err
          (h<-json* output)
        (error (message "Failed to parse CLI output: %S" err))))))
 
(defun aws--describe-log-groups ()
  "Get available log groups."
  (message "Getting log groups...")
  (h-get (aws--command "logs describe-log-groups") "logGroups"))

(defun aws--describe-log-streams (log-group)
  "Get available log streams for LOG-GROUP."
  (message "Getting log streams for %s..." log-group)
  (h-get (aws--command "logs describe-log-streams --log-group-name" log-group) "logStreams"))

(aws-comment
 (setq log-group "datomic-blaster-os-v1")
 (aws--describe-log-streams "datomic-blaster-os-v1")
 )

;; END CLI COMMANDS
;; =====================================================

;; WORKING WITH AWS CLOUDWATCH LOGS (aws logs)
;; === Log Streams ===
(defvar aws--selected-log-group nil "Currently selected AWS log group.")

(aws-comment
 (aws--describe-log-streams-selected-log-group)
 (setq streams (aws--describe-log-streams "datomic-blaster-os"))
 (setq tbl (aws--log-groups-table streams))

 (aws--sort-table-str tbl "Created" t)
 )

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
  (aws--sort-table (mapcar #'aws--log-stream-row streams) "Created"))


(define-aws-list-mode aws-log-streams-mode "AWS - Log Streams" aws--prepare-log-streams-selected-log-group)
(define-key aws-log-streams-mode-map (kbd "^") #'aws-log-groups-mode)

(aws-comment
 (setq streams (aws--describe-log-streams "datomic-blaster-os"))
 (setq stream (aref streams 0))
 )

;; ==== Log Groups ====
(defun aws--log-group-row (group)
  "Turn hash table GROUP produced by `aws--describe-log-groups' into table row."
  (h-let group
    `(("Name" 80 ,.logGroupName)
      ("Created" 20 ,(aws--format-ts .creationTime))
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

(bind-keys
 :map aws-log-groups-mode-map
 ("RET" . aws--log-group-list-streams)
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
