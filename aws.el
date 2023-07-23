;;; aws.el --- AWS CLI-*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 0.1
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

(defmacro aws-comment (&rest _) "Ignore body return nil." nil)

(defun aws--s3-ls-format (output)
  "Take OUTPUT from aws s3 ls and format it."
  (->> output
       (s-split "\n")
       (--map (s-split " " it))))

(defun aws--s3-ls (&optional path)
  "Execute aws s3 ls on PATH command and parse result into list."
  (-> (shell-command-to-string (format "aws s3 ls %s" (or path "")))
      aws--s3-ls-format))

(aws-comment
 (aws--s3-ls)
 (aws--s3-ls "ukraine-see-the-real"))

(defun aws--list-buckets (row)
  "Take one ROW of `aws--s3-ls' output and prepare print data."
  `(("Date" 12 ,(car row))
    ("Time" 12 ,(cadr row))
    ("Name" 80 ,(propertize (or (caddr row) "") 'face 'bold))))

(defun aws--prepare-columns (data)
  "Prepare columns from DATA."
  (->> (--map (list (car it) (cadr it)) (car data))
       (apply #'vector)))

(defun aws--prepare-rows (data)
  "Prepare rows from DATA."
  (let ((lists   (-map (lambda (el) (->> (-flatten (-map 'cddr el)) )) data)))
    (-map (lambda (el) `(nil [,@el])) lists)))

(define-derived-mode aws-s3-list-mode tabulated-list-mode "AWS - S3 Buckets"
  "Heroku app list mode."
  (let* ((buckets (->> (aws--s3-ls) (mapcar #'aws--list-buckets)))
         (columns (aws--prepare-columns buckets))
         (rows (aws--prepare-rows buckets)))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

;;;###autoload
(defun aws-s3-list ()
  "List AWS S3 buckets."
  (interactive)
  (let ((buff "*AWS - S3 Buckets*"))
    (switch-to-buffer buff)
    (aws-s3-list-mode)))

(aws-comment
 (use-package s3ed)
 (setq rows (aws--s3-ls))
 (setq data (mapcar #'aws--list-buckets rows))
 (aws--prepare-columns data)
 (aws--prepare-rows data))

;; 2023-07-23 Resume
(defun aws--format-ts (ts)
  "Format AWS TS to human readable string."
  (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time (/ ts 1000))))

(defun aws--command (cmd)
  "Run a cli CMD and return output as a hash-map."
  (->> cmd
       (format "aws %s --output json")
       shell-command-to-string
       h<-json*))

(defun aws--describe-log-groups ()
  "Get available log groups."
  (aws--command "logs describe-log-groups"))

(defun aws--log-group-row (group)
  "Turn hash table GROUP produced by `aws--describe-log-groups' into table row."
  (h-let group
    `(("Name" 80 ,.logGroupName)
      ("Created" 20 ,(aws--format-ts .creationTime))
      ("Retention" 7 ,(format "%s" (or .retentionInDays 0)))
      ;; ("FilterCount" 12 ,(format "%s" .metricFilterCount))
      ;; ("ARN" 12 ,.arn)
      ;; ("Stored Bytes" ,.storedBytes)
      )))

(defun aws--log-groups-table (map)
  "Turn MAP produced by `aws--describe-log-groups' into table."
  (mapcar #'aws--log-group-row (h-get map "logGroups")))

(define-derived-mode aws-log-groups-mode tabulated-list-mode "AWS - Log Groups"
  "Heroku app list mode."
  (let* ((groups (->> (aws--describe-log-groups) aws--log-groups-table))
         (columns (aws--prepare-columns groups))
         (rows (aws--prepare-rows groups)))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

;;;###autoload
(defun aws-log-groups ()
  "List all CloudWatch log groups."
  (interactive)
  (let ((buff "*AWS - Log Groups"))
    (switch-to-buffer-other-window buff)
    (aws-log-groups-mode)))

(aws-comment
 (setq map (aws--describe-log-groups))
 (setq groups (h-get s "logGroups"))
 (setq group (aref groups 0))
 (h-htbl-form s)
 (h-ht-form s)
 (h-keys s)
 (setq sample (aref grps 0))
 (h-keys sample)
 (h-get sample "logGroupName")
 )

(provide 'aws)
;;; aws.el ends here
