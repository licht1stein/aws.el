;;; aws.el --- AWS CLI-*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
;;
;;; Code:
(require 'dash)
(require 's)

(defun aws--s3-ls ()
  "Execute aws s3 ls command and parse result into list."
  (->> (shell-command-to-string "aws s3 ls")
       (s-split "\n")
       (--map (s-split " " it))))

(defun aws--s3-ls-data (row)
  "Take one ROW of `aws--s3-ls' output and prepare print data."
  `(("Name" 80 ,(caddr row))
    ("Date" 12 ,(car row))
    ("Time" 12 ,(cadr row))))

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
  (let* ((buckets (->> (aws--s3-ls) (mapcar #'aws--s3-ls-data)))
         (columns (aws--prepare-columns buckets))
	       (rows (aws--prepare-rows buckets)))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode)))

;;;###autoload
(defun aws-s3-list ()
  "List AWS S3 buckets"
  (interactive)
  (let ((buff "*AWS - S3 Buckets*"))
    (switch-to-buffer buff)
    (aws-s3-list-mode)))


(comment
 (setq rows (aws--s3-ls))
 (setq data (mapcar #'aws--s3-ls-data rows))
 (aws--prepare-columns data)
 (aws--prepare-rows data)
 )


(provide 'aws)
;;; aws.el ends here
