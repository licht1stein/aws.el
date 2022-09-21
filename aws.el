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
  `(("Name" 25 ,(caddr row) 'face 'bold)
    ("Date" 10 ,(car row))
    ("Time" 10 ,(cadr row))))



(provide 'aws)
;;; aws.el ends here
