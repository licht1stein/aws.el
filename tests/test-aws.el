;;; test-aws.el --- Test aws.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (buttercup "1.26"))
;;
;; Created: 23 Sep 2022
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
(require 'aws)
(require 'buttercup)

(defvar-local aws-test-example-ls "2021-12-20 15:02:04 cognitect-dev-local
2022-05-16 19:42:42 compose-segment-db-backup
2021-04-13 14:23:30 datomic-code-32068047-eb82-4f94-914b-af1f8ce6144e
2021-02-05 14:44:03 datomic-code-95186d6e-15ef-4eb1-b21e-b2e0b6ddc9da
2021-07-06 00:36:12 db-example-storage-s3datomic-1f6s5g2cqoyz3
2021-02-05 14:40:26 db-learn-storagef7f305e7-1y046qjbmg9zd-s3datomic-12oxiz7jr5jnl")

(defvar-local aws-test-example-ls-s3-output
      '(("2021-12-20" "15:02:04" "cognitect-dev-local") ("2022-05-16" "19:42:42" "compose-segment-db-backup") ("2021-04-13" "14:23:30" "datomic-code-32068047-eb82-4f94-914b-af1f8ce6144e") ("2021-02-05" "14:44:03" "datomic-code-95186d6e-15ef-4eb1-b21e-b2e0b6ddc9da") ("2021-07-06" "00:36:12" "db-example-storage-s3datomic-1f6s5g2cqoyz3") ("2021-02-05" "14:40:26" "db-learn-storagef7f305e7-1y046qjbmg9zd-s3datomic-12oxiz7jr5jnl")))

(describe
    (it "check list formatting for aws s3 ls"
      (expect (aws--s3-ls-format aws-test-example-ls) :to-equal aws-test-example-ls-s3-output)))


(provide 'test-aws)
;;; test-aws.el ends here
