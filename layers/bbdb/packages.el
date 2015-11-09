;;; packages.el --- bbdb Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq bbdb-packages
    '(
      ;; package names go here
      bbdb
      (bbdb :location elpa)
      ))

;; List of packages to exclude.
(setq bbdb-excluded-packages '())

;; For each package, define a function bbdb/init-<package-name>
;;
(defun bbdb/init-bbdb ()
  "Initialize my package"
  (use-package bbdb)
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
