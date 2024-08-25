;;; lav-sql.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Valery Lavrentiev
;;
;; Author: Valery Lavrentiev <lavsurgut@gmail.com>
;; Maintainer: Valery Lavrentiev <lavsurgut@gmail.com>
;; Created: August 24, 2024
;; Modified: August 24, 2024
;; Version: 0.0.1
;; Keywords: sql aws redshift
;; Homepage: https://github.com/lavsurgut/lav-sql
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;; minor mode on sql
;; execute aws command
;;
(require 'compile)

(define-compilation-mode lav-sql-compilation-mode "Lav SQL"
  "Lav SQL compilation mode."
  (progn
    (set (make-local-variable 'lav-sql-dummy-var) t)))

(defun lav-sql-generate-command (sql)
  (concat "aws redshift-data execute-statement \
            --cluster-identifier x \
            --database y \
            --sql \"" sql "\""))


(defun lav-sql-run (sql)
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))
  (let ((command-to-run (lav-sql-generate-command sql)))
    (with-current-buffer (get-buffer-create "*lav-sql*")
      ;(setq default-directory root-dir)
      (compilation-start command-to-run 'lav-sql-compilation-mode))))


;;;###autoload
(defun lav-sql-execute ()
  "Execute SQL."
  (interactive)
  (lav-sql-run "select 1"))


(provide 'lav-sql)
;;; lav-sql.el ends here
