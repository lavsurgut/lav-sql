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
(require 'async)

(defun lav-sql-generate-command (sql)
  (concat "aws redshift-data execute-statement \
            --cluster-identifier x \
            --database y \
            --sql \"" sql "\""))

(defun lav-sql-execute-async (sql)
  (let* ((stdout (get-buffer-create "*aws-redshift-data-stdout*"))
         (stderr (get-buffer-create "*aws-redshift-data-stderr*"))
         (process (make-process :name "cmd-process"
                                :command '(
                                           "aws"
                                           "redshift-data"
                                           "execute-statement"
                                           "--cluster-identifier"
                                           "onefootball-dwh"
                                           "--database"
                                           "onefootball"
                                           "--sql"
                                           "select 1"
                                           )
                                :buffer stdout
                                :stderr stderr))
         (stderr-process (get-buffer-process stderr)))
    (unless (and process stderr-process)
      (error "Process unexpectedly nil"))
    (message "Starting sql execution...")
    (set-process-sentinel process
                          (lambda (process event)
                            (message "Process %s %s. Check out the *aws-redshift-data-stderr* buffer." process event)))
    ;; (let ((exit-code (process-exit-status process)))
    ;;   (if (not (eq exit-code 0))
    ;;       (error "Process failed with exit code - %d" exit-code)
    ;;     (message "Starting sql execution...done")))
    )
  )

;;;###autoload
(defun lav-sql-execute ()
  "Execute SQL."
  (interactive)
  (lav-sql-execute-async "select 1"))


(provide 'lav-sql)
;;; lav-sql.el ends here
