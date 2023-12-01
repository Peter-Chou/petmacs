;;; flycheck-python-ruff.el --- Flycheck checker for python ruff linter -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sergey Morozov

;; Author: Sergey Morozov <nibbler@nibbler.dev>
;; Keywords: convenience, tools, flycheck, python, ruff
;; URL: https://github.com/v4n6/flycheck-python-ruff
;; Version: 0.0.5
;; Package-Requires: ((emacs "25.1") (let-alist "1.0.4") (flycheck "0.25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck checker for python ruff linter that supports error explanations.
;;
;; Usage:
;;
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-python-ruff-setup))

;;; Code:

(require 'flycheck)


(defun flycheck-python-ruff-parse (output checker buffer)
  "Parse JSON OUTPUT of CHECKER on BUFFER as python ruff errors."
  (mapcar (lambda (err)
            (let-alist err
              (let ((line (cdr (assq 'row .location)))
                    (col (cdr (assq 'column .location)))
                    (end-line (cdr (assq 'row .end_location)))
                    (end-col (cdr (assq 'column .end_location))))

                (flycheck-error-new
                 :line (and line (max line 1))
                 :column (and col (max col 1))
                 :end-line (and end-line (max end-line 1))
                 :end-column (and end-col (max end-col 1))
                 :buffer buffer
                 :checker checker
                 :filename .filename
                 :message (concat .code
                                  ": "
                                  (and (string-match (rx (*? nonl) eol) .message)
                                       (match-string 0 .message)))
                 :level 'error
                 :id .url))))

          (car (flycheck-parse-json output))))


(flycheck-def-config-file-var
    flycheck-ruff-conffile python-ruff
    '("pyproject.toml" "ruff.toml" ".ruff.toml"))


(flycheck-define-checker python-ruff
  "A Python syntax and style checker using ruff.

This syntax checker requires ruff 0.1.3 or newer.

See URL `https://docs.astral.sh/ruff/'."

  :command ("python3"
            (eval (flycheck-python-module-args 'python-ruff "ruff"))
            "--output-format=json"
            ;; Need `source-inplace' for relative imports (e.g. `from .foo
            ;; import bar')
            source-inplace)
  :working-directory flycheck-python-find-project-root
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-ruff))
                 (flycheck-python-find-module 'python-ruff "ruff")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-ruff "ruff"))

  :error-parser flycheck-python-ruff-parse
  :error-explainer (lambda (err)
                     `(url . ,(flycheck-error-id err)))
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))


;;;###autoload
(defun flycheck-python-ruff-setup ()
  "Setup Flycheck python ruff.
Add `python-ruff' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'python-ruff))

(provide 'flycheck-python-ruff)
;;; flycheck-python-ruff.el ends here
