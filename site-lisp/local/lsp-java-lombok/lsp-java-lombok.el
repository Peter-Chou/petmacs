;;; lsp-java-lombok.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Kevin Ziegler
;;
;; Author: Kevin Ziegler <https://github.com/kevinziegler>
;; Maintainer: Peter Chou <https://github.com/Peter-Chou>
;; Created: February 12, 2021
;; Modified: Tuesday 09, 2023
;; Version: 0.1.0
;; Homepage: https://github.com/kevinziegler/lsp-java-lombok
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Helper library for setting up Lombok with LSP-java
;;
;;; Code:
(require 'lsp-java)

(defvar lsp-java-lombok/enabled nil
  "Indicates the LSP server should be started with Lombok.")

(defvar lsp-java-lombok/version nil
  "When non-nil, use the specified Lombok version, otherwise use the latest.")

(defvar lsp-java-lombok/jar-url-base "https://projectlombok.org/downloads/"
  "The base path to download Lombok jars from.")

(defvar lsp-java-lombok/dir user-emacs-directory
  "The path on disk where lombok jars are saved.")

(defun lsp-java-lombok/jar-file ()
  "Get the filename for the Lombok jar."
  (concat "lombok"
          (when lsp-java-lombok/version "-")
          lsp-java-lombok/version
          ".jar"))

(defun lsp-java-lombok/jar-path ()
  "Generate the path on disk for the Lombok jar."
  (concat lsp-java-lombok/dir (lsp-java-lombok/jar-file)))

(defun lsp-java-lombok/download-jar ()
  "Download the latest lombok jar for use with LSP."
  (let* ((lombok-url (url-generic-parse-url lsp-java-lombok/jar-url-base))
         (base-path (file-name-as-directory (url-filename lombok-url)))
         (file-path (concat base-path (lsp-java-lombok/jar-file))))
    (setf (url-filename lombok-url) file-path)
    (url-copy-file lombok-url (lsp-java-lombok/jar-path))))

(defun lsp-java-lombok/append-vmargs ()
  "Apply lombok args to lsp-java-vmargs."
  (setq lsp-java-vmargs
        (append
         lsp-java-vmargs
         (list (concat "-javaagent:" (expand-file-name (lsp-java-lombok/jar-path)))))))

(defun lsp-java-lombok/setup ()
  "Download Lombok if it hasn't been downloaded already."
  (when (not (file-exists-p (lsp-java-lombok/jar-path)))
    (message "Could not find lombok for lsp-java.  Downloading...")
    (lsp-java-lombok/download-jar)))

;;;###autoload
(defun lsp-java-lombok/init ()
  (interactive)
  "Initialize lsp-java-lombok."
  (when lsp-java-lombok/enabled
    (lsp-java-lombok/setup)
    (lsp-java-lombok/append-vmargs)))

(provide 'lsp-java-lombok)
;;; lsp-java-lombok.el ends here
