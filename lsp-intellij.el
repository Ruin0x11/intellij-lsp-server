;;; lsp-intellij.el --- intellij lsp client                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  Ruin0x11
;; Keywords: java
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.0"))

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

;; lsp-mode client for intellij-lsp-server.
;; After installing lsp-mode, you can use it as follows:

;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-intellij)
;;   (add-hook 'java-mode-hook #'lsp-intellij-enable))

;; Then, after opening and configuring a project in your instance of
;; IDEA that has intellij-lsp-server, navigate to a Java file tracked
;; by that project.

;;; Code:

(require 'lsp-mode)
(require 'cl)

(defun lsp-intellij--get-root ()
  (let ((file (locate-dominating-file (buffer-file-name)
                                      (lambda (parent)
                                        (when (directory-name-p parent)
                                          (directory-files parent nil ".*.iml"))))))
    (when (not file)
      (error "No root found."))
    (file-name-directory file)))

(defun lsp-intellij--locations-to-xref-items (locations)
  "Return a list of `xref-item' from LOCATIONS, except for those inside JARs."
  (let* ((filtered-locs (seq-filter
                         (lambda (loc)
                           (string-prefix-p "file" (gethash "uri" loc)))
                         locations))
         (fn (lambda (loc) (lsp--uri-to-path (gethash "uri" loc))))
         ;; locations-by-file is an alist of the form
         ;; ((FILENAME . LOCATIONS)...), where FILENAME is a string of the
         ;; actual file name, and LOCATIONS is a list of Location objects
         ;; pointing to Ranges inside that file.
         (locations-by-file (seq-group-by fn filtered-locs))
         (items-by-file (mapcar #'lsp--get-xrefs-in-file locations-by-file)))
    (apply #'append items-by-file)))

(defun lsp-intellij-find-implementations ()
  "List all implementations for the Java element at point."
  (interactive)
  (let* ((impls (lsp--send-request (lsp--make-request
                                    "idea/implementations"
                                    (lsp--text-document-position-params))))
         (items (lsp-intellij--locations-to-xref-items impls)))
    (if items
        (xref--show-xrefs items nil)
      (message "No implementations found for: %s" (thing-at-point 'symbol t)))))

(defun lsp-intellij-open-project-structure ()
  "Opens the Project Structure dialogue for the current project."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request
   (lsp--make-request
    "idea/openProjectStructure"
    (lsp--text-document-position-params))
   t))

(defun lsp-intellij-toggle-frame-visibility ()
  "Toggles visibility of the current project's frame."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request
   (lsp--make-request
    "idea/toggleFrameVisibility"
    (lsp--text-document-position-params))
   t))

(defun lsp-intellij--render-string (str)
  (condition-case nil
      (with-temp-buffer
	(delay-mode-hooks (java-mode))
	(insert str)
	(font-lock-ensure)
	(buffer-string))
    (error str)))

(defconst lsp-intellij-dummy-executable
  (if (eq system-type 'windows-nt)
      '("cmd")
    '("sh"))
  "Program that lsp-mode will open when initializing lsp-intellij.

lsp-mode requires a process to be opened when starting a server over
TCP, even if it isn't the one being communicated with.")

(defconst lsp-intellij--handlers
  '(("idea/indexStarted" .
     (lambda (_w _p)
       (setq lsp-status "(indexing)")))
    ("idea/indexFinished" .
     (lambda (_w _p)
       (setq lsp-status nil)))))

(defun lsp-intellij--initialize-client (client)
  (mapcar #'(lambda (p) (lsp-client-on-notification client (car p) (cdr p)))
	  lsp-intellij--handlers)
  ;; Emacs strips out the \r in \r\n by default, even with lsp-mode,
  ;; so the proper coding system needs to be set to capture the \r\n.
  (setq-local default-process-coding-system (cons 'utf-8 'utf-8))
  (setq-local coding-system-for-read 'binary)
  (setq-local coding-system-for-write 'binary)
  (lsp-provide-marked-string-renderer client "java" #'lsp-intellij--render-string))

(lsp-define-tcp-client lsp-intellij "intellij" #'lsp-intellij--get-root lsp-intellij-dummy-executable
                       "127.0.0.1" 8080
                       :initialize #'lsp-intellij--initialize-client)

(provide 'lsp-intellij)
;;; lsp-intellij.el ends here
