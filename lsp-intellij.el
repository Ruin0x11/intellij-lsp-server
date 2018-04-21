;;; lsp-intellij.el --- intellij lsp client                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  Ruin0x11
;; Keywords: java
;; Package-Requires: ((emacs "25.1") (lsp-mode "4.1"))

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

(defvar lsp-intellij--config-options (make-hash-table))

(defvar lsp-intellij-use-topmost-maven-root t
  "If non-nil, `lsp-intellij' will attempt to locate the topmost
Maven project in a nested hierarchy if a Maven subproject is opened
and set the LSP project root to it. Otherwise, `lsp-intellij' will set
the project root to be the root of the Maven subproject.")

;; copied from projectile to avoid a dependency
(defun lsp-intellij--parent (path)
  "Return the parent directory of PATH.
PATH may be a file or directory and directory paths may end with a slash."
  (directory-file-name (file-name-directory (directory-file-name (expand-file-name path)))))

;; copied from projectile to avoid a dependency
(defun lsp-intellij--root-top-down-recurring (dir list)
  "Identify a project root in DIR by recurring top-down search for files in LIST.
Return the last (bottommost) matched directory in the topmost sequence
of matched directories.  Nil otherwise."
  (cl-some
   (lambda (f)
     (locate-dominating-file
      dir
      (lambda (dir)
        (and (file-exists-p (expand-file-name f dir))
             (or (string-match locate-dominating-stop-dir-regexp (lsp-intellij--parent dir))
                 (not (file-exists-p (expand-file-name f (lsp-intellij--parent dir)))))))))
   list))

(defun lsp-intellij--any-value-in-hash (hash-table)
  (block nil
    (maphash (lambda (k v) (return v))
             hash-table)))

(defun lsp-intellij--is-extracted-jar-file (file-name)
  (or
   ;; extracted from archive-mode (prevents error when LSP mode hooks run)
   (and (string-match-p "\.jar:[a-zA-Z]+" file-name) (not (file-exists-p file-name)))
   ;; extracted using lsp-intellij
   (string-match-p (regexp-quote temporary-file-directory) file-name)))

(defun lsp-intellij--get-root ()
  (if (lsp-intellij--is-extracted-jar-file (buffer-file-name))
      (lsp--workspace-root (lsp-intellij--any-value-in-hash lsp--workspaces))
    (let ((file (locate-dominating-file (buffer-file-name)
                                        (lambda (parent)
                                          (when (directory-name-p parent)
                                            (directory-files parent nil ".*.iml"))))))
      (when (not file)
        (error "No root found."))
      (let* ((pom (directory-files (file-name-directory file) nil "pom.xml"))
             (has-pom (> (length pom) 0))
             (root (if (and has-pom lsp-intellij-use-topmost-maven-root)
                       (lsp-intellij--root-top-down-recurring file '("pom.xml"))
                     file)))
        (file-name-directory root)))))

(defun lsp-intellij--make-jar-temp-path (jar-path internal-path)
  "Return a temporary path for the file in the jar at JAR-PATH, INTERNAL-PATH, to be extracted to."
  (let* ((jar-file-name (file-name-base jar-path))
         (temp-path (concat temporary-file-directory "lsp-intellij/" jar-file-name "/")))
    temp-path))

(defun lsp-intellij--write-jar-metadata (archive-path dest)
  "Write a metadata file that points to jar file ARCHIVE-PATH at DEST.

Used for allowing IntelliJ to find the actual jar an extracted jar file is contained in."
  (with-temp-buffer
    (insert (lsp--path-to-uri archive-path))
    (write-file (concat dest "jarpath") nil)))

(defun lsp-intellij--extract-archive-file (source-archive original-archive internal-path dest)
  "Extracts the file inside a jar SOURCE-ARCHIVE at INTERNAL-PATH to DEST.

Also writes the location of ORIGINAL-ARCHIVE, containing the compiled classes, so IntelliJ can find it."
  (let* ((internal-dir (substring (file-name-directory internal-path) 1))
         (internal-file (file-name-nondirectory internal-path))
         (internal-name (file-name-sans-extension internal-file))
         (search-string (concat internal-dir internal-name)))
    (save-window-excursion
      (find-file source-archive)
      (let ((archive-buffer (current-buffer)))
        (goto-char (point-min))
        (re-search-forward search-string)
        (archive-extract)
        (let ((extract-buffer (current-buffer))
              (outpath (concat dest internal-dir (file-name-nondirectory (buffer-file-name)))))
          (mkdir (file-name-directory outpath) t)
          (lsp-intellij--write-jar-metadata original-archive dest)
          (write-file outpath nil)
          (kill-buffer archive-buffer)
          (kill-buffer extract-buffer)
          outpath)))))

(defconst lsp-intellij--file-extracted-from-jar-regex
  "\\\.\\(java\\|kt\\|scala\\|xml\\|MF\\)$")

(defun lsp-intellij--extracted-file-exists (basename temp-path)
  "Test if a file with name BASENAME at TEMP-PATH has been extracted from a jar.

Used for finding the corresponding .java/.kt file from a jar's .class file.
Return the file path if found, nil otherwise."
  (and (file-exists-p temp-path)
       (cl-find-if (lambda (s) (and (eq (file-name-sans-extension s) basename)
                                    (string-match-p lsp-intellij--file-extracted-from-jar-regex s)))
                   (directory-files temp-path))))

(defun lsp-intellij--visit-jar-uri (uri)
  "Visit a URI with the jar:// protocol by extracting the file from the jar and visiting it."
  (let* ((url (url-generic-parse-url uri))
         (drive-letter (url-host url))
         (raw (url-filename url))
         (paths (split-string raw "!"))
         (jar-path (concat drive-letter ":" (car paths)))
         (internal-path (cadr paths))
         (sources-jar-path (replace-regexp-in-string "\.jar$" "-sources.jar" jar-path))
         (jar-to-extract (if (file-exists-p sources-jar-path) sources-jar-path jar-path))
         (temp-path (lsp-intellij--make-jar-temp-path jar-to-extract internal-path)))

    ;; For now, just error if no sources are found.
    ;; In the future we could request IntelliJ to give us the decompiled .class file source.
    (if (file-exists-p sources-jar-path)
        (if-let ((existing-file
                  (lsp-intellij--extracted-file-exists
                   (file-name-base internal-path)
                   temp-path)))
            existing-file
          (lsp-intellij--extract-archive-file
           jar-to-extract jar-to-extract internal-path temp-path))
      (error "No sources found for file in JAR: %s" uri))))

(defun lsp-intellij-find-implementations ()
  "List all implementations for the Java element at point."
  (interactive)
  (let* ((impls (lsp--send-request (lsp--make-request
                                    "idea/implementations"
                                    (lsp--text-document-position-params))))
         (items (lsp--locations-to-xref-items impls)))
    (if items
        (xref--show-xrefs items nil)
      (message "No implementations found for: %s" (thing-at-point 'symbol t)))))

(defun lsp-intellij--project-run-configurations ()
  "Get the list of project run configurations."
  (lsp--send-request (lsp--make-request
                      "idea/runConfigurations"
                      (lsp--text-document-position-params))))

(defun lsp-intellij--run-config-to-name (config)
  (format "[%s] %s" (gethash "configType" config) (gethash "name" config)))

(defun lsp-intellij--choose-run-configuration ()
  (when-let ((configs (lsp-intellij--project-run-configurations)))
    (let* ((display-names (mapcar #'lsp-intellij--run-config-to-name configs))
           (completions (mapcar* #'cons display-names configs))
           (chosen (cdr (assoc
                         (completing-read "Run configuration: " completions)
                         completions))))
      chosen)))

(defun lsp-intellij-run-project ()
  "Run a project using an IntelliJ run configuration."
  (interactive)
  (if-let ((config (lsp-intellij--choose-run-configuration)))
       (let ((command (lsp--send-request
                       (lsp--make-request
                        "idea/runProject"
                        (list :textDocument (lsp-text-document-identifier)
                              :id (gethash "id" config))))))

         (cond
          ((not (gethash "command" command))
           (error "Run configuration unsupported: %s" (gethash "name" config)))

          ((gethash "needsRebuild" command)
           (progn
             (setq lsp-intellij--run-after-build-command command)
             (lsp-intellij--do-build-project config)))

          (t (lsp-intellij--do-run-project command))))
     (message "No run configurations were found.")))

(defun lsp-intellij--do-run-project (command)
  (let ((default-directory (gethash "workingDirectory" command))
        (command-str (replace-regexp-in-string "\n" " "
                                               (gethash "command" command))))
    (setenv "CLASSPATH" (gethash "classpath" command))
    (compile command-str)))

;; TODO: make a hash listing a build per config
(defvar lsp-intellij--run-after-build-command nil
  "Run configuration to run after the current build finishes")

(defun lsp-intellij-build-project ()
  "Start building a project using an IntelliJ run configuration."
  (interactive)
  (if-let ((config (lsp-intellij--choose-run-configuration)))
      (lsp-intellij--do-build-project config)
    (message "No run configurations were found.")))

(defun lsp-intellij--do-build-project (config)
  (let ((command (lsp--send-request
                  (lsp--make-request
                   "idea/buildProject"
                   (list :textDocument (lsp-text-document-identifier)
                         :id (gethash "id" config)
                         :forceMakeProject nil
                         :ignoreErrors nil)))))
    (if (not (gethash "started" command))
        (error "Build failed to start")
      (message "Build started."))))

(defun lsp-intellij--on-build-finished (workspace params)
  (let ((errors (gethash "errors" params))
        (warnings (gethash "warnings" params))
        (is-aborted (gethash "isAborted" params)))
    (cond
     ((> errors 0) (message "Build failed with %s errors and %s warnings." errors warnings))
     (is-aborted (message "Build was aborted."))
     (t (progn
          (message "Build finished with %s warnings." warnings)
          (when-let ((command lsp-intellij--run-after-build-command))
            (lsp-intellij--do-run-project command)
            (setq lsp-intellij--run-after-build-command nil))))
     )))

(defun lsp-intellij-open-project-structure ()
  "Open the Project Structure dialog for the current project."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request
   (lsp--make-request
    "idea/openProjectStructure"
    (lsp--text-document-position-params))
   t))

(defun lsp-intellij-toggle-frame-visibility ()
  "Toggle visibility of the current project's frame."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request
   (lsp--make-request
    "idea/toggleFrameVisibility"
    (lsp--text-document-position-params))
   t))

(defun lsp-intellij--render-string (str mode)
  (condition-case nil
      (with-temp-buffer
        (delay-mode-hooks (funcall mode))
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

(defconst lsp-intellij--notification-handlers
  '(("idea/indexStarted" .
     (lambda (_w _p)
       (message "Indexing started.")
       (setq lsp-status "(indexing)")))
    ("idea/indexFinished" .
     (lambda (_w _p)
       (message "Indexing finished.")
       (setq lsp-status nil)))
    ("idea/buildFinished" .
     (lambda (w p)
       (lsp-intellij--on-build-finished w p)))))

(defconst lsp-intellij--request-handlers
  '(("idea/temporaryDirectory" .
     (lambda (_w _p)
       (list :directory (lsp--path-to-uri temporary-file-directory))))))

(defun lsp-intellij--initialize-client (client)
  (mapcar #'(lambda (p) (lsp-client-on-notification client (car p) (cdr p)))
          lsp-intellij--notification-handlers)
  (mapcar #'(lambda (p) (lsp-client-on-request client (car p) (cdr p)))
          lsp-intellij--request-handlers)

  ;; Emacs strips out the \r in \r\n by default, even with lsp-mode,
  ;; so the proper coding system needs to be set to capture the \r\n.
  (setq-local default-process-coding-system (cons 'utf-8 'utf-8))
  (setq-local coding-system-for-read 'binary)
  (setq-local coding-system-for-write 'binary)
  ;; Ensure the client uses the server's sync method
  (setq-local lsp-document-sync-method nil)

  (lsp-provide-marked-string-renderer client "java" (lambda (s) (lsp-intellij--render-string s 'java-mode)))
  (lsp-provide-marked-string-renderer client "kotlin" (lambda (s) (lsp-intellij--render-string s 'kotlin-mode)))
  (lsp-client-register-uri-handler client "jar" 'lsp-intellij--visit-jar-uri))

(lsp-define-tcp-client lsp-intellij "intellij" #'lsp-intellij--get-root lsp-intellij-dummy-executable
                       "127.0.0.1" 8080
                       :initialize #'lsp-intellij--initialize-client)

(defun lsp-intellij--set-configuration ()
  (lsp--set-configuration `(:intellij ,lsp-intellij--config-options)))

(add-hook 'lsp-after-initialize-hook 'lsp-intellij--set-configuration)

(defun lsp-intellij-set-config (name option)
  "Set a config option in the intellij lsp server."
  (puthash name option lsp-intellij--config-options))

(defun lsp-intellij-set-temporary-directory (dir)
  "Set the temporary directory for extracted jar files."
  (lsp-intellij-set-config "temporaryDirectory" dir))

(lsp-intellij-set-temporary-directory (lsp--path-to-uri temporary-file-directory))

(provide 'lsp-intellij)
;;; lsp-intellij.el ends here
