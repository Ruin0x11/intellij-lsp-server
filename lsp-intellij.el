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
(defvar lsp-intellij--progress-state (make-hash-table))

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
        (error "No root found"))
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
  (save-some-buffers t nil)
  (if-let ((config (lsp-intellij--choose-run-configuration)))
      (lsp-intellij--do-run-project config)
     (message "No run configurations were found.")))

(defun lsp-intellij--get-run-command (config)
  "Gets the run command for a given RunConfigurationDescription."
  (lsp--send-request
   (lsp--make-request
    "idea/runProject"
    (list :textDocument (lsp-text-document-identifier)
          :id (gethash "id" config)))))

(defun lsp-intellij--do-run-project (config)
  (let ((command (lsp-intellij--get-run-command config)))
         (cond
          ((or (not command) (not (gethash "command" command)))
           (error "Run configuration unsupported: %s" (gethash "name" config)))

          ((not (gethash "isUpToDate" command))
           (progn
             (setq lsp-intellij--run-after-build-command command)
             (lsp-intellij--do-build-project config)))

          (t (lsp-intellij--run-project-command command))))
  )
(defun lsp-intellij--run-project-command (command)
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
  (save-some-buffers t nil)
  (if-let ((config (lsp-intellij--choose-run-configuration)))
      (lsp-intellij--do-build-project config)
    (message "No run configurations were found.")))

(defun lsp-intellij--do-build-project (config)
  (let ((buffer (get-buffer-create "*lsp-intellij-build-output*"))
        (command (lsp--send-request
                  (lsp--make-request
                   "idea/buildProject"
                   (list :textDocument (lsp-text-document-identifier)
                         :id (gethash "id" config)
                         :forceMakeProject nil
                         :ignoreErrors nil)))))
    (with-current-buffer buffer
      (erase-buffer))
    (if (not (gethash "started" command))
        (error "Build failed to start")
      (progn
        (lsp-intellij--set-progress-state "building" t)
        (message "Build started.")))))

(defun lsp-intellij-run-at-point ()
  "Run the item (main class, unit test) at point."
  (interactive)
  (unless (lsp-intellij--run-project-from-code-lens
           (lsp-intellij--most-local-code-lens))
    (user-error "No configurations at point")))

(defun lsp-intellij-run-buffer-class ()
  "Run the configuration for the buffer's class.

This will run all tests if the class is a test class."
  (interactive)
  (unless (lsp-intellij--run-project-from-code-lens
           (lsp-intellij--run-buffer-lens))
    (user-error "No configurations for running buffer")))

(defun lsp-intellij--run-project-from-code-lens (lens)
  (when lens
    (let* ((data (gethash "data" lens))
           (config (gethash "configuration" data)))
      (lsp-intellij--do-run-project config))))

(defun lsp-intellij--run-buffer-lens ()
  (cl-find-if (lambda (lens)
             (let* ((data (gethash "data" lens))
                    (state (gethash "state" data)))
               (= state 1))) ;; RunClass
           lsp-code-lenses))

(defun lsp-intellij--min-by (f coll)
  (when (listp coll)
    (cl-reduce (lambda (min this)
                (if (> (funcall f min) (funcall f this)) this min))
            coll)))

(defun lsp-intellij--code-lenses-at-point ()
  "Gets the code lenses under the current point."
  (seq-filter (lambda (lens)
                (let* ((range (gethash "range" lens))
                       (start (lsp--position-to-point (gethash "start" range)))
                       (end (lsp--position-to-point (gethash "end" range))))
                  (lsp--point-is-within-bounds-p start end)))
              lsp-code-lenses))

(defun lsp-intellij--most-local-code-lens ()
  "Finds the code lens with the smallest range at point."
  (lsp-intellij--min-by (lambda (lens)
             (let* ((range (gethash "range" lens))
                    (start (lsp--position-to-point (gethash "start" range)))
                    (end (lsp--position-to-point (gethash "end" range))))
               (- end start)))
                        (lsp-intellij--code-lenses-at-point)))

(defvar lsp-intellij--code-lens-overlays (make-hash-table :test 'eq))

(defun lsp-intellij--remove-cur-code-lens-overlays ()
  (let ((overlays lsp-intellij--code-lens-overlays)
        (buf (current-buffer)))
    (dolist (overlay (gethash buf overlays))
      (delete-overlay overlay))
    (remhash buf overlays)))

(defconst lsp-intellij--code-lens-kind-face
  '((0 . lsp-intellij-face-code-lens-run)
    (2 . lsp-intellij-face-code-lens-test)
    (3 . lsp-intellij-face-code-lens-test-pass)
    (4 . lsp-intellij-face-code-lens-test-fail)
    (5 . lsp-intellij-face-code-lens-test-unknown)))

(defun lsp-intellij--render-code-lenses (lenses)
  "Create a callback to process a code lenses response LENSES."
  (let ((buf (current-buffer)))
    (cl-check-type buf buffer)
    (with-current-buffer buf
      (lsp-intellij--remove-cur-code-lens-overlays)
      (when (and lenses (/= (length lenses) 0))
        (let* ((overlays lsp-intellij--code-lens-overlays)
               (buf-overlays (gethash (current-buffer) overlays)))
          (save-restriction
            (widen)
            (dolist (lens lenses)
              (let* ((range (gethash "range" lens nil))
                     (data (gethash "data" lens))
                     (state (gethash "state" data 0))
                     (start (gethash "start" range))
                     (end (gethash "end" range))
                     overlay)
                (when (not (= state 1)) ;; not RunClass
                  (setq overlay (make-overlay (lsp--position-to-point start)
                                              (lsp--position-to-point end)))
                  (overlay-put overlay 'face
                               (cdr (assq state lsp-intellij--code-lens-kind-face)))
                  (push overlay buf-overlays)
                  (puthash (current-buffer) buf-overlays overlays))))))))))

(defun lsp-intellij--on-build-messages (workspace params)
  (let ((buffer (get-buffer-create "*lsp-intellij-build-output*")))
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
        (mapc (lambda (mes)
                (let ((path (if (string-blank-p (gethash "uri" mes))
                                "<unknown>"
                              (lsp--uri-to-path (gethash "uri" mes))))
                      (diags (gethash "diagnostics" mes)))
                  (lsp-intellij--insert-build-messages path diags)))
              params))
      (compilation-shell-minor-mode t))))

(defun lsp-intellij--insert-build-messages (path diags)
  (mapc (lambda (d) (lsp-intellij--insert-build-message path (lsp--make-diag d))) diags))

(defun lsp-intellij--insert-build-message (path diag)
  (let ((line (lsp-diagnostic-line diag))
        (column (lsp-diagnostic-column diag))
        (severity
         (pcase (lsp-diagnostic-severity diag)
           (1 'error)
           (2 'warning)
           (_ 'info)))
        (message (lsp-diagnostic-message diag))
        (source (lsp-diagnostic-source diag)))
    (goto-char (point-max))
    ;; use GCC's line format
    (insert (format "%s:%s:%s: %s: %s\n" path line column severity message))))

(defun lsp-intellij--on-build-finished (workspace params)
  (lsp-intellij--set-progress-state "building" nil)
  (let ((errors (gethash "errors" params))
        (warnings (gethash "warnings" params))
        (is-aborted (gethash "isAborted" params))
        (command lsp-intellij--run-after-build-command))
    (setq lsp-intellij--run-after-build-command nil)
    (cond
     ((> errors 0)
      (progn
        (message "Build failed with %s errors and %s warnings." errors warnings)
        (pop-to-buffer (get-buffer-create "*lsp-intellij-build-output*") 'other-window)
        (goto-char (point-min))))

     (is-aborted (message "Build was aborted."))

     (t (progn
          (message "Build finished with %s warnings." warnings)
          (when command
            (lsp-intellij--run-project-command command)))))))

(defun lsp-intellij-open-project-structure ()
  "Open the Project Structure dialog for the current project."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command "openProjectStructure" nil))

(defun lsp-intellij-open-run-configurations ()
  "Open the Run/Debug Configurations dialog for the current project."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command "openRunConfigurations" nil))

(defun lsp-intellij-toggle-frame-visibility ()
  "Toggle visibility of the current project's frame."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command "toggleFrameVisibility" nil))

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
       (lsp-intellij--set-progress-state "indexing" t)))
    ("idea/indexFinished" .
     (lambda (_w _p)
       (message "Indexing finished.")
       (lsp-intellij--set-progress-state "indexing" nil)))
    ("idea/buildFinished" .
     (lambda (w p)
       (lsp-intellij--on-build-finished w p)))
    ("idea/buildMessages" .
     (lambda (w p)
       (lsp-intellij--on-build-messages w p)))))

(defun lsp-intellij--set-progress-state (key value)
  (if value
      (puthash key value lsp-intellij--progress-state)
    (remhash key lsp-intellij--progress-state))
  (let ((result))
    (maphash
     (lambda (k v)
       (setq result (if result (concat result " " k) k)))
     lsp-intellij--progress-state)
    (setq lsp-status (format "(%s)" result))))

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

(add-hook 'lsp-after-diagnostics-hook (lambda () (lsp--update-code-lenses 'lsp-intellij--render-code-lenses)))


(defun lsp-intellij-set-config (name option)
  "Set a config option in the intellij lsp server."
  (puthash name option lsp-intellij--config-options))

(defun lsp-intellij-set-temporary-directory (dir)
  "Set the temporary directory for extracted jar files."
  (lsp-intellij-set-config "temporaryDirectory" dir))

(lsp-intellij-set-temporary-directory (lsp--path-to-uri temporary-file-directory))

;;;###autoload
(defface lsp-intellij-face-code-lens-run
  '((((background dark))  :background "dark green")
    (((background light)) :background "green"))
  "Face used for areas with a run configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test
  '((((background dark))  :background "saddle brown")
    (((background light)) :background "yellow"))
  "Face used for areas with a test configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test-pass
  '((((background dark))  :background "sea green")
    (((background light)) :background "green"))
  "Face used for areas with a passing test configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test-fail
  '((((background dark))  :background "firebrick")
    (((background light)) :background "red"))
  "Face used for areas with a failing test configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test-unknown
  '((((background dark))  :background "saddle brown")
    (((background light)) :background "yellow"))
  "Face used for areas with a test configuration an with unknown state."
  :group 'lsp-intellij-faces)

(provide 'lsp-intellij)
;;; lsp-intellij.el ends here
