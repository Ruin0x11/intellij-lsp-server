;; TODO: see if a series of test notifications can be standardised on the LSP server, reducing the amount of work needed to implement the test running feature

(defun lsp-intellij--run-test-teamcity-proc ()
  (interactive)
  (let ((proc (start-process "teamcity test"
                             (get-buffer-create "*teamcity*")
                             "ruby" "E:/build/teamcity.rb"))
        (new-buffer (generate-new-buffer "*lsp-intellij test output*")))
    (set-process-filter proc #'lsp-intellij--test-process-filter)
    new-buffer))

(defun lsp-intellij--test-process-filter (process output)
  (let ((buffer (process-buffer process)))
    (when (and buffer
               (buffer-live-p buffer))

      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output)
        (while (progn
                 (goto-char (point-min))
                 (search-forward "\n" nil t))
          (let ((line-end (point))
                (teamcity-message nil)
                (did-read-teamcity nil))
            (goto-char (point-min))
            (condition-case _err
                (progn
                  (setq teamcity-message (lsp-intellij--read-teamcity-message))
                  (if teamcity-message
                      (setq line-end (1+ (point))
                            did-read-teamcity t)
                    (goto-char (point-min))))
              (error
               (goto-char (point-min))))
            (if did-read-teamcity
                (progn
                  (message "TEAMCITY: %s" (prin1-to-string teamcity-message))
                  (lsp-intellij--handle-teamcity-message teamcity-message))
              (let ((line (buffer-substring (point-min) line-end)))
                (message "OTHER: %s" line)))
            (delete-region (point-min) line-end)))))))

(defun lsp-intellij--read-teamcity-message ()
  "Reads a teamcity message of the format ##teamcity[<message>].
Advances point just past the message."
  (when (looking-at "##teamcity\\[\\([a-zA-Z]+\\)\\(.*\\)]")
    (let* ((message-name (match-string-no-properties 1))
          (end (match-end 0))
          (attrs (lsp-intellij--parse-teamcity-attrs (match-string-no-properties 2))))
      (goto-char end)
      (list message-name attrs))))

(defun lsp-intellij--parse-teamcity-attrs (attrs)
  (let ((pos 0)
        matches)
    (while (string-match " \\(.*\\) = '\\(.*\\)'" attrs pos)
      (let* ((attr-name (match-string 1 attrs))
            (attr-value (match-string 2 attrs))
            (item (cons attr-name attr-value)))
        (push item matches)
        (setq pos (match-end 0))))
    matches))

(defun lsp-intellij--handle-teamcity-message (teamcity-message)
  (let ((method-name (car teamcity-message))
        (attrs (cdr teamcity-message)))

    ))

(defconst lsp-intellij--teamcity-handlers
  #s(hash-table
     test equal
     data
     ("enteredTheMatrix"  nil)
     ("suiteTreeStarted"  nil)
     ("suiteTreeNode"     nil)
     ("suiteTreeEnded"    nil)
     ("treeEnded"         nil)
     ("rootName"          nil)
     ("testSuiteStarted"  nil)
     ("testSuiteFinished" nil)
     ("testStarted"       nil)
     ("testFinished"      nil)
     ("testIgnored"       nil)))

(cl-defstruct lsp-intellij--teamcity-data
  (suiteTree nil) ;; list representation of the test suite tree. car is root, cdr is children.
  ;; a subelement like (2 4 5 6) represents a tree with root 2 and children (4 5 6).
  ;; during list building there is no root (it is nil), but it is added on the rootName method.
  (elementStack '()) ;; stack of parent elements for the current test suite node.
  ;; pushed to on suiteTreeStarted, popped on suiteTreeEnded.

  )

;; TESTING_STARTED = "testingStarted";
;; TESTING_FINISHED = "testingFinished";
;; KEY_TESTS_COUNT = "testCount";
;; ATTR_KEY_TEST_ERROR = "error";
;; ATTR_KEY_TEST_COUNT = "count";
;; ATTR_KEY_TEST_DURATION = "duration";
;; ATTR_KEY_TEST_OUTPUT_FILE = "outputFile";
;; ATTR_KEY_LOCATION_URL = "locationHint";
;; ATTR_KEY_LOCATION_URL_OLD = "location";
;; ATTR_KEY_STACKTRACE_DETAILS = "details";
;; ATTR_KEY_DIAGNOSTIC = "diagnosticInfo";
;; ATTR_KEY_CONFIG = "config";

;; MESSAGE = "message";
;; TEST_REPORTER_ATTACHED = "enteredTheMatrix";
;; SUITE_TREE_STARTED = "suiteTreeStarted";
;; SUITE_TREE_ENDED = "suiteTreeEnded";
;; SUITE_TREE_NODE = "suiteTreeNode";
;; BUILD_TREE_ENDED_NODE = "treeEnded";
;; ROOT_PRESENTATION = "rootName";

;; ATTR_KEY_STATUS = "status";
;; ATTR_VALUE_STATUS_ERROR = "ERROR";
;; ATTR_VALUE_STATUS_WARNING = "WARNING";
;; ATTR_KEY_TEXT = "text";
;; ATTR_KEY_TEXT_ATTRIBUTES = "textAttributes";
;; ATTR_KEY_ERROR_DETAILS = "errorDetails";
;; ATTR_KEY_EXPECTED_FILE_PATH = "expectedFile";
;; ATTR_KEY_ACTUAL_FILE_PATH = "actualFile";

;; CUSTOM_STATUS = "customProgressStatus";
;; ATTR_KEY_TEST_TYPE = "type";
;; ATTR_KEY_TESTS_CATEGORY = "testsCategory";
;; ATTR_VAL_TEST_STARTED = "testStarted";
;; ATTR_VAL_TEST_FINISHED = "testFinished";
;; ATTR_VAL_TEST_FAILED = "testFailed";
