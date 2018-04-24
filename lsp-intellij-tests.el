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
              (message "TEAMCITY: %s" (prin1-to-string teamcity-message))
              (let ((line (buffer-substring (point-min) line-end)))
                (message "OTHER: %s" line)))
            (delete-region (point-min) line-end)))))))


(defun lsp-intellij--read-teamcity-message ()
  "Reads a teamcity message of the format ##teamcity[<contents>].
Advances point just past the message."
  (when (looking-at "##teamcity\\[\\([a-zA-Z]+\\)\\(.*\\)]")
    (let* ((message-name (match-string-no-properties 1))
          (end (match-end 0))
          (args (lsp-intellij--parse-teamcity-args (match-string-no-properties 2))))
      (goto-char end)
      (list message-name args))))

(defun lsp-intellij--parse-teamcity-args (args)
  (let ((pos 0)
        matches)
    (while (string-match " \\(.*\\) = '\\(.*\\)'" args pos)
      (let* ((arg-name (match-string 1 args))
            (arg-value (match-string 2 args))
            (item (cons arg-name arg-value)))
        (push item matches)
        (setq pos (match-end 0))))
    matches))

