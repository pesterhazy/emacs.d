(defun my-pop-mark ()
  (interactive)
  (pop-mark))

(defun cider-reset-dev ()
  (interactive)
  (when (not (cider-current-connection))
    (cider-connect "localhost" (second (first (cider-locate-running-nrepl-ports)))))
  (cider-eval-buffer)
  (cider-interactive-eval
   "(when-let [r (resolve 'fy.api.repl/reset-dev-light)] (r))"))

(defun cider-force-connect ()
  (interactive)
  (let ((repl (cider-current-repl nil nil)))
    (when repl
      (cider-quit repl))
    (cider-connect '(:host "localhost" :port 48888))))

(defun cider-force-eval-buffer ()
  (interactive)
  (let ((repl (cider-current-repl nil nil)))
    (when (not repl)
      (cider-force-connect))
    (cider-eval-buffer)))

(defun make-current-buffer-exexcutable ()
  (interactive)
  (set-file-modes (buffer-file-name) #o755))

(defun iterm ()
  (interactive)
  (call-process-shell-command "open -a iTerm"))

(defun touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))

(defun eval-and-touch ()
  (interactive)
  (cider-eval-buffer)
  (touch))

(defun insert-random-uuid ()
  (interactive)
  (insert (shell-command-to-string "python -c 'import uuid, sys; sys.stdout.write(str(uuid.uuid4()))'")))


(defun insert-bar ()
  (interactive)
  (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"))

(defun insert-next ()
  (interactive)
  (sp-newline)
  (evil-insert nil))

(defun paste-comment ()
  (interactive)
  (insert (concat ";; " (replace-regexp-in-string "\n" "\n;; " (shell-command-to-string "pbpaste"))))
  )

(defun new-shell-script ()
  (interactive)
  (insert "#!/usr/bin/env bash\n\nset -euo pipefail\n\n")
  (save-buffer)
  (make-current-buffer-exexcutable)
  (shell-script-mode))

(defun new-figly-shell-script ()
  (interactive)
  (insert "#!/usr/bin/env bash\nset -euo pipefail && cd \"$(dirname \"${BASH_SOURCE[0]}\")/..\"\n\n")
  (save-buffer)
  (make-current-buffer-exexcutable)
  (shell-script-mode))

(defun find-primary-proj ()
  (interactive)
  (find-file "~/pitch/pitch-app/README.md"))

(defun find-work ()
  (interactive)
  (find-file "~/Dropbox/zettel/work.org"))

(defun run-current-shell-buffer ()
  (interactive)
  (shell-command (buffer-file-name)))

(defun again ()
  (interactive)
  (compile "again"))

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END.
Dependency:
npm i -g sql-formatter-cli"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sql-formatter-cli" nil t)))

(defun sql-beautify-buffer ()
  "Beautify SQL in buffer."
  (interactive)
  (sql-beautify-region (point-min) (point-max)))

(defun asdfcount ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "asdfcount" (buffer-name) t)))

(defun asdfbump ()
  (interactive)
  (save-excursion
    (asdfcount)
    (save-buffer)))
