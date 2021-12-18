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
  (if (string-equal major-mode "typescript-mode")
      (insert "// ********************************************************************\n")
    (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")))

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
  (find-file "~/pitch/pitch-app/desktop-app/src/app/pitch/app.cljs"))

(defun find-compose ()
  (interactive)
  (find-file "~/Dropbox/zettel/compose.txt"))
(defun find-compose2 ()
  (interactive)
  (find-file "~/Dropbox/zettel/compose2.txt"))
(defun find-1x1 ()
  (interactive)
  (find-file "~/Dropbox/zettel/1x1.txt"))
(defun find-reading ()
  (interactive)
  (find-file "~/Dropbox/zettel/reading.md"))
(defun find-writing ()
  (interactive)
  (find-file "~/Dropbox/zettel/writing.md"))
(defun find-scraps ()
  (interactive)
  (find-file "/Users/user/prg/cljs-scrap/src/main/scrap/scrap.cljs"))

(defun find-it ()
  (interactive)
  (find-file "~/arena/bin/it"))
(defun find-diary ()
  (interactive)
  (find-file "~/Dropbox/zettel/diary.md"))

(defun find-init-el ()
  (interactive)
  (find-file "~/emacs.d/init.el"))
(defun find-my-functions ()
  (interactive)
  (find-file "~/emacs.d/elisp/my-functions.el"))

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

(defun my-changes ()
  (interactive)
  (diff-buffer-with-file))

(defun em-dash ()
  (interactive)
  (insert "\u2014"))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iTerm

;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/

(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun iterm-open-new-tab ()
  (interactive)
  (do-applescript
   (format
    "
    tell application \"iTerm2\"
        activate
        tell current window
            create tab with default profile
            tell the current session
                write text \"cd %s\"
            end tell
        end tell
    end tell"
    (replace-regexp-in-string "\\\\" "\\\\\\\\"
                              (shell-quote-argument (or default-directory "~"))))))


(defun iterm-open ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n")))

(defun iterm-new-tab ()
  "Go to present working dir and focus iterm, in new tab"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n")))

(defun iterm-repeat-last-command ()
  (interactive)
  (do-applescript
   (concat
    "tell application \"iTerm2\"\n"
    "  activate\n"
    "    tell current session of current window\n"
    "      tell application \"System Events\" to keystroke (ASCII character 30)\n" ;; up arrow
    "      tell application \"System Events\" to key code 36\n" ;; return
    "    end tell\n"
    "end tell\n")))

(defun save-as (filename)
  "Clone the current buffer and switch to the clone"
  (interactive "FCopy and switch to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
           (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

(defun my-jump-to-definition ()
  (interactive)
  (if (eq 'typescript-mode major-mode)
      (tide-jump-to-definition)
    (dumb-jump-go)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun insert-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))
