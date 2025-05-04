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
    (cider-connect (plist-put '(:host "localhost" :cljs-repl-type shadow)
                              :port (cl-second (cl-first (cider-locate-running-nrepl-ports)))))))

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
  (find-file "~/prg/telli/backend/package.json"))

(defun find-secondary-proj ()
  (interactive)
  (find-file "~/pitch/pitch-app/projects/backend/dev/user.clj"))

(defun find-compose ()
  (interactive)
  (find-file "~/zettel/compose.md"))
(defun find-compose2 ()
  (interactive)
  (find-file "~/zettel/compose2.md"))
(defun find-notes ()
  (interactive)
  (find-file "/Users/user/prg/telli/gizmos/notes.txt"))
(defun find-1x1 ()
  (interactive)
  (find-file "~/zettel/1x1.txt"))
(defun find-reading ()
  (interactive)
  (find-file "~/zettel/reading.md"))
(defun find-writing ()
  (interactive)
  (find-file "~/zettel/writing.md"))
(defun find-scraps ()
  (interactive)
  (find-file "~/zettel/scrap.dat"))

(defun find-it ()
  (interactive)
  (find-file "~/arena/bin/it"))
(defun find-diary ()
  (interactive)
  (find-file "~/zettel/diary.md"))
(defun find-worktodo ()
  (interactive)
  (find-file "~/zettel/worktodo.md"))

(defun find-init-el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun find-zshrc ()
  (interactive)
  (find-file "~/.zshrc"))
(defun find-my-functions ()
  (interactive)
  (find-file "~/.emacs.d/elisp/my-functions.el"))

(defun find-work ()
  (interactive)
  (find-file "~/zettel/work.org"))

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
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun insert-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun zprint-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "zprint" (buffer-name) t)))

(require 'cl-extra)
(setq project-sentinels '("bb.edn" "deps.edn" "package.json" ".monorepo-project" ".git"))

(defun find-enclosing-project (dir)
  (locate-dominating-file
   dir
   (lambda (file)
     (and (file-directory-p file)
          (cl-some (lambda (sentinel)
                     (file-exists-p (expand-file-name sentinel file)))
                   project-sentinels)))))

(defun project-find-file-in-repo ()
  (interactive)
  (let* ((root (locate-dominating-file default-directory ".git"))
         (pr (project-current nil root)))
    (project-find-file-in nil (list root) pr nil)))

(defun paste-as-comment ()
  (interactive)
  (let ((beg (point-marker))
	(end (save-excursion
	       (insert " ")
	       (point-marker))))
    (yank-pop)
    (comment-region beg end)))

(defun flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(defun mopro-helm-ag (&optional query)
  (interactive)
  (let ((dir (find-enclosing-project ".")))
    (unless dir
      (error "Could not find the root"))
    (helm-do-ag dir nil query)))

(defun mopro-find-file ()
  (interactive)
  (let ((root (or
               (find-enclosing-project ".")
               projectile-project-root)))
    (projectile-find-file-in-directory root)))

(defun magic-tokenize ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "magic tokenize" (buffer-name) t)))

(defun magic-go-locate ()
  (interactive)
  (save-excursion
    (let* ((command (format "magic go-locate %s" (shell-quote-argument (buffer-file-name))))
          (result (shell-command-to-string command)))
      (message result))))

;; (json-parse-string "[\"a\"]")
;; (completing-read
;;  "Complete a foo: "
;;  '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
;;  nil t)

(defun open-md ()
  (interactive)
  (save-excursion
    (shell-command (concat "open-md " (buffer-file-name)))))

(defun quote-js (s)
  (with-temp-buffer
    (insert s)
    (shell-command-on-region (point-min) (point-max) "python3 -c \"import sys,json; sys.stdout.write(json.dumps(sys.stdin.read()))\"" nil 'replace)
    (buffer-string)))

(defun paste-quoted ()
  (interactive "*")
  (let ((str-val (quote-js (simpleclip-get-contents))))
    (unless str-val
      (error "No content to paste"))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (push-mark (point) t)
    (insert-for-yank str-val)
    (when (and (not (minibufferp))
               (not simpleclip-less-feedback)
               (simpleclip-called-interactively-p 'interactive))
      (message "pasted quoted from clipboard"))))

(defun my--projectile-select-files (project-files &optional invalidate-cache)
  "Select a list of files based on filename at point.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (string-replace "../"
                                 ""
                                 (or (thing-at-point 'filename) ""))))
         (files (if file
                    (cl-remove-if-not
                     (lambda (project-file)
                       (string-match-p (regexp-quote file) project-file))
                     project-files)
                  nil)))
    files))

(defun my--find-monorepo-file (invalidate-cache)
  (let* ((project-root (projectile-acquire-root))
         (project-files (projectile-project-files project-root))
         (files (my--projectile-select-files project-files invalidate-cache))
         (file (cond ((= (length files) 1)
                      (car files))
                     ((> (length files) 1)
                      (projectile-completing-read "Switch to: " files))
                     (t
                      (projectile-completing-read "Switch to: " project-files)))))
    (message "%s" (expand-file-name file project-root))
    (find-file (expand-file-name file project-root))))

(defun find-monorepo-file (&optional invalidate-cache)
  (interactive "P")
  (my--find-monorepo-file invalidate-cache))

(defun show-buffer-file-name ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun copy-filename-relative-to-git-root ()
  "Copy the current buffer's filename relative to the Git repository root to the system clipboard."
  (interactive)
  (let* ((filename (buffer-file-name))
         (git-root (locate-dominating-file filename ".git"))
         (relative-filename (if git-root
                                (file-relative-name filename git-root)
                              (user-error "Not inside a Git repository"))))
    (kill-new relative-filename)
    (cond
     ((eq system-type 'darwin) ;; macOS
      (call-process-region relative-filename nil "pbcopy"))
     ((or (eq system-type 'gnu/linux) (eq system-type 'linux)) ;; Linux
      (call-process-region relative-filename nil "xclip" nil nil nil "-selection" "clipboard"))
     ((eq system-type 'windows-nt) ;; Windows
      (with-temp-buffer
        (insert relative-filename)
        (call-process-region (point-min) (point-max) "clip")))
     (t
      (user-error "Unsupported system type")))
    (message "Copied: %s" relative-filename)))

(defun open-in-goland ()
  (interactive)
  (save-excursion
    (call-process "open" nil "*Messages*" nil "-a" "GoLand" (buffer-file-name))))

(defun open-in-webstorm ()
  (interactive)
  (save-excursion
    (call-process "/Applications/WebStorm.app/Contents/MacOS/webstorm" nil "*Messages*" nil "--line" (number-to-string (line-number-at-pos)) (buffer-file-name))))

(defun open-in-cursor ()
  (interactive)
  (save-excursion
    (call-process "cursor" nil "*Messages*" nil "-g" (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos))))))

(defun open-in-rubymine ()
  (interactive)
  (save-excursion
    (call-process "open" nil "*Messages*" nil "-a" "RubyMine" (buffer-file-name))))

(defun open-in-vscode ()
  (interactive)
  (save-excursion
    (call-process "open" nil "*Messages*" nil "-a" "Visual Studio Code" (buffer-file-name))))

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun sql-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) (expand-file-name "~/arena/vendor/pgFormatter-5.5/pg_format") (buffer-name) t)))
