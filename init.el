;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;; No Spacesmacs but we imitate its keybindings just enough
;;; to keep our muscle memory alive.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preliminaries

(setq gc-cons-threshold 200000000)
(setq read-process-output-max (* 1024 1024))
(setq create-lockfiles nil)

;; simpleclip-paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom

(setq custom-file (expand-file-name "~/.emacs.d/emacs-custom.el"))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package configuration

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load extra files

(setenv "RIPGREP_CONFIG_PATH"
        (expand-file-name "~/.rgconfig"))
(exec-path-from-shell-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(load-file (expand-file-name "~/.emacs.d/elisp/my-functions.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; startup

(load "server")
(unless (server-running-p) (server-start))
(setq inhibit-startup-screen t)
;; don't open new frames when accessed via open -a Emacs filename
(setq ns-pop-up-frames nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm

(setq helm-mode-fuzzy-match t)
(require 'helm-ag)
(require 'helm-command)
(setq helm-ag-base-command "rg --no-heading --color=never --hidden -M150")
(setq helm-M-x-fuzzy-match t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance

(load-theme 'solarized-light t)
(setq which-key-idle-delay 3.0)
(which-key-mode)
;; brew tap homebrew/cask-fonts
;; brew install --cask font-iosevka
;; (setq default-frame-alist '((font . "Iosevka-15")))
;; (setq default-frame-alist '((font . "Menlo-15")))

;; maximize vertically
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
;; modeline
(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior

(save-place-mode 1)
(setq confirm-nonexistent-file-or-buffer nil)

;; git

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(require 'git-link)
(setq git-link-open-in-browser t)

;; backups

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; misc

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(require 'better-defaults)
(require 'simpleclip)
(simpleclip-mode 1)
(mode-line-bell-mode)
(setq vc-follow-symlinks t) ;; avoid annoying prompt

;; scrolling

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)

;; general

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)
(global-display-line-numbers-mode)
(recentf-mode 1)

;; expand-region

(setq expand-region-contract-fast-key "V")

;; aggressive-indent

(mapc (lambda (hook)
        (add-hook hook 'aggressive-indent-mode))
      '(clojure-mode-hook))

(mapc (lambda (hook))
      '(typescript-mode-hook))

;; see https://github.com/clojure-emacs/clojure-mode/issues/516
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq comment-indent-function 'comment-indent-default)
            (setq comment-add 0)
            (comment-normalize-vars)
            (setq-local comment-column 0)))

;; lisp


(require 'smartparens-config)

(dolist (hook '(clojure-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-smartparens-strict-mode))
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect nil)

;; C-q: insert literal character
;; C-q C-j -> newline
;; delete a quote character without regard for balancing: C-u 0 BACKSPACE
;; C-h k <KEY> -> help for key or key sequence (describe-key)
;; C-x C-h -> show all prefix commands that with C-x; works with any prefix commmand
;; C-c -> prefix command for user-defined keybindings; package authors shouldn't use it


(defun clojure-word-chars ()
  "Add more special characters " "for Clojure."
  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w"))

(add-hook 'clojure-mode-hook 'clojure-word-chars)
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

;; javascript

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
(setq js2-strict-missing-semi-warning nil)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq-default typescript-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))
(add-hook 'typescript-ts-mode-hook #'prettier-js-mode)
(add-hook 'json-ts-mode-hook #'prettier-js-mode)
(add-hook 'yaml-mode-hook #'prettier-js-mode)
(add-hook 'json-ts-mode-hook #'prettier-js-mode)

;; text

(add-hook 'visual-line-mode-hook (lambda () (setq fill-column 90)))
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-fill-column-mode)

;;;; yaml

(require 'highlight-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard


(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode t)
(global-evil-visualstar-mode)
(evil-collection-init)
(evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)

(require 'evil-leader)
(global-evil-leader-mode)

;; Use MacOS key-bindings (Option-hyhpen, Option-Shift-hyphen) for en-dash and em-dash
;; (define-key undo-tree-map (kbd "M-_") nil) ;; remove override
(global-set-key (kbd "M-_") (lambda () (interactive) (insert "—")))
(global-set-key (kbd "M--") (lambda () (interactive) (insert "–")))
(global-set-key (kbd "M-8") (lambda () (interactive) (insert "•")))
(global-set-key (kbd "M-*") (lambda () (interactive) (insert "°")))
(global-set-key (kbd "C-;") 'yas-insert-snippet)
(global-set-key (kbd "s-V") 'paste-quoted)
(global-set-key (kbd "s-q") 'kill-emacs)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'eval-region
  "d" 'eval-defun
  "x" 'eval-expression)

(mapc (lambda (state)
        (evil-global-set-key state (kbd "s-=") (lambda () (interactive) (text-scale-increase 1)))
        (evil-global-set-key state (kbd "s-+") (lambda () (interactive) (text-scale-increase 1)))
        (evil-global-set-key state (kbd "s--") (lambda () (interactive) (text-scale-increase -1)))
        (evil-global-set-key state (kbd "SPC SPC") 'helm-M-x)
        (evil-global-set-key state (kbd "SPC i i") 'helm-imenu)
        (evil-global-set-key state (kbd "SPC b b") 'helm-mini)
        (evil-global-set-key state (kbd "SPC b d") 'kill-current-buffer)
        (evil-global-set-key state (kbd "SPC b l") 'helm-bookmarks)
        (evil-global-set-key state (kbd "SPC b t") 'bookmark-set)
        (evil-global-set-key state (kbd "SPC w m") 'delete-other-windows)
        (evil-global-set-key state (kbd "SPC f f") 'helm-find-files)
        (evil-global-set-key state (kbd "SPC f d") 'projectile-find-file-dwim)
        (evil-global-set-key state (kbd "SPC f s") 'show-file-name)
        (evil-global-set-key state (kbd "SPC f c") 'copy-filename-relative-to-git-root)
        (evil-global-set-key state (kbd "SPC f r") 'rename-file-and-buffer)
        (evil-global-set-key state (kbd "SPC f g") (lambda ()
                                                     (interactive) (projectile-find-file-in-directory "/Users/user/prg/telli/gizmos")))
        (evil-global-set-key state (kbd "SPC j f") 'find-monorepo-file)
        (evil-global-set-key state (kbd "SPC /") 'helm-do-ag-project-root)
        (evil-global-set-key state (kbd "SPC ?") 'mopro-helm-ag)
        (evil-global-set-key state "U" 'backward-up-list)
        (evil-global-set-key state "R" 'down-list)
        (evil-global-set-key state "L" 'sp-forward-sexp)
        (evil-global-set-key state "H" 'sp-backward-sexp)
        (evil-global-set-key state ";" 'mark-sexp)
        (evil-global-set-key state (kbd "SPC o f") 'fill-paragraph)
        (evil-global-set-key state (kbd "SPC o o") 'find-primary-proj)
        (evil-global-set-key state (kbd "SPC o O") 'find-secondary-proj)
        (evil-global-set-key state (kbd "SPC s a") 'save-as)
        (evil-global-set-key state (kbd "SPC s s") 'save-some-buffers)
        (evil-global-set-key
         state
         (kbd "SPC o a")
         (lambda ()
           (interactive)
           (find-file "~/prg/aoc2022/src/day.ts")))
        (evil-global-set-key state (kbd "SPC o c") 'find-compose)
        (evil-global-set-key state (kbd "SPC o C") 'find-compose2)
        (evil-global-set-key state (kbd "SPC o n") 'find-notes)
        (evil-global-set-key state (kbd "SPC o 1") 'find-1x1)
        (evil-global-set-key state (kbd "SPC o r") 'find-reading)
        (evil-global-set-key state (kbd "SPC o w") 'find-writing)
        (evil-global-set-key state (kbd "SPC o s") 'find-scraps)
        (evil-global-set-key state (kbd "SPC o i") 'find-it)
        (evil-global-set-key state (kbd "SPC o d") 'find-diary)
        (evil-global-set-key state (kbd "SPC o D") 'find-worktodo)
        (evil-global-set-key state (kbd "SPC t o") 'iterm-open)
        (evil-global-set-key state (kbd "SPC t r") 'iterm-repeat-last-command)

        ;; (evil-global-set-key state (kbd "SPC t t") 'tgt-toggle)
        (evil-global-set-key state (kbd "SPC t t") 'projectile-toggle-between-implementation-and-test)

        (evil-global-set-key state (kbd "SPC j j") 'lsp-bridge-find-def)
        (evil-global-set-key state (kbd "SPC j b") 'lsp-bridge-find-def-return)
        (evil-global-set-key state (kbd "SPC j t") 'lsp-bridge-find-type-def)
        (evil-global-set-key state (kbd "SPC j i") 'lsp-bridge-find-impl)
        (evil-global-set-key state (kbd "SPC j r") 'lsp-bridge-find-references)
        (evil-global-set-key state (kbd "SPC j a") 'lsp-bridge-code-action)
        (evil-global-set-key state (kbd "SPC j R") 'lsp-bridge-rename)
        (evil-global-set-key state (kbd "SPC j h") 'lsp-bridge-show-documentation)
        (evil-global-set-key state (kbd "SPC j s") 'lsp-bridge-workspace-list-symbol-at-point)
        (evil-global-set-key state (kbd "SPC j l") 'lsp-bridge-workspace-list-symbols)
        (evil-global-set-key state (kbd "SPC r l") 'helm-resume)
        (evil-global-set-key state (kbd "SPC r y") 'helm-show-kill-ring)
        (evil-global-set-key state (kbd "SPC p F") 'helm-projectile)
        (evil-global-set-key state (kbd "SPC p f") 'project-find-file)
        (evil-global-set-key state (kbd "SPC f e d") 'find-init-el)
        (evil-global-set-key state (kbd "SPC f e z") 'find-zshrc)
        (evil-global-set-key state (kbd "SPC f e m") 'find-my-functions)
        (evil-global-set-key state (kbd "SPC m c") 'cider-force-connect)
        (evil-global-set-key state (kbd "SPC m k") 'cider-force-eval-buffer)
        (evil-global-set-key state (kbd "SPC s c") 'evil-ex-nohighlight)
        (evil-global-set-key state (kbd "SPC i e") 'em-dash)

        (evil-global-set-key state (kbd "SPC s k") 'smerge-keep-current)

        (evil-global-set-key state (kbd "SPC m m") 'markdown-shifttab)

        (evil-global-set-key state (kbd "SPC s j") 'sp-split-sexp)
        (evil-global-set-key state (kbd "SPC k t") 'sp-transpose-sexp)
        (evil-global-set-key state (kbd "SPC k s") 'sp-forward-slurp-sexp)
        (evil-global-set-key state (kbd "SPC k b") 'sp-forward-barf-sexp)
        (evil-global-set-key state (kbd "SPC k r") 'raise-sexp)
        (evil-global-set-key state (kbd "SPC k w") 'insert-parentheses)
        (evil-global-set-key state (kbd "SPC k W") 'sp-unwrap-sexp)
        (evil-global-set-key state (kbd "SPC k c") 'sp-clone-sexp)
        (evil-global-set-key state (kbd "SPC o b") 'sp-splice-sexp-killing-backward)
        (evil-global-set-key state (kbd "SPC o t") 'touch)
        (evil-global-set-key state (kbd "SPC c c") 'clojure-comment-dwim)

        (evil-global-set-key state (kbd "SPC e e") 'lsp-bridge-diagnostic-jump-next)
        (evil-global-set-key state (kbd "SPC e p") 'lsp-bridge-diagnostic-jump-prev)
        (evil-global-set-key state (kbd "SPC e l") 'my-buffer-diagnostics)
        (evil-global-set-key state (kbd "SPC e L") 'my-project-diagnostics)

        (evil-global-set-key state (kbd "SPC i b") 'insert-bar)

        ;; (evil-global-set-key state (kbd "SPC t t") 'projectile-toggle-between-implementation-and-test)

        (evil-global-set-key state (kbd "SPC g g") 'open-in-goland)
        (evil-global-set-key state (kbd "SPC g w") 'open-in-webstorm)
        (evil-global-set-key state (kbd "SPC g c") 'open-in-cursor)
        (evil-global-set-key state (kbd "SPC g r") 'open-in-rubymine)
        (evil-global-set-key state (kbd "SPC g v") 'open-in-vscode)
        (evil-global-set-key state (kbd "SPC g l l") 'git-link)
        (evil-global-set-key state (kbd "SPC g t m") 'git-timemachine)
        (evil-global-set-key state (kbd "SPC g ]") 'diff-hl-next-hunk)
        (evil-global-set-key state (kbd "SPC g [") 'diff-hl-prev-hunk))
      '(normal visual))

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC c l") 'comment-line)
        (evil-global-set-key state (kbd "SPC v") 'er/expand-region))
      '(normal))

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC c l") 'comment-dwim)
        )
      '(visual))

(evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "0" 'evil-beginning-of-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "$" 'evil-end-of-visual-line)

(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(require 'yasnippet)
(yas-global-mode 1)



;; (setq tab-always-indent 'complete)
;; (global-company-mode)
;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)

(helm-mode 1)


;; (dolist (hook '(clojure-mode-hook
;;                 python-mode-hook
;;                 typescript-ts-mode-hook
;;                 go-mode-hook
;;                 js2-mode-hook))
;;   (add-hook hook 'eglot-ensure))

(add-hook 'project-find-functions
          #'(lambda (d)
              (let ((dir (find-enclosing-project d)))
                (if dir (list 'vc 'Git  dir) nil))))

(add-to-list 'tgt-projects '((:root-dir "~/prg/elucidate")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/pico")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/pretzel")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/baselib")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/grasshopper")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/watchdog")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/pit")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/bblib")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/circle")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/rhino")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/projects/devtools")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/pitch/pitch-app/services/backend")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/prg/aoc2022")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes ".test")))
(add-to-list 'tgt-projects '((:root-dir "~/prg/bash2bb")
                             (:src-dirs "src")
                             (:test-dirs "test")
                             (:test-suffixes "_test")))
(add-to-list 'tgt-projects '((:root-dir "~/prg/pgltest")
                             (:src-dirs "lib")
                             (:test-dirs "test")
                             (:test-suffixes ".test")))
(setq tgt-open-in-new-window nil)

;; (setq langtool-language-tool-jar "/Users/user/lagnguagetool/languagetool-commandline.jar")
;; (require 'langtool)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(winner-mode 1)

(require 'wgrep)

;; (require 'apheleia)
;; (apheleia-global-mode +1)

(add-hook 'go-mode-hook 'hs-minor-mode)

(setq bookmark-save-flag 1)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp bridge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lsp-bridge"))

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(dolist (hook '(python-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook))
  (add-hook hook #'add-node-modules-path)
  (add-hook hook #'lsp-bridge-mode))

(defun my/project-node-bin ()
  "Return the nearest node_modules/.bin directory, or nil."
  (let* ((root (locate-dominating-file default-directory "node_modules"))
         (bin  (and root (expand-file-name "node_modules/.bin" root))))
    (when (file-directory-p bin) bin)))

(defun my/use-project-node-bin ()
  "Prepend the project’s node_modules/.bin to exec-path & PATH **for this buffer only**."
  (when-let ((bin (my/project-node-bin)))
    ;; Make PATH & exec‑path buffer‑local so we don't leak into other projects
    (make-local-variable 'exec-path)
    (cl-pushnew bin exec-path :test #'string-equal)

    (make-local-variable 'process-environment)
    (let* ((path (getenv "PATH"))
           (new  (concat bin path-separator path)))
      (setenv "PATH" new))))

(defun my/project-venv-bin ()
  "Return the closest .venv/*bin directory, or nil if none is found."
  (let* ((root (locate-dominating-file default-directory ".venv"))
         (bin  (and root
                    (expand-file-name
                     (if (eq system-type 'windows-nt)
                         ".venv/Scripts"      ; Windows uses Scripts\
                       ".venv/bin")          ; POSIX uses bin/
                     root))))
    (when (file-directory-p bin) bin)))

(defun my/use-project-venv-bin ()
  "Prepend the project’s .venv/*bin to exec-path & PATH **for this buffer only**."
  (when-let ((bin (my/project-venv-bin)))
    ;; Don’t leak to other buffers
    (make-local-variable 'exec-path)
    (cl-pushnew bin exec-path :test #'string-equal)

    (make-local-variable 'process-environment)
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))))

(dolist (hook '(python-mode-hook
                typescript-mode-hook
                tsx-ts-mode-hook
                js-mode-hook))
  (add-hook hook #'my/use-project-node-bin)
  (add-hook hook #'my/use-project-venv-bin))

(setq lsp-bridge-python-command "/opt/homebrew/bin/python3")
(setq lsp-bridge-python-lsp-server "pyright")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; init.el ends here

;; (require 'python-mode)

(use-package lazy-ruff
  :ensure t
  :hook ((python-mode . lazy-ruff-mode))
  :config
  (setq lazy-ruff-format-command "ruff format")
  (setq lazy-ruff-check-command "ruff check --select I")
  (lazy-ruff-global-mode t))


(defun my-fun () (interactive)
       (switch-to-prev-buffer-skip (lambda (win buf &rest args)
				     (not (buffer-modified-p buf)))))
