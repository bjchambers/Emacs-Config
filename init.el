;; Do this early on in the load so it doesn't stay visible while loading
(setq inhibit-splash-screen t)  ; no splash screen
(line-number-mode 1)            ; have line numbers
(column-number-mode 1)          ; and column numbers

(tool-bar-mode -1)  ;; no tool bar
(scroll-bar-mode -1)
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, theres always a menu drawn, so don't let it empty
  (menu-bar-mode -1))

;; Setup some nice fonts
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "SourceCodePro-Regular")
  (set-face-font 'default "Bitstream Vera Sans Mono-10"))

;; Make the window big enough for line numbers + info on either side
(if window-system
    (set-frame-size (selected-frame) 172 40))

(global-hl-line-mode) ; highlight current line
(global-linum-mode 1) ; add line numbers on the left

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its content
;; to reflect what's on -disk
(global-auto-revert-mode 1)

;; M-x shell is a nice shell interface to use, let's make it colorful.
;; If you need a terminal emulater rather than just a shell, consider M-x term
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; When you do this, to kill emacs either close its frame from the window
;; manager, or do M-x kill-emacs.  Don't need a nice shortcut for an 
;; irregular action
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; Full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
    (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Set a sane path (based on the ZSH rc path)
(if (not (getenv "TERM_PROGRAM"))
  (setenv "PATH" (shell-command-to-string "source $HOME/.zshrc && printf $PATH"))
  (setq exec-path (split-string path ":")))

(defvar my-packages 
  (list 'magit 'solarized-theme 'python-mode 'flymake-cursor 'virtualenv 
	'flymake-jshint 'js2-mode 'fill-column-indicator 'haskell-mode
	'markdown-mode 'scala-mode)
 "Libraries that should be installed by default")

(defun install-my-packages ()
  "Install all packages listed in my-packages that aren't installed."
  (interactive)
  (dolist (package my-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun is-online? ()
  "See if we're online.  

On Windows, which doesn't have network-interface-list, assume we're online."
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) 
	      (unless (equal "lo" (car iface))
		(member 'up (first (last (network-interface-info
					  (car iface)))))))
            (network-interface-list))
      t))

(when (is-online?)
  (unless package-archive-contents (package-refresh-contents))
  (install-my-packages))

;; Setup some nicer fonts/themes
(setq preferred-themes '(solarized-dark solarized-light))
(load-theme (car preferred-themes) t)

;; Setup a function to rotate among my preferred themes
(defun toggle-theme ()
  (interactive)
  (setq preferred-themes (append (cdr preferred-themes) 
                                 (list (car preferred-themes))))
  (load-theme (car preferred-themes) t))
(global-set-key [f12] 'toggle-theme)

;; Setup magit now that its loaded
(global-set-key (kbd "C-x C-z") 'magit-status)

;; Move the custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Make the buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Setup flymake
(require 'flymake)
(require 'flymake-cursor)

(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.py\\'" flymake-pylint-init))

(defun turn-on-flymake ()
  (flymake-mode t))

(setq fci-rule-column 80)
(defun turn-on-fill-column ()
  (fci-mode))

(add-hook 'python-mode-hook 'turn-on-flymake)
(add-hook 'python-mode-hook 'turn-on-fill-column)
(add-hook 'js2-mode-hook 'turn-on-flymake)
(add-hook 'js2-mode-hook 'turn-on-fill-column)

;; No flashing cursor
(blink-cursor-mode (- (*) (*) (*)))

(defun flymake-gjslint-init ()
    "Initialize flymake for gjslint"
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace)))
      (list "gjslint" (list temp-file "--strict" "--nosummary"))))

(add-to-list 'flymake-allowed-file-name-masks
	     '(".+\\.js$"
	       flymake-gjslint-init
	       flymake-simple-cleanup
	       flymake-get-real-file-name))

(add-to-list 'flymake-err-line-patterns
	     '("^Line \\([[:digit:]]+\\), E:[[:digit:]]+: "
	       nil 1 nil))

;; Configure Haskell mode:
(add-to-list 'auto-mode-alist '("\.x$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\.y$" . haskell-mode))

;; We want to use haskell-mode for Alex files too, but we don't want the
;; fancy indenter
(defun preferred-haskell-indent ()
  (if (or (string-match "\.x$" (buffer-file-name)) 
	  (string-match "\.y$" (buffer-file-name)))
    (turn-on-haskell-simple-indent)
    (turn-on-haskell-indentation)))

(add-hook 'haskell-mode-hook 'preferred-haskell-indent)
(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

(defun haskell-hook ()
  ;; Load the current file (and make a session if not already made)
  (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)

  ;; Build the cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)

  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c cc") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at the point, print it in the message
  ;; buffer.
  (define-key haskell-mode-map  (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map  (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports.  Keep tapping to jump between import groups.
  ;; C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskel-mode-tag-find)

  ;; Save the current buffer and execute any special save actions.
  (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above, but backwards
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1))))

;; Useful to have these keybindings for .cabal files, too
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch))

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files." t)
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; Scala Mode
(require 'scala-mode-auto)

(setq-default indent-tabs-mode nil)
