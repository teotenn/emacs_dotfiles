(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile 
  (add-to-list 'load-path "~/.emacs.d/elpa/")
  (require 'use-package))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; A diferentiation for termux
(setq tt/is-termux (string-match-p
		    (rx (* nonl) "com.termux" (* nonl))
		    (getenv "HOME")))

;; Stop the welcome screen
(setq inhibit-startup-screen t)
;; Hide toolbar, scrollbar and menu
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Allows mark-type-delete
(delete-selection-mode t)

;; Starting file
;; (setq initial-buffer-choice
;;       (lambda ()
;; 	(if (buffer-file-name)
;; 	    (current-buffer)
;; 	  (find-file "~/Code/personal_config/org/brujula.org"))))

;; enable column numbers
(setq column-number-mode t)

;; show-paren-mode
(show-paren-mode 1)

;; Save bookmars upon adding each
(setq bookmark-save-flag 1)

;; always allow 'y' instead of 'yes'.
(setq use-short-answers t)

;; default to utf-8 for all the things
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Backup files in ~/tmp/
(setq temporary-file-directory "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Changelog on Linux systems
(if (eq system-type 'gnu/linux)
    (progn
      (setq add-log-full-name "Manuel Teodoro")
      (setq add-log-mailing-address "teotenn@proton.me")
      (setq change-log-default-name "CHANGELOG")))

(add-hook 'change-log-mode-hook
	  (lambda ()
	    (make-local-variable 'tab-width)
	    (make-local-variable 'left-margin)
	    (setq tab-width   2
		  left-margin 2)))

;; Shell warning indentation off
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'message) #'ignore))
                (apply orig-fun args))))

;; Extra code
(add-to-list 'load-path "~/Code/emacs_dotfiles/emacs/scripts/")

;; Make eww default browser
(setq browse-url-browser-function 'eww-browse-url)

;; From emacs 29.1
(setq show-paren-context-when-offscreen 'overlay)

;; fill columns to 80
(setq fill-column 80)

;; Prompt password for open gpg keys
(if (eq system-type 'gnu/linux)
    (setq epa-pinentry-mode 'loopback))
;; Requires to create file ~/.gnupg/gpg-agent.conf and add the following lines
;; allow-emacs-pinentry
;; allow-loopback-pinentry

;; personal function for windows
(defun tt/wrap ()
  "Shortcut to open neotree directly on wrapper"
  (interactive)
  (if (eq system-type 'windows-nt)
      (neotree-dir "c:/Users/teodorm3/Documents/Wrapper")
    (message "tt/wrap is available only on Windows")))

;; Personal registers
(set-register ?w '(buffer . "workflow.org"))

;; Select font
(defun tt/set-font-if-found (family font size)
  "If the font is installed, sets it globally for the session,
   given a `family' name, `set-frame-font' the `font' by its name and `size'"
  (let ((selected-font (format "%s %s" font size)))
  (find-font (font-spec :name family))
  (set-frame-font selected-font)))

;; My pre-selected font
(tt/set-font-if-found "Jetbrains" "Jetbrains Mono" 10)

(defun tt/switch-font (arg)
  "Switches fonts from a pre-defined list of `arg' size, or 12 by default.

   The list and details of the fonts has to be defined within the function
   based on personal choice."
  (interactive "P")
  (let* ((list-fonts '("Jetbrains" "Montserrat" "Monospace"))
	 (font-size (or arg 10))
	 (selected-font (ido-completing-read "Select font: " list-fonts)))
    (cond
     ((< font-size 7)
      (message "Size selected is too small!"))
     ((string= selected-font "Jetbrains")
      (tt/set-font-if-found "Jetbrains" "Jetbrains Mono" font-size))
     ((string= selected-font "Montserrat")
      (tt/set-font-if-found "Montserrat" "Montserrat" font-size))
     ((string= selected-font "Monospace")
      (tt/set-font-if-found "Cascadia Mono" "Monospace" font-size)))))

;; Keybindings to toggle 
(defvar toggle-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'display-line-numbers-mode)
    (define-key map "t" 'tool-bar-mode)
    (define-key map "s" 'scroll-bar-mode)
    (define-key map "m" 'menu-bar-mode)
    (define-key map "f" 'tt/switch-font)
    (define-key map "d" 'neotree-toggle)
    map)
  "Key map for toggling")
(global-set-key (kbd "<f9>") toggle-keymap)

;; tt-edit-mode
(require 'tt-edit-mode)
(global-set-key (kbd "<f12>") tt-edit-mode-keymap)

;; Some interesting moving through buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Inside emacs 
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; From outside
(defun client-save-kill-emacs(&optional display)
  " This is a function that can bu used to save buffers and 
shutdown the emacs daemon. It should be called using 
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers, active clients
or frame.  If so, an x window will be opened and the user will
be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

    ; Check if there are modified buffers, active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
					(> (length (frame-list)) 1)
				       ))  

    ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
	(message "Initializing x windows system.")
	(x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

    ; Save the current frame.  
    (setq new-frame (selected-frame))


    ; When displaying the number of clients and frames: 
    ; subtract 1 from clients (this client).
    ; subtract 2 from frames (the frame just created and the default frame.)
    (when (or (not active-clients-or-frames)
	       (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2)))) 
      
      ; If the user quits during the save dialog then don't exit emacs.
      ; Still close the terminal though.
      (let((inhibit-quit t))
             ; Save buffers
	(with-local-quit
	  (save-some-buffers)) 
	      
	(if quit-flag
	  (setq quit-flag nil)  
          ; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
		 ; Exit emacs
	    (kill-emacs))) 
	))

    ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )


(defun modified-buffers-exist() 
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
		 (buffer-modified-p buffer)
		 (not (buffer-base-buffer buffer))
		 (or
		  (buffer-file-name buffer)
		  (progn
		    (set-buffer buffer)
		    (and buffer-offer-save (> (buffer-size) 0))))
		 )
	(setq modified-found t)
	)
      )
    modified-found
    )
  )

;; Dictionaries
;; (use-package flyspell
;;   :defer t
;;   :if (eq system-type 'windows-nt)
;;   :init
;;   (setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling"))
;;   (setq ispell-program-name "C:\\Users\\teodorm3\\Bin\\Hunspell\\bin\\hunspell.exe"))

(if (eq system-type 'gnu/linux)
    (use-package flyspell
      :config
      ;; Check on the go for all text-based modes (org, md, etc)
      (add-hook 'text-mode-hook 'flyspell-mode)
      (setq ispell-list-command "--list")
      (setq ispell-program-name "aspell")))

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

;; Config for windows
(if (eq system-type 'windows-nt)
    (use-package ssh-agency))
(if (eq system-type 'windows-nt)
    (setenv "SSH_ASKPASS" "git-gui--askpass"))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/tmp/"))))

;; (setq undo-tree-history-directory-alist '(("." . "~/tmp")))

(if (eq system-type 'gnu/linux)
    (use-package i3wm-config-mode
      :mode
      (("/sway/.*config.*/" . i3wm-config-mode)
      ("/sway/config\\'" . i3wm-config-mode))))

(use-package cheatsheet
  :bind ("C-c s" . cheatsheet-show)
  :config
  (cheatsheet-add-group 'Info
			'(:key "C-x l" :description "count-lines-page"))
  (cheatsheet-add-group 'Consoles
			'(:key "M-r" :description "Back search")
			'(:key "C-c C-l" :description "list previous commands")
			'(:key "C-c RET" :description "copy NOT execute cmd"))
  (cheatsheet-add-group 'R
			'(:key "C-c <F5>" :description "shiny run_app()")
			'(:key "C-c C-z" :description "move console-script"))
  (cheatsheet-add-group 'Move
			'(:key "M-g i" :description "i menu")
			'(:key "C-c m" :description "imenu-list-smart-toggle")
			'(:key "C-x o" :description "other-window"))
  (cheatsheet-add-group 'Edit
			'(:key "M-h" :description "mark-parragraph")
			'(:key "C-M-h" :description "mark function")
			'(:key "M-y" :description "yank-pop")
			'(:key "C-x C-o" :description "delete-blank-lines")
			'(:key "C-x n" :description "narrow menu")
			'(:key "C-x w" :description "widen")
			'(:key "C-c c" :description "tt/copy-symbol-at-point")
			'(:key "M-u" :description "make-upcase-at-point")
			'(:key "C-x C-u" :description "upcase-region")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "SlateBlue1"))))
   '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse4"))))
   '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium orchid"))))
   '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "HotPink1"))))
   '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "SystemHilight"))))
   '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray55"))))
   '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "firebrick1"))))
   '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse2"))))
   '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "purple3"))))
   ))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/Code/emacs_dotfiles/emacs/snippets"
	  ))
  :config
  (yas-global-mode 1))

;; csv-mode is not default anymore
(use-package csv-mode
  :mode ("\\.csv" . csv-mode))

;; load screenshot script
;; cloned from https://github.com/tecosaur/screenshot
;; Require pckgs <transient> and <posframe>
(use-package transient
  :defer t)
(use-package posframe
  :defer t)

(defun tt/load-screenshot()
  (interactive)
  (load "screenshot.el"))

;; Lisp interpreter (for slime and sly)
;; (use-package slime
;;   :if (eq system-type 'windows-nt)
;;   :ensure nil
;;   :disabled)

;; (use-package slime
;;   :if (eq system-type 'gnu/linux)
;;   :init
;;   (setq inferior-lisp-program "sbcl"))

;; neotree
(use-package neotree
  :commands neotree-dir
  :config
  (setq neo-theme 'icons))

;; htmlize to improve rendering of source code blocks
(use-package htmlize)

;; all the icons
(use-package all-the-icons
  :defer t
  :if (display-graphic-p))

(use-package imenu-list
  :bind (("C-c m" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t))

;; Flycheck for syntax. Not global
;;(setq lintr-modifier-function "with_defaults(line_length_linter=NULL)")

;; (use-package flycheck
;;   :config
;;   (setq flycheck-lintr-linters tt/lintr-linters))

(use-package flycheck
  :if (eq system-type 'windows-nt)
  :init
  (setq flycheck-r-lintr-executable "C:\\Users\\teodorm3\\Bin\\R-4.2.1\\bin\\x64\\R.exe")
  :config
  (setq flycheck-lintr-linters "linters_with_defaults(line_length_linter = line_length_linter(120))"))

;; Flymake
  (setq tt/lintr-linters
	"lintr::linters_with_defaults(
	     line_length_linter = line_length_linter(120),
	     linters = object_name_linter(styles = c('dotted.case', 'lowercase', 'snake_case'))
	   )"
	)

  (use-package flymake
    :config
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; ESS ------------------------
;; R on windows
(if (eq system-type 'windows-nt)
    (setq inferior-ess-r-program "C:/Program Files (x86)/R-4.1.2/bin/R.exe"))

;; Personal functions for ess
(defun tt-inferior-ess-keymap ()
  "Define a keymap for ESS inferior processes to call prev and next command
     with C-up and C-down respectively"
  (setq-local ansi-color-for-comint-mode 'filter)
  (define-key inferior-ess-mode-map [\C-up]
	      'comint-previous-matching-input-from-input)
  (define-key inferior-ess-mode-map [\C-down]
	      'comint-next-matching-input-from-input)
  (define-key inferior-ess-mode-map [\C-x \t]
	      'comint-dynamic-complete-filename))

;; Send personal commands to R
(defun tt-send-command-to-r (command)
  "Sends the string `command' to ESS r process"
  (if (stringp command)
      (let ((proc (ess-get-process)))
	(ess-send-string proc command))
    (message "The command is not a character string")))

(defvar tt-r-profile "
  options(help_type = \"text\")\n
  utils::assignInNamespace(\"q\",
    function(save = \"no\", status = 0, runLast = TRUE) 
      {.Internal(quit(save, status, runLast))}, 
    \"base\")
")

(defun tt-r-ess-init ()
  "Sends variable `tt-r-profile' to an ESS process"
  (tt-send-command-to-r tt-r-profile))

(defun tt-rsend-shiny-run-app ()
  "Executes shiny <run_app()> in the inferior-ess-r process."
  (interactive)
  (tt-send-command-to-r "run_app()"))

(defun tt-rsend-dev-off ()
  (interactive)
  (tt-send-command-to-r "dev.off()"))

(defvar tt-exec-r-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'tt-rsend-shiny-run-app)
    (define-key map "o" 'tt-rsend-dev-off)
    map)
  "Key map to send commands to inferior ESS R")

;(global-set-key (kbd "<f5>") tt-exec-r-keymap)

;; ESS config
(use-package ess
  ;:defer t
  :init
  (setq ess-style 'RStudio)
  :mode
  (("\\.[rR]" . ess-r-mode)
   ("\\.[jJ][lL]" . ess-julia-mode))
  :hook ((inferior-ess-mode . tt-inferior-ess-keymap)
	 (ess-r-post-run . tt-r-ess-init))
  :config
  (setq ess-r-flymake-linters tt/lintr-linters)
  (setq ess-use-flymake nil)
  (setq ess-eval-visibly-p t) ; ESS process (print all)
  (setq ess-ask-for-ess-directory nil)
  ;; Syntax highlights
  (setq ess-R-font-lock-keywords
	'((ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-R-fl-keyword:%op% . t)
	  (ess-fl-keyword:fun-calls . t)
	  (ess-fl-keyword:numbers . t)
	  (ess-fl-keyword:operators)
	  (ess-fl-keyword:delimiters)
	  (ess-fl-keyword:=)
	  (ess-R-fl-keyword:F&T . t))))

(if (eq system-type 'gnu/linux)
    (use-package julia-mode
      :mode ("\\.[jJ][lL]" . ess-julia-mode)
      :hook (julia-mode-hook . ess-julia-mode)))

;; Add my keymaps hook
(add-hook 'ess-r-mode-hook
  (lambda() (local-set-key (kbd "C-c t") tt-exec-r-keymap)))
(add-hook 'inferior-ess-r-mode-hook
  (lambda() (local-set-key (kbd "C-c t") tt-exec-r-keymap)))

;; R markdown
(use-package polymode)
(use-package poly-R)
(use-package poly-markdown)
(use-package quarto-mode)

;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

 ;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
 ;;(autoload 'r-mode "ess-site" "(Autoload)" t)

;; Add chunk
(defun rmarkdown-new-chunk (name)
  "Insert a new R chunk."
  (interactive "sChunk name: ")
  (insert "\n```{r " name "}\n")
  (save-excursion
    (newline)
    (insert "```\n")
    (previous-line)))
;; Map it to C-c `
(define-key markdown-mode-map "\C-c`" 'rmarkdown-new-chunk)

(use-package company
  :config
  ;; Turn on company-mode globally:
  (add-hook 'after-init-hook 'global-company-mode)
  ;; integration for capf
  ;; (setq completion-at-point-functions
  ;; 	(list
  ;; 	 (cape-company-to-capf
  ;; 	  (apply-partially #'company--multi-backend-adapter
  ;; 			   '(company-dabbrev company-elisp)))))
;; More customization options for company:
(setq company-selection-wrap-around t
      ;; Align annotations to the right tooltip border:
      company-tooltip-align-annotations t
      ;; Idle delay in seconds until completion starts automatically:
      company-idle-delay 0.45
      ;; Completion will start after typing two letters:
      company-minimum-prefix-length 3
      ;; Maximum number of candidates in the tooltip:
      company-tooltip-limit 10))

(use-package company-quickhelp
  :after company
  :custom
  ;; Load company-quickhelp globally:
  (company-quickhelp-mode)
  ;; Time before display of documentation popup:
  (setq company-quickhelp-delay nil))

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; (when (require 'cape nil :noerror)
;;   ;; Setup Cape for better completion-at-point support and more

;;   ;; Add useful defaults completion sources from cape
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)

;;   ;; Silence the pcomplete capf, no errors or messages!
;;   ;; Important for corfu
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;;   ;; Ensure that pcomplete does not write to the buffer
;;   ;; and behaves as a pure `completion-at-point-function'.
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package org
  :ensure nil
  :bind
  ("M-q" . toggle-truncate-lines)
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files '("~/Code/personal_config/org/"))
  ;; Settags closer (default is -80)
  (setq org-tags-column -40)
  ;; src blocks
  (setq org-src-fontify-natively t
	org-src-window-setup 'current-window
	org-src-strip-leading-and-trailing-blank-lines t
	org-src-preserve-indentation t
	org-src-tab-acts-natively t)
  ;; org clock format
  (setq org-duration-format (quote h:mm))
  (setq org-ellipsis " >"))

;; --- ORG BABEL ---
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (emacs-lisp . t))
 )

(if (eq system-type 'gnu/linux)
    (setq org-babel-python-command "python3"))

(if (eq system-type 'windows-nt)
    (setq org-babel-R-command "C:/Users/teodorm3/Bin/R-4.2.1/bin/x64/R --slave --no-save"))

(use-package org-tempo
  :ensure nil
  :config
  ;; clocktable
  (add-to-list 'org-structure-template-alist '("CT" . ": clocktable :scope subtree :maxlevel 4 :block today"))
  ;; other
  ;; (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("pexport" . "src python :session :results output :exports both"))
  (add-to-list 'org-structure-template-alist '("pnoeval" . "src python :exports code :eval no"))
  (add-to-list 'org-structure-template-alist '("phide" . "src python :session :exports none"))
  ;; elisp
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  ;; R
  (add-to-list 'org-structure-template-alist '("r" . "src R"))
  (add-to-list 'org-structure-template-alist '("rtibble" . "src R :session :results table :colnames yes :exports both"))
  (add-to-list 'org-structure-template-alist '("rplot" . "src R :session :file figure-N.png :results value graphics file :results output :exports both"))
  (add-to-list 'org-structure-template-alist '("rexport" . "src R :session :results output :exports both")))

(use-package org-transclusion)

;; Use this if not ipython
;;(setq python-shell-interpreter "python3")

(if (eq system-type 'gnu/linux)
    (progn
      (use-package elpy
	:if (eq system-type 'gnu/linux)
	:mode ("\\.py" . python-mode)
	:init
	(setq elpy-rpc-python-command "python3")
	:config
	(elpy-enable)
	(setq python-shell-interpreter "jupyter"
	      python-shell-interpreter-args "console --simple-prompt"
	      python-shell-prompt-detect-failure-warning nil)
	(add-to-list 'python-shell-completion-native-disabled-interpreters
		     "jupyter"))

      (use-package jedi
	:after elpy)

      ;; Auto formatting help
      ;; Requires to install python "black"
      ;; Use it by calling M-x blacken-buffer
      (use-package blacken
	:after elpy)

      ;; Jupyter and iPython
      (use-package ein
	:hook (ein:connect-mode-hook . ein:jedi-setup))))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (customize-set-variable 'tabspaces-initialize-project-with-todo nil)
  (customize-set-variable 'tabspaces-default-tab "Base")
  (customize-set-variable 'tabspaces-include-buffers '("*scratch*"))
  (customize-set-variable 'tabspaces-use-filtered-buffers-as-default t))

;; All the icons
(use-package all-the-icons)

;; Modus Themes ---
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
	modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  
  ;; Org mode headers
  (setq modus-themes-headings
	'((1 . (ultrabold 1.2))
          (2 . (rainbow bold 1.2))	
	  (3 . (rainbow semibold 1.1))
          (t . (semilight 1.1))))
  (setq modus-themes-scale-headings t)
  
  ;; tab bar
  (setq modus-themes-common-palette-overrides
	'((bg-tab-bar bg-cyan-nuanced)
	  (bg-tab-current bg-magenta-intense)
	  (bg-tab-other bg-cyan-subtle)
	  (fg-heading-1 blue-warmer)
          (bg-heading-1 bg-dim)))
  
  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted :no-confirm)
  (define-key toggle-keymap (kbd "z") #'modus-themes-toggle)
  (define-key toggle-keymap (kbd "Z") #'modus-themes-select))

;; tab bar mode
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)

(use-package which-key
  :config
  (which-key-mode))

(use-package time
  :ensure nil
  :config
  (setq display-time-format "%b/%e %H:%M ")
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  (add-hook 'after-init-hook #'display-time-mode))


;(load "prot-modeline.el")
(require 'prot-modeline)
(load "tt-modeline.el")

(setq mode-line-compact nil) ; Emacs 28

(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                prot-modeline-narrow
                prot-modeline-input-method
                prot-modeline-buffer-status
                " "
                prot-modeline-buffer-identification
                "  "
                prot-modeline-major-mode
                prot-modeline-process
                "  "
                prot-modeline-vc-branch
                "  "
		mode-line-position
                "  "
                prot-modeline-align-right
		(:eval (custom-modeline-region-info))
		" "
                prot-modeline-misc-info))

(prot-modeline-subtle-mode 1)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(setq dired-listing-switches "-aBhl --group-directories-first")

(use-package all-the-icons-dired
  :defer t
  :hook   (dired-mode . all-the-icons-dired-mode))

(require 'dired-ranger)

;; My keybindings
(defvar tt-dired-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'dired-ranger-copy)
    (define-key map "p" 'dired-ranger-paste)
    (define-key map "m" 'dired-ranger-move)
    map)
  "Key map for dired extensions")

(define-key dired-mode-map (kbd "C-c t") tt-dired-keymap)

;; Using garbage magic hack.
 (use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)

;;; Vertico
(when (require 'vertico nil :noerror)
  (require 'vertico-directory)
  ;; Cycle back to top/bottom result when the edge is reached
  (customize-set-variable 'vertico-cycle t)

  ;; Start Vertico
  (vertico-mode 1))

;; The code below is not necessary because those are turn off by default
;; on emacs, however they conflict with vertico and orderless so
;; Is good to remember to turn them off.
  ;; (with-eval-after-load 'crafted-defaults-config
  ;;   (fido-mode -1)
  ;;   (fido-vertical-mode -1)
  ;;   (icomplete-mode -1)
  ;;   (icomplete-vertical-mode -1)))

;; To allow vertico keep history
(use-package savehist
:init
(savehist-mode))

;;; Marginalia
(when (require 'marginalia nil :noerror)
  ;; Configure Marginalia
  (customize-set-variable 'marginalia-annotators
			  '(marginalia-annotators-heavy
			    marginalia-annotators-light
			    nil))
  (marginalia-mode 1))

;;; Orderless
(when (require 'orderless nil :noerror)
  ;; Set up Orderless for better fuzzy matching
  (customize-set-variable 'completion-styles '(orderless basic))
  (customize-set-variable 'completion-category-overrides
			  '((file (styles . (partial-completion))))))

;; Since Consult doesn't need to be required, we assume the user wants these
;; setting if it is installed (regardless of the installation method).
(when (locate-library "consult")
  ;; Set some consult bindings
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)

  (setq completion-in-region-function #'consult-completion-in-region))
