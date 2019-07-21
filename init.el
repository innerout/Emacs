;;; -*- lexical-binding: t; -*-
;; (setq gc-cons-threshold 64000000) ;; Setting GC thresholds higher for faster startup
;; (add-hook 'after-init-hook #'(lambda()
;; 			       ;;restore after startup
;; 			       (setq gc-cons-threshold 800000)))

(setq ring-bell-function 'ignore) ;; Disable the ring bell
(setq inhibit-startup-screen t)   ;; Disable Startup screen

;;run in home directory find . -name "*~" -delete
(setq backup-by-copying t        ;;Backup setup
      backup-directory-alist '(("." . "~/.emacsbackups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(global-auto-revert-mode t)

(show-paren-mode 1)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-delay nil) ;; Show Matching Parenthesis without delay


(setq blink-matching-paren 'show)
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)

(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
		(not (or cursor-in-echo-area
			 executing-kbd-macro
			 noninteractive
			 (minibufferp)
			 this-command))
		(and (not (bobp))
		     (memq (char-syntax (char-before)) '(?\) ?\$)))
		(= 1 (logand 1 (- (point)
				  (save-excursion
				    (forward-char -1)
				    (skip-syntax-backward "/\\")
				    (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
		  (lambda (msg &rest args)
		    (let ((msg (apply #'format-message msg args)))
		      (setq ov (display-line-overlay+
				(window-start) msg ))))))
	 (blink-matching-open))))))

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))

(setq x-stretch-cursor t)   ;; Make cursor the width of the character it is under
(setq load-prefer-newer t)

(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer) ;; kill the current buffer without prompting

(desktop-save-mode 1) ;; Save sessions between Emacs sessions
(save-place-mode 1) ;; Opens the File in the last position that it was closed.
(setq-default save-place-forget-unreadable-files nil) ;; Optimization for nfs.

(mouse-wheel-mode t)  ;; Mouse scrolling in terminal
(blink-cursor-mode 0) ;; No blinking Cursor
(tool-bar-mode 0)     ;; Disable toolbar
(delete-selection-mode t) ;; Highlighting Text and typing deletes the text like other editors

(column-number-mode t)    ;; Column Number in modeline
(global-visual-line-mode 1)

(setq-default x-select-enable-clipboard t ;; Copy/Paste to clipboard with C-w/C-y
	      x-select-enable-primary t)

;;Shows the path to the file that is opened in the current buffer on the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (buffer-file-name)
		 "%b"))))

;;Source Code Pro font
(add-to-list 'default-frame-alist '(font . "Source Code Pro"))

(put 'erase-buffer 'disabled nil)  ;; Erases whole buffer.
(put 'upcase-region 'disabled nil) ;; Upper case disable.
(put 'set-goal-column 'disabled nil);;Doesnt reset cursor's column when changing line
(setq auto-window-vscroll nil) ;; Improves general performance when scrolling fast
;;Folding mode built-in emacs, i should search for a good folding plugin though
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode))) ;; flyspell for comments and strings
(global-hl-line-mode)

;;Disable Scroll-bar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
;;Disable Menu
(menu-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)

(when (>= emacs-major-version 27)
  (global-so-long-mode))

;;Package.el is available after version 24 of Emacs, check for older systems like CentOS
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   )
  (package-initialize))

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(setq-default use-package-always-ensure t ; Auto-download package if not exists
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;;Install use-package for the first time that emacs starts on a new system.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))
;; I should consider checking straight.el

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-interval 2)
  (auto-package-update-maybe))

(require 'bind-key)

(use-package spacemacs-theme
  :ensure t
  :no-require t)

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(use-package async
  :ensure t
  :init(async-bytecomp-package-mode 1))

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package undohist
  :ensure t
  :config
  (undohist-initialize))

;;Different Color for every variable
(use-package color-identifiers-mode
  :ensure t
  :init (add-hook 'after-init-hook 'global-color-identifiers-mode))

;;Different Color for every matching {}()[]
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package flycheck-pos-tip
  :ensure t)

;;Syntax checking for different languages
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;;Raises the limit of flycheck errors that can be displayed in a single buffer, useful when using clang-analyzer.
  (setq flycheck-checker-error-threshold 5000)
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))

(use-package flycheck-clang-analyzer
  :disabled
  :ensure t
  ;; :after flycheck
  ;; :config (flycheck-clang-analyzer-setup)
  )

(use-package aggressive-indent
  :disabled
  :ensure t
  :init
  (add-hook 'c-mode-hook 'aggressive-indent-mode)
  (add-hook 'c++-mode-hook 'aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode 'c-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line))))))

;;An amazing plugin with infinite features to use.
(use-package helm
  :disabled
  :demand t
  :ensure t
  :init
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode t)
  :bind(
  	("C-x C-f" . helm-find-files)
  	("C-x b" . helm-buffers-list)
  	("M-x" . helm-M-x)))

(use-package helm-themes
  :disabled
  :ensure t)

;;C-h b
(use-package helm-descbinds
  :disabled
  :ensure t
  :init (helm-descbinds-mode))

;;Remember to run after installation M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


(use-package ivy
  :ensure t
  :after counsel
  :bind
  ("C-s" . swiper-isearch)
  ("C-c C-r" . ivy-resume)
  ("M-x" . counsel-M-x)
  ;; ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-x p" . counsel-mark-ring)
  :init
  (define-key counsel-mode-map (kbd "C-x C-f") 'counsel-explorer)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist
  	'((t . ivy--regex-fuzzy)))
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy))

(use-package counsel
  :ensure t
  :init
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))

(use-package swiper
  :ensure t)

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  (setq ivy-virtual-abbreviate 'full
	;;ivy-rich-switch-buffer-align-virtual-buffer t ;;temporarily disabled
	)
  (setq ivy-rich-path-style 'abbrev))

(use-package ivy-prescient
  :ensure t
  :init(ivy-prescient-mode t))

(use-package prescient
  :ensure t
  :init(prescient-persist-mode t))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-explorer
  :ensure t
  :init
  (ivy-explorer-mode 1))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

;; (use-package cc-mode
;;   :config
;;   (setq c-default-style "linux")
;;   (setq c-basic-offset 8)
;;   (define-key c-mode-base-map (kbd "RET") 'newline-and-indent);; indent when pressing enter
;;   (c-set-offset 'comment-intro 0));;indent comments according to the indentation style
(use-package cc-mode)

(defun my-indent-style()
  (setq c-default-style "linux")
  (setq c-basic-offset 8)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent);; indent when pressing enter
  (c-set-offset 'comment-intro 0);;indent comments according to the indentation style
  )

(add-hook 'c-mode-hook 'my-indent-style)
(add-hook 'c++-mode-hook 'my-indent-style)

;;open files with a tree like structure
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;Autoclosing (){}[]
(use-package smartparens
  :ensure t
  :bind(("M-]" . sp-unwrap-sexp))
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t)
  (setq sp-escape-quotes-after-insert nil))

;;Cscope is an alternative for  Ctags but much faster in big codebases.
(use-package xcscope
  :disabled
  :ensure t
  :config
  (cscope-setup)
  (setq cscope-option-use-inverted-index t))

;;Shows every indentation block.
(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode . highlight-indent-guides-mode))
  :config (setq highlight-indent-guides-method 'character))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :ensure t
  ;;Special Case in python to shift to the left use C-C <
  :bind("TAB" . company-indent-or-complete-common)
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (push 'company-files company-backends))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  :ensure t)

(use-package focus
  :ensure t)

(add-to-list 'focus-mode-to-thing '(c-mode . lsp-folding-range))

(defvar home-dir (getenv "HOME"))
(defvar emacs-email (getenv "MU4E"))

(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :commands (lsp-ccls-enable)
  :init
  (setq ccls-executable  "/bin/ccls")
  (setq ccls-initialization-options '(:index (:comments 2)
					     :index (:reparseForDependency 1)
					     :completion (:detailedLabel t))))


(use-package company-lsp
  :commands company-lsp
  :ensure t
  :config (setq company-transformers nil company-lsp-async t
		company-lsp-cache-candidates nil company-lsp-enable-recompletion t)
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :bind
  (:map lsp-ui-mode-map
	([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-doc-use-webkit t
        lsp-ui-doc-enable t
	lsp-prefer-flymake nil
        lsp-ui-imenu-enable t))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp))))

;;Shows the changes that have happened to the file based on the last git commit.
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

;;magit-log-trace-definition to check the changes
;;that happened on this function in older commits
(use-package magit
  :ensure t)

;;Removes trailing whitespace and newline. I should check if emacs has a builtin mode(Probably has)
(use-package ethan-wspace
  :ensure t
  :config
  (setq mode-require-final-newline 'nil)
  (add-hook 'after-save-hook 'ethan-wspace-clean-all)
  (global-ethan-wspace-mode 1))

(use-package whitespace
  :init
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face empty tabls lines-tail trailing))
  (global-whitespace-mode))

(use-package markdown-mode
  :ensure t)

(use-package markdown-mode+
  :ensure t)

;;Startup screen alternative plugin
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;;The famous org mode, i should learn it more.
(use-package org
  :ensure org-plus-contrib
  :bind(("C-c a" . org-agenda)
  	("C-x c" . org-capture)))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook(lambda()(org-bullets-mode 1))))

(defun load-mu4e ()
  "Load my mu4e config and afterwards call mu4e."
  (interactive)
  (load-file (concat home-dir "/Downloads/mu4e-config.el"))
  (mu4e))

;; Pressing V when having a thread of emails pretifies with this plugin
(use-package mu4e-conversation
  :ensure t
  :init
  (with-eval-after-load 'mu4e (require 'mu4e-conversation))
  (with-eval-after-load 'mu4e (global-mu4e-conversation-mode)))

(use-package mu4e-alert
  :ensure t
  :init
  (mu4e-alert-set-default-style 'libnotify))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(use-package academic-phrases
  :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (add-hook 'pdf-view-mode-hook (lambda ()
				  (bms/pdf-midnite-amber))) ; automatically turns on midnight-mode for pdfs
  )

(defun bms/pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1))

(defun bms/pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
  (pdf-view-midnight-minor-mode))

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

;; M-x langtool-check
(use-package langtool
  :ensure t
  :defer t
  :init
  (setq langtool-language-tool-jar (concat home-dir "languagetool/languagetool-standalone/target/LanguageTool-4.4-SNAPSHOT/LanguageTool-4.4-SNAPSHOT/languagetool-commandline.jar"))
  (setq langtool-language-tool-server-jar (concat home-dir "/languagetool/languagetool-standalone/target/LanguageTool-4.4-SNAPSHOT/LanguageTool-4.4-SNAPSHOT/languagetool-server.jar"))
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  (setq langtool-default-language "en-US"))

(use-package multiple-cursors
  :ensure t
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("M-<down-mouse-1>" . mc/add-cursor-on-click))

(defun elfeed-start()
  "Update elfeed feeds and start it."
  (interactive)
  (elfeed-update)
  (elfeed))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed-start)
  :init (setq elfeed-feeds
	      '("https://github.com/languagetool-org/languagetool/releases.atom"
		"https://www.reddit.com/r/emacs/.rss"
		"https://www.reddit.com/r/archlinux/.rss"
		"https://www.reddit.com/r/cpp/.rss"
		"https://www.reddit.com/r/C_Programming/.rss")))

(use-package rmsbolt
  :ensure t)

;;https://projectile.readthedocs.io/en/latest/usage/
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/gitfolders/"))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-generic-command '("fd . -0"))
  (setq projectile-globally-ignored-directories
	(append '(
		  ".git"
		  ".svn"
		  "*.cquery_cached_index"
		  "*.ccls_cache"
		  )
		projectile-globally-ignored-directories))

  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t)

;;M-x bug-hunter-init-file for debugging the .emacs
(use-package bug-hunter
  :ensure t)

(use-package sublimity
  :ensure t
  :init
  (require 'sublimity-scroll)
  (require 'sublimity-attractive)
  (setq sublimity-attractive-centering-width nil)
  (sublimity-mode 1))

(defun bind-resize-frame()
  "Set keybinds to resize frames."
  (interactive)
  (global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-S-<down>") 'enlarge-window)
  (global-set-key (kbd "C-S-<up>") 'shrink-window))

(defun unbind-resize-frame()
  "Unset keybinds to resize frames."
  (interactive)
  (global-unset-key (kbd "C-S-<left>"))
  (global-unset-key (kbd "C-S-<right>"))
  (global-unset-key (kbd "C-S-<down>"))
  (global-unset-key (kbd "C-S-<up>")))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-python-executable "python")
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-lsp t))

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(use-package auto-dictionary
  :ensure t
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package company-auctex
  :ensure t
  :init (company-auctex-init))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
	    (setq TeX-source-correlate-mode t)
	    (setq TeX-source-correlate-method 'synctex)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq-default TeX-master "paper.tex")
	    (setq reftex-plug-into-AUCTeX t)
	    (pdf-tools-install)
	    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		  TeX-source-correlate-start-server t)
	    ;; Update PDF buffers after successful LaTeX runs
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)
	    (add-hook 'LaTeX-mode-hook
		      (lambda ()
			(reftex-mode t)
			(flyspell-mode t)))
	    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "67a0265e2497207f5f9116c4d2bfbbab4423055e3ab1fa46ea6bd56f7e322f6a" default)))
 '(ethan-wspace-face-customized nil)
 '(fci-rule-color "#383838")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-display-style (quote fancy))
 '(markdown-command "pandoc")
 '(mode-require-final-newline nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/TODO.org")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (auto-dictionary auctex-latexmk yasnippet-snippets xcscope which-key use-package undohist undo-tree sublimity spacemacs-theme smartparens rmsbolt rainbow-delimiters pdf-tools org-plus-contrib org-bullets objed neotree multiple-cursors mu4e-conversation mu4e-alert markdown-mode+ magit-popup magit lv lsp-ui lsp-sh lsp-python-ms langtool ivy-xref ivy-rich ivy-prescient ivy-explorer highlight-indent-guides helm-themes helm-descbinds graphql git-gutter ghub gcmh focus flycheck-pos-tip flycheck-clang-analyzer ethan-wspace elfeed eldoc-eval doom-modeline dashboard counsel-projectile company-lsp company-auctex color-identifiers-mode ccls bug-hunter beacon auto-package-update auto-compile all-the-icons-ivy all-the-icons-dired aggressive-indent ag academic-phrases)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(send-mail-function (quote smtpmail-send-it))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "red"))))
 '(show-paren-mismatch ((t (:background "blue")))))

(if (string= "1" emacs-email)
    (load-mu4e))
