
;;Setting GC thresholds higher
(setq gc-cons-threshold 20971520
      gc-cons-percentage 0.3)

;;Shows the path to the file that is opened in the current buffer on the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (buffer-file-name)
		 "%b"))))

;;Source Code Pro font
(add-to-list 'default-frame-alist '(font . "Source Code Pro"))

;;Column Number in modeline
(column-number-mode t)

;;Show Matching Parenthesis without delay
(show-paren-mode 1)
(setq show-paren-delay nil)

;;Highlighting Text and typing deletes the text like other editors
(delete-selection-mode t)

;; Opens the File in the last position that you closed it.The setq-default optimizes this function for nfs.
(save-place-mode 1)
(setq-default save-place-forget-unreadable-files nil)

;;Mouse scrolling in terminal
(mouse-wheel-mode t)

;;No blinking Cursor
(blink-cursor-mode 0)

;;Copy/Paste to clipboard with C-w/C-y
(setq-default x-select-enable-clipboard t)
(setq-default x-select-enable-primary t)

;;Erases whole buffer
(put 'erase-buffer 'disabled nil)
;;Disable the ring bell
(setq ring-bell-function 'ignore)

;;Folding mode built-in emacs, should search for a good folding plugin
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-hl-line-mode)
;;Upcase disable
(put 'upcase-region 'disabled nil)


;;Disable Scroll-bar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
;;Disable Menu
;;(menu-bar-mode 0)
;;Disable toolbar
(tool-bar-mode 0)

;;Package.el is available after version 24 of Emacs, check for older systems like CentOS
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   )
  (package-initialize))

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;;Install use-package for the first time that emacs starts on a new system.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))
;; I should consider checking straight.el

(require 'bind-key)

(use-package spacemacs-theme
  :ensure t
  :no-require t)

(global-display-line-numbers-mode)

;;Different Color for every variable
(use-package color-identifiers-mode
  :ensure t
  :init (add-hook 'after-init-hook 'global-color-identifiers-mode))

;;Different Color for every matching {}()[]
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;Syntax checking for different languages
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package flycheck-pos-tip
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode 'c-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line))))))

;;An amazing plugin with infinite features to use.
(use-package helm
  :demand t
  :ensure t
  :init
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode t)
  :bind(
	("C-x C-f" . helm-find-files)
  	("C-x b" . helm-buffers-list)
	("M-x" . helm-M-x)
  	))

(async-bytecomp-package-mode 1)

(use-package helm-themes
  :ensure t)

;;C-h b
(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode))

(use-package cc-mode
  :config
  (setq c-default-style "linux")
  (setq c-basic-offset 8);;
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent);; indent when pressing enter
  (c-set-offset 'comment-intro 0));;indent comments according to the indentation style

;;Remember to run after installation M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;;Open files with a tree like structure
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;Autoclosing (){}[]
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t)
  :bind(

	("C-M-f" . sp-forward-sexp)
	("C-M-b" . sp-backward-sexp)

	("C-M-n" . sp-next-sexp)
	("C-M-p" . sp-previous-sexp)

	("C-S-f" . sp-forward-symbol)
	("C-S-b" . sp-backward-symbol)

	("C-M-k" . sp-kill-sexp)
	("C-k"   . sp-kill-hybrid-sexp)
	("M-k"   . sp-backward-kill-sexp)
	("C-M-w" . sp-copy-sexp)
	("C-M-d" . delete-sexp)

	("M-<backspace>" . backward-kill-word)
	("C-<backspace>" . sp-backward-kill-word)
	([remap sp-backward-kill-word] . backward-kill-word)

	("M-[" . sp-backward-unwrap-sexp)
	("M-]" . sp-unwrap-sexp)

	("C-x C-t" . sp-transpose-hybrid-sexp)))

;;Cscope is an alternative for  Ctags but much faster in big codebases.
(use-package xcscope
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

;;autocomplete plugin that is pretty fast.
(use-package company
  :ensure t
  :bind("M-TAB" . company-complete)
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (push 'company-files company-backends))


(use-package lsp-mode
  :ensure t
  :defer t)

(use-package ccls ;;cquery
  :ensure t
  :commands (lsp-ccls-enable)
  :init
  (setq ccls-executable "/home/hacker/gitfolders/ccls/release/ccls")
  ;;(setq cquery-executable "/home/hacker/gitfolders/cquery/build/release/bin/cquery")
  ;; (setq cquery-extra-args '("--log-all-to-stderr" "--log-file" "/tmp/cquery.log"))
  (add-hook 'c-mode-hook #'ccls//enable)
  (add-hook 'c++-mode-hook #'ccls//enable))

(defun ccls//enable ()
  (condition-case nil
      (lsp-ccls-enable)
    (user-error nil)))

;; loads my mu4e config and afterwards calls mu4e
(defun load-mu4e ()
  (interactive)
  (load-file "/home/hacker/Downloads/mu4e-config.el")
  (mu4e))

(use-package company-lsp
  :ensure t
  :config (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :bind
  ( :map lsp-ui-mode-map
	 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	 ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook
  (c-mode-hook . flycheck-mode)
  (lsp-mode-hook . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t))

(use-package lsp-python
  :ensure t
  :init (add-hook 'python-mode-hook #'lsp-python-enable))

;;Shows the changes that have happened to the file based on the last git commit.
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(use-package magit
  :ensure t)

;;Removes trailing whitespace and newline. I should check if emacs has a builtin mode(Probably has)
(use-package ethan-wspace
  :ensure t
  :config
  (add-hook 'after-save-hook 'ethan-wspace-clean-all)
  (global-ethan-wspace-mode 1))

(use-package markdown-mode
  :ensure t)

(use-package markdown-mode+
  :ensure t)

;;Startup screen alternative plugin
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  )

;;The famous org mode, i should learn it more.
(use-package org
  :ensure org-plus-contrib
  :bind(("C-c a" . org-agenda)
  	("C-x c" . org-capture)))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook(lambda()(org-bullets-mode 1))))

(use-package mu4e-conversation
  :ensure t
  :init
  (with-eval-after-load 'mu4e (require 'mu4e-conversation))
  (with-eval-after-load 'mu4e (global-mu4e-conversation-mode)));; Pressing V when having  a thread of emails gets nice with this plugin

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

;;M-x bug-hunter-init-file for debugging the .emacs
(use-package bug-hunter
  :ensure t)

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
 '(inhibit-startup-screen nil)
 '(markdown-command "pandoc")
 '(mode-require-final-newline nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/TODO.org")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (cquery bug-hunter org-bullets org org-plus-contrib rainbow-delimiters use-package flycheck-title magit markdown-mode markdown-mode+ git-gutter color-identifiers-mode aggressive-indent indent-guide spacegray-theme xcscope list-packages-ext helm flycheck)))
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
