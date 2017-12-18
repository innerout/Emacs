(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/elpa/emacs-async")
(add-to-list 'load-path "~/.emacs.d/elpa/helm")
(column-number-mode t)
(show-paren-mode 1)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   '("elpy" . "https://jorgenschaefer.github.io/packages/"))
  (package-initialize)
  )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package))
  )

(require 'bind-key)

(use-package spacemacs-theme
  :ensure t
  :no-require t
  )

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode 'flycheck-mode))
  )

(use-package linum
  :config
  (global-linum-mode t)
  (setq linum-format "%d ")
  )

(use-package color-identifiers-mode
  :ensure t
  :init (global-color-identifiers-mode)
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  )

(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode 'c-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line)))))
  )

(use-package helm
  :ensure t
  :bind(("C-x C-f" . helm-find-files)
	("C-x b" . helm-buffers-list)
	)
  )

(use-package cc-mode
  :config
  (setq c-default-style "linux")
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  )

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle)
  )

(use-package origami
  :ensure t
  :init (global-origami-mode)
  :bind( ("C-c v o" . origami-open-node)
	 ("C-c v c" . origami-close-node)
	 ("C-c v n" . origami-next-fold)
	 ("C-c v p" . origami-previous-fold)
	 ("C-c v 0" . origami-close-all-nodes)
	 ("C-c v 1" . origami-open-all-nodes)
         )
  )

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode 1) ;; enable autopair in all buffers
  (setq autopair-autowrap t)
  )


(put 'upcase-region 'disabled nil)

(use-package xcscope
  :ensure t
  :config
  (cscope-setup)
  (setq cscope-option-use-inverted-index t)
  )

(use-package indent-guide
  :ensure t
  :init(indent-guide-global-mode)
  )
;; (ede-enable-generic-projects )
(semantic-mode 1)
(global-semantic-highlight-func-mode t)
;;(global-semantic-idle-scheduler-mode t)
;; (semantic-load-enable-code-helpers)


(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  )

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  :config
  (defun ac-common-setup()
    (setq ac-sources (append ac-sources '( ac-source-semantic ac-source-semantic-raw ac-source-c-headers ac-source-filename)))
    )
  )

(use-package ac-c-headers
  :ensure t
  :config
  (add-hook 'c-mode-hook
	    (lambda ()
	      (add-to-list 'ac-sources 'ac-source-c-header-symbols t))
	    )
  )

;;git-gutter
(use-package git-gutter
  :ensure t
  :init
  (git-gutter:linum-setup)  ;; to use git-gutter.el and linum-mode
  (global-git-gutter-mode +1)  ;; If you enable global minor mod
  :config
  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x c p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x c n") 'git-gutter:next-hunk)
  )

(use-package magit
  :ensure t
  )

(use-package ethan-wspace
  :ensure t
  :config
  (global-ethan-wspace-mode 1)
  (add-hook 'after-save-hook 'ethan-wspace-clean-all)
  )

(use-package markdown-mode
  :ensure t
  )

(use-package markdown-mode+
  :ensure t
  )

(use-package flycheck-pos-tip
  :ensure t)
(async-bytecomp-package-mode 1)

;; (ede-cpp-root-project "kreon" :file "/home/hacker/HEutropia/kreon/btree/btree.c"
;; 					 :include-path '( "../allocator" "../BdfsBlockServer" "../debug" "../filter_ulitilities" "../HadoopDriver" "../include" "../jbtree" "../red_black_tree" "../scanner" ) )
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
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (flycheck-title elpy magit markdown-mode markdown-mode+ git-gutter origami color-identifiers-mode neotree aggressive-indent yasnippet-snippets indent-guide spacegray-theme xcscope bison-mode ac-c-headers list-packages-ext helm flycheck)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
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
 )
(put 'erase-buffer 'disabled nil)
;;;;
