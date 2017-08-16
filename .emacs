(column-number-mode t)
(global-linum-mode t)
(show-paren-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(load-file (concat user-emacs-directory "~/cedet/cedet-devel-load.el"))
(load-file (concat user-emacs-directory "~/cedet/contrib/cedet-contrib-load.el"))

(defun my-c-mode-cedet-hook ()
  (setq ac-sources '( ac-source-semantic))
  (local-set-key [(control tab)] 'semantic-ia-complete-symbol-menu)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(semantic-load-enable-code-helpers)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "/home/kernel/.emacs.d/elpa/emacs-async")
(add-to-list 'load-path "/home/kernel/.emacs.d/elpa/helm")
(require 'helm-config)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

 (add-to-list 'load-path "/home/kernel/.emacs.d/neotree")
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)

;;https://github.com/capitaomorte/autopair

(add-to-list 'load-path "~/.emacs.d/plugins") ;; comment if autopair.el is in standard load path 
(require 'autopair)
(autopair-global-mode 1) ;; enable autopair in all buffers
(setq autopair-autowrap t)
(put 'upcase-region 'disabled nil)

(require 'xcscope)
(cscope-setup)

(require 'indent-guide)
(indent-guide-global-mode)

(require 'auto-complete)
(ac-config-default)

(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
(require 'semantic)

(global-ede-mode t)
(ede-enable-generic-projects)

(semantic-mode 1)
(ede-cpp-root-project "kreon" :file "/home/kernel/ITE/kreon/btree/btree.c"
					 :include-path '( "../allocator" "../BdfsBlockServer" "../debug" "../filter_ulitilities" "../HadoopDriver" "../include" "../jbtree" "../red_black_tree" "../scanner" ) )
; create a project for our program.
;(ede-cpp-root-project "my project" :file "~/test/test.c"
;					  :include-path '("~/test"))
; you can use system-include-path for setting up the system header file locations.
; turn on automatic reparsing of open buffers in semantic
;(global-semantic-idle-scheduler-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (spacegray)))
 '(custom-safe-themes
   (quote
	("d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "67a0265e2497207f5f9116c4d2bfbbab4423055e3ab1fa46ea6bd56f7e322f6a" default)))
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(nrepl-message-colors
   (quote
	("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
	(indent-guide spacegray-theme xcscope bison-mode ac-c-headers list-packages-ext helm flycheck auto-complete)))
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
