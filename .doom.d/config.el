;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "George Xanthakis"
      user-mail-address "kompiouterakias@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)
;;(setq debug-on-error t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
(add-hook 'cmake-mode-hook 'eldoc-cmake-enable)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'flycheck-mode  'flycheck-clang-tidy-setup)
(add-hook 'org-mode '(org-superstar-mode))
(add-hook 'after-init-hook 'global-color-identifiers-mode)
(remove-hook! doom-first-buffer-hook #'drag-stuff-global-mode)
(remove-hook! text-mode-hook #'auto-fill-mode)
(add-hook 'pdf-view-mode-hook (lambda () (pdf-midnite-amber)))
(add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
(add-hook 'prog-mode-hook 'color-identifiers-mode)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; (add-hook 'lsp-after-initialize-hook (lambda
;;                                        ()
;;                                        (flycheck-add-next-checker 'lsp 'c/c++-clang)))
;; (add-hook 'lsp-after-initialize-hook (lambda
;;                                        ()
;;                                        (flycheck-add-next-checker 'lsp 'c/c++-clang-tidy)))

(setq-default indent-tabs-mode t)
(add-hook 'lisp-mode (setq indent-tabs-mode nil))
(add-hook 'emacs-lisp-mode (setq indent-tabs-mode nil))

(defun pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1))

(defun pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
  (pdf-view-midnight-minor-mode))

(setq doom-font "Monaco 12")
(setq confirm-kill-emacs nil)

(if window-system 
    (setq doom-theme 'doom-snazzy)
  (setq doom-theme 'spacemacs-dark))

(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-modeline-code-actions-enable t)
;;(setq lsp-signature-function 'lsp-signature-posframe)

(use-package! which-function
  :defer 5
  :init(which-function-mode 1))

(use-package! ssh-config-mode
  :init
  (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/knownhosts\\'"       . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(smartparens-global-mode)
(map! :after smartparens
      :map smartparens-mode-map
      "<C-left>" #'left-word
      "<M-left>" #'left-word
      "<C-right>" #'right-word
      "<M-right>" #'right-word
      "M-]" #'sp-unwrap-sexp)

(map! "C-M-z" 'company-complete)
(map! "C-M-i" #'clang-format-buffer)
(map! [remap goto-line] #'goto-line-preview)
(map! [remap zap-to-char] #'ace-jump-zap-to-char)
(map! [remap zap-up-to-char] #'ace-jump-zap-up-to-char)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(global-set-key (kbd "<prior>") 'change-language)
(defvar change-lang 0)
(defun greek-keyboard()
  "Change keyboard language to Greek."
  (setq change-lang 1)
  (set-input-method "greek"))

(defun english-keyboard()
  "Change keyboard language to English."
  (setq change-lang 0)
  (set-input-method nil))

(defun change-language()
  "Change keyboard language."
  (interactive)
  (cond
   ((equal change-lang 1) (english-keyboard))
   ((equal change-lang 0) (greek-keyboard))))

(global-auto-revert-mode t)
(setq blink-matching-paren 'show)
(put 'narrow-to-region 'disabled nil)

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
  "Display line at POS as STR with FACE.FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
	      (goto-char pos)
	      (make-overlay (line-beginning-position)
			    (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
		 (or face '(:inherit default :inherit highlight)))
    ol))


(setq org-roam-directory "~/gitfolders/schedule-life")

(setq org-ref-default-bibliography '("~/gitfolders/schedule-life/bibliography/biblio.bib")
      org-ref-pdf-directory "~/gitfolders/schedule-life/pdfs/"
      bibtex-completion-bibliography "~/gitfolders/schedule-life/bibliography/biblio.bib" )

(setq org-ellipsis "⤵")
(setq docstr-c-style 'javadoc)
(setq docstr-key-support t)

(defun nicer-org()
  (progn
    (org-num-mode 1)
    (variable-pitch-mode 1)
    (org-superstar-mode 1)
    (ws-butler-mode nil)))

(add-hook! 'org-mode-hook 'nicer-org)
(use-package! ob-bitfield
  :after org)

;;Load mu4e configuration based on Doom's templates
(defvar mu4e-config-file "~/.emacs.d/doom_mu4e.el")

(if (file-exists-p mu4e-config-file)
    (load-file mu4e-config-file))


(setq lsp-clients-clangd-args '("-j=8"
				"--background-index"
				"--completion-style=detailed"
				"--header-insertion=iwyu"
				"--clang-tidy"
				"--function-arg-placeholders"
				"--limit-references=0"
				"--limit-results=0"))

(setq-default evil-escape-key-sequence "jk")
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq tramp-default-method "sshx")
;; (after! lsp (lsp-register-client
;; 	      (make-lsp-client :new-connection (lsp-tramp-connection "/usr/bin/clangd")
;; 			       :major-modes '(c-mode c++-mode)
;; 			       :remote? t
;; 			       :server-id 'clangd-remote)))

(map! [remap dabbrev-expand] 'hippie-expand)

(after! lsp-treemacs (lsp-treemacs-sync-mode 1))
(after! flycheck 'flycheck-clang-tidy)
(after! lsp-ui (setq lsp-ui-doc-show-with-cursor t))
(after! magit (setq magit-diff-refine-hunk 'all))
(after! magit (setq git-commit-summary-max-length 72))
(after! company (setq company-idle-delay nil))

;;(setq debug-on-message "vacuous schema")

(use-package! sideline
  :hook (flycheck-mode . sideline-mode)
  :init
  (setq	sideline-order-left 'down              ; or 'up
	sideline-order-right 'up               ; or 'down
	sideline-format-left "%s   "           ; format for left aligment
	sideline-format-right "   %s"          ; format for right aligment
	sideline-priority 100                  ; overlays' priority
	sideline-display-backend-name t)
  (setq sideline-backends-right '(sideline-lsp sideline-flycheck sideline-flymake))

;; (setq sideline-backends-right '(sideline-flycheck)
;;         ;;sideline-backends-right '(sideline-lsp)
;; 	)
  )      ; display the backend name
(use-package! sideline-lsp
  :init
  (setq sideline-lsp-update-mode 'line))
(use-package! sideline
  :hook (flymake-mode-hook . sideline-mode)
  :init
  (setq sideline-flymake-display-errors-whole-line 'line))

(use-package! lsp-mode :hook (lsp-mode-hook . sideline-mode))
(use-package! lsp-ui :init (setq lsp-ui-sideline-enable nil))  ; disable original sideline
(use-package! sideline-flycheck :hook (flycheck-mode-hook . sideline-flycheck-setup))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
