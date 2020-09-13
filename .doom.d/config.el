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
(add-hook! message-mode-hook word-wrap-mode)
(add-hook! after-init-hook global-color-identifiers-mode)
(add-hook! cmake-mode-hook cmake-font-lock-activate)
(add-hook! cmake-mode-hook eldoc-cmake-enable)
(add-hook! prog-mode-hook goto-address-prog-mode)
(add-hook! text-mode-hook goto-address-mode)
(add-hook! flycheck-mode  flycheck-clang-tidy-setup)
(remove-hook! doom-first-buffer-hook #'drag-stuff-global-mode)
(remove-hook! text-mode-hook #'auto-fill-mode)
(add-hook 'lsp-after-initialize-hook (lambda
                                       ()
                                       (flycheck-add-next-checker 'lsp 'c/c++-clang)))
(add-hook 'lsp-after-initialize-hook (lambda
                                       ()
                                       (flycheck-add-next-checker 'lsp 'c/c++-clang-tidy)))
(add-hook 'pdf-view-mode-hook (lambda () (bms/pdf-midnite-amber)))

(defun bms/pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1))

(defun bms/pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
  (pdf-view-midnight-minor-mode))

(setq doom-font "Monaco-12")
(setq doom-theme 'doom-snazzy)
(setq lsp-headerline-breadcrumb-enable t)
(which-function-mode 1)
(smartparens-global-mode)
(smartparens-global-strict-mode)
(beacon-mode 1)
(map! :after smartparens
      :map smartparens-mode-map
      "<C-left>" #'left-word
      "<M-left>" #'left-word
      "<C-right>" #'right-word
      "<M-right>" #'right-word
      "M-]" #'sp-unwrap-sexp)

(map! "C-x k" #'bjm/kill-this-buffer)
(map! [remap goto-line] #'goto-line-preview)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
(whole-line-or-region-global-mode)
(setq blink-matching-paren 'show)
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

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


(defvar home-dir (getenv "HOME"))
(when (file-directory-p (concat home-dir "/gitfolders/mu4e_setup/"))
  (defun load-mu4e ()
    "Load my mu4e configuration and afterwards call mu4e."
    (interactive)
    (defvar mu4e-config (concat home-dir "/gitfolders/mu4e_setup/mu4e-config.el"))
    (load-file mu4e-config)
    (mu4e)))

(use-package! flycheck-clang-tidy
  :after flycheck)

(after! (flycheck gitlab-ci-mode)
  (gitlab-ci-mode-flycheck-enable))


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
