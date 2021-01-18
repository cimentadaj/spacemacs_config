;;; package -- Summary
;;; Commentary:

;;; Code:

(setq themes-packages
      '(
        doom-themes
        solaire-mode
        doom-modeline
        all-the-icones
        all-the-icones-ivy
        all-the-icones-dired
        highlight-numbers
        highlight-operators
        highlight-escape-sequences))

(defun themes/init-doom-themes ()
  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t)

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-dracula t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)

    ;; or for treemacs users
    ;; use the colorful treemacs theme
    (setq doom-themes-treemacs-theme "doom-colors")
    (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(defun themes/init-solaire-mode ()
  (use-package solaire-mode
    :ensure t
    :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
           (minibuffer-setup . solaire-mode-in-minibuffer))
    :config
    (solaire-mode-swap-bg)
    (solaire-global-mode +1)))

(defun themes/init-doom-modeline ()
  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom
    (inhibit-compacting-font-caches t)
    (doom-modeline-buffer-file-name-style 'relative-from-project)
    (doom-modeline-bar-width 1)
    (doom-modeline-modal-icon nil)
    (doom-modeline-height 15)
    (doom-modeline-env-python-executable "python3")
    :config
    (when (member "Menlo" (font-family-list))
      (set-face-attribute 'mode-line nil :height 110 :font "Menlo")
      (set-face-attribute 'mode-line-inactive nil :height 110 :font "Menlo")))
  )

(defun themes/init-all-the-icons ()
  (use-package all-the-icons
    :ensure t
    :custom
    (all-the-icons-scale-factor 1.0)
    (all-the-icons-install-fonts)
    (all-the-icons-ivy-rich-mode 1)))

(defun themes/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :ensure t
    :hook (after-init . all-the-icons-ivy-setup)
    :custom
    (all-the-icons-ivy-buffer-commands '())))

(defun themes/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode)))

(defun themes/init-highlight-numbers ()
  (use-package highlight-numbers
    :ensure t
    :hook (prog-mode . highlight-numbers-mode)))

(defun themes/init-highlight-operators ()
  (use-package highlight-operators
    :ensure t
    :hook (prog-mode . highlight-operators-mode)))

(defun themes/init-highlight-escape-sequences ()
  (use-package highlight-escape-sequences
    :ensure t
    :hook (prog-mode . hes-mode)))

;; Taken partially from https://github.com/ianpan870102/.personal-emacs.d/blob/master/init.el
;; START HERE

;; (use-package spacemacs-common
;;   :ensure spacemacs-theme
;;   :custom-face
;;   (line-number              ((t (:foreground "#414B4f" :background "#282B2E"))))
;;   (line-number-current-line ((t (:foreground "#616B6f" :background "#282B2E"))))
;;   (highlight-symbol-face    ((t (:background "#44444f"))))
;;   :custom
;;   (spacemacs-theme-comment-bg nil)
;;   (spacemacs-theme-comment-italic t)
;;   :config
;;   (load -theme 'spacemacs-dark t))

;; (use-package centaur-tabs
;;   :demand
;;   :bind (("C-S-<tab>" . centaur-tabs-backward)
;; 	 ("C-<tab>" . centaur-tabs-forward)
;; 	 ("C-x p" . centaur-tabs-counsel-switch-group))
;;   :custom
;;   (centaur-tabs-set-bar 'under)
;;   (x-underline-at-descent-line t)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker " ● ")
;;   (centaur-tabs-cycle-scope 'tabs)
;;   (centaur-tabs-height 30)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-close-button " × ")
;;   :config
;;   (centaur-tabs-mode +1)
;;   (centaur-tabs-headline-match)
;;   (centaur-tabs-group-by-projectile-project)
;;   (when (member "Arial" (font-family-list))
;;     (centaur-tabs-change-fonts "Arial" 130)))

;; (use-package highlight-symbol
;;   :ensure t
;;   :hook (prog-mode . highlight-symbol-mode)
;;   :custom
;;   (high light-symbol-idle-delay 0.3))
