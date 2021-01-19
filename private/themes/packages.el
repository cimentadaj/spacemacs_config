;;; package -- Summary
;;; Commentary:

;;; Code:

(setq themes-packages
      '(
        doom-themes
        solaire-mode
        all-the-icons
        all-the-icons-ivy
        all-the-icons-dired
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

(defun themes/post-init-all-the-icons ()
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

(defun themes/post-init-highlight-numbers ()
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
