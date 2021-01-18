;; `used-only' installs only explicitly used packages and uninstall any
;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     sql
     nginx
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     csv
     ivy
     (auto-completion :variables
                      auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-use-company-box t)
     better-defaults
     emacs-lisp
     shell-scripts
     (shell :variables shell-default-shell 'eshell)
     git
     markdown
     html
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            org-todo-keywords' ((sequence "TODO" "IN PROCESS" "WAITING"
                                          "|" "DONE"))
            org-log-done 'time
            org-log-done 'note)
     ;; (spell-checking :variables spell-checking-enable-by-default nil)
     ;; (syntax-checking :variables syntax-checking-enable-by-default nil)
     (spell-checking :variables
                     spell-checking-enable-auto-dictionary t
                     )
     (syntax-checking :variables
                      flycheck-check-syntax-automatically (quote (save idle-change mode-enabled))
                      flycheck-idle-change-delay 4 ;; Set delay based on what suits you the best
                      syntax-checking-enable-tooltips nil
                      )
     ipython-notebook
     ;; rust
     yaml
     docker
     ;; tmux
     c-c++
     ;; (scala :variables
     ;;        scala-backend 'scala-metals)
     ;; version-control
     ;; latex
     ;; extra-langs
     ;; private layers
     pythonp
     (ess :variables
          ;; ess-enable-smart-equals t
          ess-enable-electric-spacing-r t
          ess-enable-smartparens t
          ess-r-backend 'lsp)
     funk
     blog
     polymode
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(doom-modeline
                                      all-the-icons
                                      all-the-icons-ivy-rich
                                      all-the-icons-dired
                                      solaire-mode
                                      ivy-rich
                                      expand-region
                                      helm-projectile
                                      helm-ag
                                      solarized-theme
                                      poly-R
                                      poly-noweb
                                      poly-markdown
                                      yasnippet-snippets
                                      impatient-mode
                                      flycheck-julia)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-one
                         spacemacs-light
                         solarized-dark
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too
   ;; crappy.
   ;; "IBM Plex Mono" or "Source Code Pro"
   dotspacemacs-default-font '("Source Code Pro"
                               :size 17
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;; Text settings =====================================================
  (setq-default
   ;; Break lines at specified column (<= 80, defaults 72)
   fill-column 90
   ;; Turns on auto-fill-mode to automatically break lines
   ;; auto-fill-function 'do-auto-fill
   ;; Makes the kill-ring (emacs clipboard) to store only 4 entries,
   ;; otherwise it may flush memory
   kill-ring-max 50
   whitespace-line-column 80
   whitespace-style '(face lines-tail)
   tramp-verbose 6
   )

  ;; Sets default directory
  (setq default-directory "~/")
  ;; Inhibits the Emacs startup message w/ tutorial, etc..
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)

  ;; Whenever emacs asks for yes or no, just y or n
  (fset 'yes-or-no-p 'y-or-n-p)

  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1)

  (setq-default
   ;; doom-modeline
   inhibit-compacting-font-caches t
   doom-modeline-buffer-file-name-style 'relative-from-project
   doom-modeline-bar-width 1
   doom-modeline-modal-icon nil
   doom-modeline-height 15
   doom-modeline-env-python-executable "python3"
   ;; all-the-icons
   all-the-icons-scale-factor 1.0
   all-the-icons-install-fonts
   )

  ;; Custom doom-modeline
  (doom-modeline-mode 1)

  (when (member "Menlo" (font-family-list))
    (set-face-attribute 'mode-line nil :height 110 :font "Menlo")
    (set-face-attribute 'mode-line-inactive nil :height 110 :font "Menlo"))

  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)

  (add-hook 'shell-mode-hook 'company-mode)

  ;; ;; Enable solaire-mode anywhere it can be enabled
  ;; (solaire-global-mode +1)
  ;; (solaire-mode-swap-bg)

  ;; ;; To enable solaire-mode unconditionally for certain modes:
  ;; (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  ;; ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; ;; itself off every time Emacs reverts the file
  ;; (add-hook 'after-revert-hook #'turn-on-solaire-mode)

  ;; ;; highlight the minibuffer when it is activated:
  ;; (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; ;; NOTE: This is necessary for themes in the doom-themes package!
  ;; (solaire-mode-swap-bg)

  ;; Start emacs with full screen
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Show line numbering on all buffers
  ;; Ideally, I'd like only line numbering
  ;; on the left buffers but I don't know
  ;; how to do it.
  (global-display-line-numbers-mode)

  ;; Set lines to continue if they're too long instead of
  ;; continuing them in the next line
  (setq-default truncate-lines t)
  ;; (setq toggle-truncate-lines t)
  ;; (setq truncate-partial-width-windows nil)


  ;; Flycheck hook for different modes
  (add-hook 'ess-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)

  (flycheck-julia-setup)
  (add-to-list 'flycheck-global-modes 'julia-mode)
  (add-to-list 'flycheck-global-modes 'ess-julia-mode)

  ;; (define-key 'company-active-map (kbd "TAB") #'company-indent-or-complete-common)

  (projectile-discover-projects-in-directory "~/repositories/")

  ;; Expand-region to delete when writing under selection
  (pending-delete-mode t)

  ;; For highlighting in red after 80th column
  (add-hook 'prog-mode-hook #'whitespace-mode)

  ;; Open up first file as inbox
  ;; (find-file "~/google_drive/gtd/inbox.org")

  ;; For evaluation emacs-lisp code chunk in org mode
  ;; (define-key 'org-mode-map (kbd "M-n") 'eval-region)

  (with-eval-after-load 'sh-script
    (define-key sh-mode-map (kbd "C-c C-b") 'executable-interpret))
  ;; (define-key sh-mode-map (kbd "C-<") 'tide-insert-assign))

  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "M-k") 'kill-region))

  (setq lsp-enable-snippet nil)

  (define-key emacs-lisp-mode-map (kbd "C-c C-v") 'describe-function)

  (setq flycheck-flake8rc "~/.spacemacs.d/private/pythonp/.flake8")
  (setq flycheck-pylintrc "~/.spacemacs.d/private/pythonp/.pylintrc")

  ;; Company

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-m") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous))

  (require 'ansi-color)
  (defun my-colorize-buffer-window (win)
    (ansi-color-apply-on-region (window-start win) (window-end win t)))

  (defun my-colorize-buffer (win _start)
    (mapc #'my-colorize-buffer-window (get-buffer-window-list (window-buffer win) nil 'visible)))

  (add-hook 'company-show-doc-buffer 'my-colorize-buffer)

  ;; (defun my-comint-shorten-long-lines (text)
  ;;   (let* ((regexp "^\\(.\\{80\\}\\).*?\\(\"?\\)$")
  ;;          (shortened-text (replace-regexp-in-string regexp "\\1\\2" text)))
  ;;     (if (string= shortened-text text)
  ;;         text
  ;;       (propertize shortened-text 'font-lock-face 'shadow 'help-echo text))))

  ;; (add-hook 'comint-preoutput-filter-functions 'my-comint-shorten-long-lines)

  ;; (defun my-comint-shorten-long-lines (text)
  ;;   (let* ((regexp "^\\(.\\{80\\}\\).*?$")
  ;;          (shortened-text (replace-regexp-in-string regexp "\\1" text)))
  ;;     (if (string= shortened-text text)
  ;;         text
  ;;       (propertize shortened-text 'help-echo text))))

  ;; (add-hook 'comint-preoutput-filter-functions 'my-comint-shorten-long-lines)

  (setq company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-idle-delay 0.05
        company-minimum-prefix-length 2
        company-tooltip-limit 10)

  (setq company-auto-complete 'company-explicit-action-p)

  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; When scrolling with the cursor, show 4 lines above/below
  (setq scroll-margin 5)
  ;; Deactivate scroll margin in terminals
  (defun spacemacs//unset-scroll-margin ()
    (setq-local scroll-margin 0))

  (spacemacs/add-to-hooks
   'spacemacs//unset-scroll-margin
   '(
     messages-buffer-mode-hook
     comint-mode-hook
     term-mode-hook
     erc-mode-hook
     inferior-ess-mode-hook
     eshell-mode-hook
     inferior-python-mode-hook
     ))
  ;; Turn on FCI (Fill Column Indicator) mode
                                        ; (turn-on-fci-mode)
  ;; Maxima mode https://www.emacswiki.org/emacs/MaximaMode
  ;; (add-to-list 'load-path "/usr/local/share/maxima/5.18.1/emacs/")
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
  (setq imaxima-use-maxima-mode-flag t)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))
  ;; (add-hook 'prog-mode-hook 'column-enforce-mode) ;; For highlighting in red
  ;; Automatically set to 80
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (32 40 41 46)))
 '(company-begin-commands
   (quote
    (self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash)))
 '(company-idle-delay 0.05)
 '(company-tooltip-idle-delay 0.05)
 '(ein:output-area-inlined-images t)
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-lintr-linters
   "with_defaults(trailing_blank_lines_linter = NULL, object_usage_linter = NULL, camel_case_linter = NULL, object_name_linter = NULL)")
 '(package-selected-packages
   (quote
    (so-long flycheck-julia company-box frame-local ess-smart-equals ess ess-R-data-view julia-repl julia-mode simple-httpd nov phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode impatient-mode org-noter disaster company-c-headers cmake-mode clang-format sql-indent scala-mode sbt-mode noflet dash poly-rst yasnippet-snippets nginx-mode all-the-icons-ivy-rich shrink-path doom-dracula-theme highlight-escape-sequences highlight-operators all-the-icons-dired all-the-icons-ivy doom-modeline solaire-mode doom-custom-theme memoize all-the-icons doom-modeline-theme insert-shebang fish-mode company-shell powerline poly-noweb poly-markdown org-category-capture alert log4e gntp org-plus-contrib magit-popup hydra lv dash-functional parent-mode helm helm-core haml-mode flx highlight magit git-commit smartparens iedit anzu evil goto-chg undo-tree ctable pyvenv highlight-indentation with-editor exec-path-from-shell polymode deferred request anaphora websocket transient tablist json-mode docker-tramp json-snatcher json-reformat projectile pkg-info epl counsel swiper ivy web-completion-data pos-tip company markdown-mode rust-mode bind-map bind-key yasnippet packed auctex async spinner anaconda-mode pythonic f s avy auto-complete popup flyspell-correct flycheck helm-themes helm-swoop helm-mode-manager helm-flx helm-descbinds ace-jump-helm-line yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill toml-mode toc-org thrift tagedit stan-mode spaceline solarized-theme smex smeargle slim-mode shell-pop scss-mode scad-mode sass-mode restart-emacs rainbow-delimiters racer qml-mode pytest pyenv-mode py-isort pug-mode popwin poly-R pip-requirements persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file neotree mwim multi-term move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lorem-ipsum live-py-mode linum-relative link-hint ivy-hydra indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers helm-projectile helm-make helm-ag google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-ivy flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elpy elisp-slime-nav ein dumb-jump doom-themes dockerfile-mode docker diminish define-word cython-mode csv-mode counsel-projectile company-web company-statistics company-quickhelp company-auctex company-anaconda column-enforce-mode clean-aindent-mode cargo auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
