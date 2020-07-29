;; Sets default directory
(setq default-directory "~/")
;; Inhibits the Emacs startup message w/ tutorial, etc..
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;; Whenever emacs asks for yes or no, just y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Start emacs with full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Adds 80 character line tweaks
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))

(add-hook 'prog-mode-hook #'whitespace-mode)

(setq-default fill-column 80)
