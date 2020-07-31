;;======================================================================
;; Keybindings only
;;======================================================================

;; C-TAB move between buffers
(global-set-key [(control tab)] 'other-window)

;; Remaps kill this buffer to familiar CTRL + W
(global-set-key [(control w)] 'kill-this-buffer)
(global-set-key (kbd "M-k")  'kill-region)
(global-set-key (kbd "M-k")  'kill-region)

;; Reload buffer without reopening with F5
(global-set-key (kbd "<f5>") 'revert-buffer)

;; From scratch
(global-set-key (kbd "C-x 9") 'R-scratch)

;; Adds two character moving around
(global-set-key (kbd "M-s s") 'avy-goto-char-2)

;; Allows for easy searching in projects
(global-set-key (kbd "M-m p s s") 'helm-projectile-ag)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(global-set-key (kbd "<M-down>") 'forward-paragraph)
(global-set-key (kbd "<M-up>") 'backward-paragraph)

(global-set-key (kbd "C-x C-a C-a") 'org-archive-done-tasks)

(bind-key "TAB" #'indent-for-tab-command)
(bind-key "M-i" #'company-complete)

(defalias 'list-buffers 'ibuffer)
