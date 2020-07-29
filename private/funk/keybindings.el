;;======================================================================
;; Keybindings only
;;======================================================================

;; C-TAB move between buffers
(global-set-key [(control tab)] 'other-window)

;; Change font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Remaps kill this buffer to familiar CTRL + W
(global-set-key [(control w)] 'kill-this-buffer)
(global-set-key (kbd "M-k")  'kill-region)
(global-set-key (kbd "M-k")  'kill-region)

;; Reload buffer without reopening with F5
(global-set-key (kbd "<f5>") 'revert-buffer)

;; From scratch
(global-set-key (kbd "C-x 9") 'R-scratch)
