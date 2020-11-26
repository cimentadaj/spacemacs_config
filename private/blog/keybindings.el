(spacemacs/declare-prefix "P" "blog")
(spacemacs/declare-prefix "Ps" "stats")

(define-key global-map (kbd "M-m P s n ") 'blog/new-blog-post-blogdown)
(define-key global-map (kbd "M-m P s p ") 'blog/preview-blog-post-blogdown)
(define-key global-map (kbd "M-m P s P ") 'blog/publish-blog-post-blogdown)

;;; keybindings.el ends here
