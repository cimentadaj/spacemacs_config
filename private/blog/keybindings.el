;;; package -- summary

;;; Commentary:

;;; Code:

(spacemacs/declare-prefix "P" "blog")
(spacemacs/declare-prefix "Ps" "stats")
(spacemacs/declare-prefix "Pp" "personal")

(define-key global-map (kbd "M-m P s n ") 'blog/new-blog-post-blogdown)
(define-key global-map (kbd "M-m P s p ") 'blog/preview-blog-post-blogdown)
(define-key global-map (kbd "M-m P s P ") 'blog/publish-blog-post-blogdown)
(define-key global-map (kbd "M-m P s b ") 'blog/open-blog-blogdown)

(define-key global-map (kbd "M-m P p n ") 'blog/new-blog-post-personal)
(define-key global-map (kbd "M-m P p p ") 'blog/preview-blog-post-personal)
(define-key global-map (kbd "M-m P p P ") 'blog/publish-blog-post-personal)
(define-key global-map (kbd "M-m P p b ") 'blog/open-blog-personal)

;;; keybindings.el ends here
