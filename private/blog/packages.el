;;; packages.el --- blog layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jorge <jorge@cimentadaj-Apollo>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `blog-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `blog/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `blog/pre-init-PACKAGE' and/or
;;   `blog/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst blog-packages
  '()
  "The list of Lisp packages required by the blog layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun find-blog ()
  "Returns the regexp found in the known list of
   projectile projects."
  ;; regexp are a bit different in emacs so I struggled
  ;; a bit to find a way to build this function. The idea
  ;; is to make this function search the (projectile-relevant-known-projects)
  ;; and extract the regexp of the directory. This way you can
  ;; filter both the stats blog as your private blog.
  "~/repositories/cimentadaj.github.io")

(defun new-blog-post-R ()
  "Skeleton of R function to create new blogpost using blogdown"
  "
  my_new_post <- function(title, ...) {

  title_change <- trimws(gsub('[[:punct:]]', '', title))
  slug <- stringr::str_to_lower(gsub('\\\\\\\\s','-', title_change))

  dir <- paste0(lubridate::today(), '-', slug)

  blogdown::new_post(title = title,
                     author = 'Jorge Cimentada',
                     subdir = file.path('./blog', dir),
                     ext = '.Rmd',
                     ...)
  }")

(defun blog/new-blog-post-blogdown ()
"Run cimentadaj::my_new_post() on an argument read from the mini buffer.
This effectively creates the Rmd file on the result of (find-blog) creating
all the necessary folders in between. Finally, it opens the file as a new buffer."
  (interactive)
  (let* ((name-blog-post (read-from-minibuffer "Name of the blogpost: "))
         (setwd-dir (concat "setwd('" (find-blog) "')"))
         (new-post-expression (concat "my_new_post('" name-blog-post "')"))
         (incomplete-expression (concat setwd-dir ";" (new-blog-post-R) ";" new-post-expression))
         (entire-expression (concat "Rscript -e " (prin1-to-string incomplete-expression))))
    ;; Run rscript expression and save to log.txt
    (shell-command (concat entire-expression "> /tmp/log.txt"))
    (with-temp-buffer
      (insert-file-contents "/tmp/log.txt")
      (forward-line 2)
      (delete-char 1)
      (delete-char 1)
      (delete-char 1)
      (delete-char 1)
      (delete-char 1)
      (mwim-end-of-line)
      (backward-delete-char-untabify 1)
      (mwim-beginning-of-line)
      (kill-line)
      (shell-command "rm -rf /tmp/log.txt")
      (find-file (concat (find-blog) "/" (car kill-ring))))
    ))

;;; packages.el ends here
