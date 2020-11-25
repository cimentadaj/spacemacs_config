;;; package --- Summary

;;; Commentary:

;;; Code:

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

(defun new-blog-post-blogdown ()
  "This is a comment."
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
      (find-file (concat (find-blog) "/" (car kill-ring))))
    ))

(new-blog-post-blogdown)


;;; functions.el ends here
