;;; Package --- Summary

;;; Commentary:

;;; Code:

(defun find-blog ()
  "Return the regexp found in the known list of projectile projects."
  ;; regexp are a bit different in emacs so I struggled
  ;; a bit to find a way to build this function. The idea
  ;; is to make this function search the (projectile-relevant-known-projects)
  ;; and extract the regexp of the directory. This way you can
  ;; filter both the stats blog as your private blog.
  "~/repositories/cimentadaj.github.io")

(defun new-blog-post-R ()
  "Skeleton of R function to create new blogpost using blogdown."
  "
  my_new_post <- function(title, ...) {

  title_change <- trimws(gsub('[[:punct:]]', '', title))
  slug <- stringr::str_to_lower(gsub(' +','-', title_change))

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
      (shell-command "rm -rf /tmp/log.txt")
      (forward-line 2)
      (delete-char 5)
      (end-of-line 1)
      (backward-delete-char-untabify 1)
      (beginning-of-line 1)
      (kill-line))
    (find-file (concat (find-blog) "/" (car kill-ring)))))

;; (find-file "/home/jorge/repositories/cimentadaj.github.io/content/blog/2020-11-26-what-ever/2020-11-26-what-ever.Rmd")

;; (find-file "/home/jorge/repositories/cimentadaj.github.io/content/blog/2020-11-26-what-ever/2020-11-26-what-ever.Rmd")

(blog/new-blog-post-blogdown)

;; (find-file "/home/jorge/repositories/cimentadaj.github.io/content/blog/2020-11-26-what-ever/2020-11-26-what-ever.md")


;;; funcs.el ends here
