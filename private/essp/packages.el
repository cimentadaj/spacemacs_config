
;; packages.el --- ESS (R) Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq essp-packages
      '(
        ess
        ess-smart-equals
        golden-ratio
        org
        julia-mode))

(defun essp/init-ess ()
  (use-package ess-site
    :mode (("\\.sp\\'"           . S-mode)
           ("/R/.*\\.q\\'"       . R-mode)
           ("\\.[qsS]\\'"        . S-mode)
           ("\\.ssc\\'"          . S-mode)
           ("\\.SSC\\'"          . S-mode)
           ("\\.[rR]\\'"         . R-mode)
           ("\\.[rR]nw\\'"       . Rnw-mode)
           ("\\.[sS]nw\\'"       . Snw-mode)
           ("\\.[rR]profile\\'"  . R-mode)
           ("NAMESPACE\\'"       . R-mode)
           ("CITATION\\'"        . R-mode)
           ("\\.omg\\'"          . omegahat-mode)
           ("\\.hat\\'"          . omegahat-mode)
           ("\\.lsp\\'"          . XLS-mode)
           ("\\.do\\'"           . STA-mode)
           ("\\.ado\\'"          . STA-mode)
           ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
           ("\\.jl\\'"           . ess-julia-mode)
           ("\\.[Ss]t\\'"        . S-transcript-mode)
           ("\\.Sout"            . S-transcript-mode)
           ("\\.[Rr]out"         . R-transcript-mode)
           ("\\.Rd\\'"           . Rd-mode)
           ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
           ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
           ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
           ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
           ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
           ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
    :commands (R stata julia SAS)
    :init
    (progn
      (spacemacs/register-repl 'ess-site 'julia)
      (spacemacs/register-repl 'ess-site 'R)
      (spacemacs/register-repl 'ess-site 'SAS)
      (spacemacs/register-repl 'ess-site 'stata)
      ;; Explicitly run prog-mode hooks since ess-mode does not derive from
      ;; prog-mode major-mode
      (add-hook 'ess-mode-hook 'spacemacs/run-prog-mode-hooks)
      (when (configuration-layer/package-usedp 'company)
        (add-hook 'ess-mode-hook 'company-mode)
        (add-hook 'ess-julia-mode-hook 'company-mode)
        (add-hook 'inferior-ess-mode-hook 'company-mode)
        (add-hook 'inferior-ess-julia-mode-hook 'company-mode))))

  ;; R --------------------------------------------------------------------------
  (setq spacemacs/ess-config
    '(progn
    ;; Follow Hadley Wickham's R style guide
    (setq ess-first-continued-statement-offset 2
          ess-continued-statement-offset 2
          ess-expression-offset 2
          ess-nuke-trailing-whitespace-p t
          ess-default-style 'DEFAULT
          ess-indent-offset 2)
    (defun spacemacs/ess-start-repl ()
      "Start a REPL corresponding to the ess-language of the current buffer."
      (interactive)
      (cond
       ((string= "S" ess-language) (call-interactively 'R))
       ((string= "STA" ess-language) (call-interactively 'stata))
       ((string= "SAS" ess-language) (call-interactively 'SAS))))

    (spacemacs/declare-prefix-for-mode 'ess-r-mode "md" "package dev")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "mv" "view data")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "ms" "session (REPL)")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "mc" "chunks")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "mg" "graphics")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "mr" "rmarkdown")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "mS" "shiny")
    (spacemacs/declare-prefix-for-mode 'ess-r-mode "m/" "extra")

    (spacemacs/set-leader-keys-for-major-mode 'ess-julia-mode
      "'"  'julia
      "si" 'julia)
    (spacemacs/set-leader-keys-for-major-mode 'ess-r-mode
      "'"  'spacemacs/ess-start-repl
      "si" 'spacemacs/ess-start-repl
      ;; noweb
      "cC" 'ess-eval-chunk-and-go
      "cc" 'ess-eval-chunk
      "cd" 'ess-eval-chunk-and-step
      "cm" 'ess-noweb-mark-chunk
      "cN" 'ess-noweb-previous-chunk
      "cn" 'ess-noweb-next-chunk
      ;; REPL
      "sB" 'ess-eval-buffer-and-go
      "sb" 'ess-eval-buffer
      ","  'ess-eval-region-or-function-or-paragraph-and-step
      "e" 'ess-eval-paragraph-and-step
      "f" 'ess-eval-function
      "i" 'ess-interrupt
      "o"  'ess-eval-word
      "l" 'ess-eval-line
      "R" 'ess-eval-region
      "sp" 'ess-eval-paragraph-and-step
      "sd" 'ess-eval-region-or-line-and-step
      "sr" 'ess-eval-region
      "st" 'ess-eval-function
      ;; Extra stuff
      "/c" 'ess-comint-clean-buffer
      "/r" 'inferior-ess-reload
      "/e" 'end-of-defun
      "/a" 'beginning-of-defun
      "/l" 'ess-load-library
      "/w" 'ess-set-working-directory
      "/h" 'ess-display-help-on-object
      ;; R data viewers
      "vs" 'df-sample-small
      "vm" 'df-sample-medium
      "vl" 'df-sample-large
      ;; Package Dev helpers
      "di" 'ess-r-devtools-install-package
      "dt" 'ess-r-devtools-test-package
      "dl" 'ess-r-devtools-load-package
      "dc" 'ess-r-devtools-check-package
      "dd" 'ess-r-devtools-document-package
      "df" 'ess-roxy-update-entry
      "ds" 'tide-devtools-setup
      ;; R help
      "ho" 'ess-display-help-on-object
      "hi" 'ess-display-index
      "ha" 'ess-display-help-apropos
      ;; Graphics devices
      "gn" 'tide-new-gdev
      "gc" 'tide-cur-gdev
      "gs" 'tide-switch-to-gdev
      "gl" 'tide-list-all-gdev
      "gp" 'tide-save-gdev-pdf
      "gc" 'tide-capture-gdev
      "gj" 'tide-switch-next-gdev
      "gk" 'tide-switch-prev-gdev
      ;; R Markdown
      "rc" 'insert-chunk
      "rr" 'tide-rmd-rend
      "rd" 'tide-draft-rmd
      ;; Shiny
      "Sr" 'tide-shiny-run-app
      )

    (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
    ;; (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
    ;; (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)
    ;; Toggle underscore off no replacement of _ for <-
    (setq ess-smart-S-assign-key nil)
    ;; Stop R repl eval from blocking emacs.
    (setq ess-eval-visibly 'nowait)
    ;; Set fontification
    (setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:keywords   . t)
        (ess-R-fl-keyword:constants  . t)
        (ess-R-fl-keyword:modifiers  . t)
        (ess-R-fl-keyword:fun-defs   . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:%op%       . t)
        (ess-fl-keyword:fun-calls)
        (ess-fl-keyword:numbers)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters)
        (ess-fl-keyword:=)
        (ess-R-fl-keyword:F&T)))
    ;; enable prettify symbols
    (add-hook 'ess-mode-hook 'prettify-symbols-mode)

    ;; =====================================================================
    ;; Tidyverse IDE
    ;; =====================================================================

    (defun tide-insert-pipe ()
      "Insert a %>% and newline"
      (interactive)
      (insert " %>% "))

    (defun tide-insert-assign ()
      "Insert an assignment <-"
      (interactive)
      (insert " <- "))

    (defun ess-comint-clean-buffer ()
      "Applies comint-clear-buffer to the inferior process
       associated with a buffer. If you're in buffer '*a*' and run this,
       it will switch to the associated inferior buffer apply
       comint-clear-buffer and switch back to *a*"
      (interactive)
      (let ((first-buffer (current-buffer)))
        (ess-switch-to-inferior-or-script-buffer t)
        (comint-clear-buffer)
        (switch-to-buffer-other-window first-buffer)))

    (define-key ess-r-mode-map (kbd "C->") 'tide-insert-pipe)
    (define-key ess-r-mode-map (kbd "C-<") 'tide-insert-assign)
    (define-key inferior-ess-r-mode-map (kbd "C->") 'tide-insert-pipe)
    (define-key inferior-ess-r-mode-map (kbd "C-<") 'tide-insert-assign)
    (define-key ess-mode-map (kbd "C-j") 'ess-eval-line-and-step)

    ;; (defun ess-eval-line-function-or-paragraph (&optional vis)
    ;;   "Send the current function if \\[point] is inside one.
    ;;   Otherwise send the current paragraph to the inferior ESS process.
    ;;   Prefix arg VIS toggles visibility of ess-code as for
    ;;   `ess-eval-region'. Returns 'function if a function was evaluated
    ;;   or 'paragraph if a paragraph."
    ;;   (interactive "P")
    ;;   (condition-case nil
    ;;       (progn
    ;;         (ess-eval-line vis) 'line
    ;;         (ess-eval-function vis) 'function
    ;;         (ess-eval-paragraph vis) 'paragraph)))

    (defun tide-draft-rmd ()
      "Draft a new Rmd file from a template interactively."
      (interactive)
      (setq rmd-file
            (read-from-minibuffer "Rmd Filename (draft_<date>.Rmd): "
                                  nil nil t t
                                  (format "draft_%s.Rmd"
                                          (string-trim
                                           (shell-command-to-string "date --iso-8601")))))
      (setq rmd-template
            (read-from-minibuffer
             (format "Draft %s from template (mmmisc/basic): " rmd-file)
                                               nil nil t t "mmmisc/basic"))
      (symbol-name rmd-template)
      (string-match "\\([^/]+\\)/\\([^/]+\\)"
                    (symbol-name rmd-template))
      (setq template-pkg
            (substring
             (symbol-name rmd-template)
             (match-beginning 1)
             (match-end 1)))
      (setq template-name
            (substring
             (symbol-name rmd-template)
             (match-beginning 2)
             (match-end 2)))
      (message "Drafting using template %s from package %s" template-name template-pkg)
      (ess-eval-linewise
       (format "rmarkdown::draft(file = \"%s\", template = \"%s\",
                package = \"%s\", edit = FALSE)"
               rmd-file template-name template-pkg))
      )
    ;; Graphics device management ;;
    (defun tide-new-gdev ()
      "create a new graphics device"
      (interactive)
      (ess-eval-linewise "dev.new()"))

    (defun tide-cur-gdev ()
      "return current graphics device"
      (interactive)
      (ess-eval-linewise "dev.cur()"))

    (defun tide-list-all-gdev ()
      "list all graphics devices"
      (interactive)
      (ess-eval-linewise "dev.list()"))

    (defun tide-switch-to-gdev ()
      "Prompt for the number of the graphics device to make current"
      (interactive)
      (setq dev-num
            (read-from-minibuffer "Select R graphics device: "
                                  nil nil t t "1"))
      (ess-eval-linewise
       (format "dev.set(%s)" dev-num)))

    (defun tide-switch-next-gdev ()
      "switch to next available graphics device"
      (interactive)
      (ess-eval-linewise "dev.set(dev.next())"))

    (defun tide-switch-prev-gdev ()
      "switch to previous available graphics device"
      (interactive)
      (ess-eval-linewise "dev.set(dev.prev())"))

    (defun tide-save-gdev-pdf ()
      "Save current graphics device as pdf"
      (interactive)
      (ess-eval-linewise "dev.copy2pdf()"))

    (defun tide-capture-gdev ()
      "Capture current graphics device as image"
      (interactive)
      (ess-eval-linewise "dev.capture()"))

    ;; Devtools
    (defun tide-devtools-setup ()
      "setup R package in current working directory"
      (interactive)
      (ess-eval-linewise "devtools::setup()"))

    ;; Shiny
    (defun tide-shiny-run-app ()
      "Run a shiny app in the current working directory"
      (interactive)
      (ess-eval-linewise "shiny::runApp()"))

    ;; Rmarkdowm
    (defun tide-rmd-rend ()
      "Render rmarkdown files with an interactive selection prompt"
      (interactive)
      (ess-eval-linewise "mmmisc::rend()"))

    (defun pos-paragraph ()
      (backward-paragraph)
      (next-line 1)
      (beginning-of-line)
      (point))

    (defun highlight-piped-region ()
      (let ((end (point))
            (beg (pos-paragraph)))
          (set-mark beg)
          (goto-char end)
          (end-of-line)
          (deactivate-mark)
          (setq last-point (point))
          (goto-char end)
          (buffer-substring-no-properties beg last-point)))

    (defun run-partial-pipe ()
      (interactive)
      (let ((string-to-execute (highlight-piped-region)))
        ;; https://stackoverflow.com/questions/65882345/replace-last-occurence-of-regexp-in-a-string-which-has-new-lines-replace-regexp/65882683#65882683
        (ess-eval-linewise
         (replace-regexp-in-string
          ".+<-" "" (replace-regexp-in-string
                     "\\(\\(.\\|\n\\)*\\)\\(%>%\\|\+\\) *\\'" "\\1" string-to-execute)))))

    (define-key ess-mode-map (kbd "<C-S-return>") 'run-partial-pipe)

    ;; Data Views
    (defun df-at-point-to-buffer (&optional numrows)
      "output a sample of another data.frame to and jump to buffer."
      (let ((object (symbol-at-point))
            (r-process (ess-get-process))
            (r-output-buffer (get-buffer-create "*R-output*"))
            (numrows (or numrows 300)))
        (ess-command
         (format "mmmisc::df_preview(%s, %s)\n" object numrows)
         r-output-buffer nil nil nil r-process)
        (switch-to-buffer-other-window r-output-buffer)
        ))

    (defun df-sample-small ()
      "Sample and print 30 rows of a data.frame"
      (interactive)
      (df-at-point-to-buffer 30)
      )

    (defun df-sample-medium ()
      "Sample and print 300 rows of a data.frame"
      (interactive)
      (df-at-point-to-buffer 300)
      )

    (defun df-sample-large ()
      "Sample and print 3000 rows of a data.frame"
      (interactive)
      (df-at-point-to-buffer 3000)
      )

    ;;======================================================================
    ;; (R) markdown mode
    ;;======================================================================

    ;; Insert a new (empty) chunk to R markdown ============================
    (defun insert-chunk ()
      "Insert chunk environment Rmd sessions."
      (interactive)
      (insert "```{r}\n\n```")
      (forward-line -1)
      )
    ;; key binding
    (global-set-key (kbd "C-c i") 'insert-chunk)

    ;; Mark a word at a point ==============================================
    ;; http://www.emacswiki.org/emacs/ess-edit.el
    (defun ess-edit-word-at-point ()
      (save-excursion
        (buffer-substring
         (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
         (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))
    ;; eval any word where the cursor is (objects, functions, etc)
    (defun ess-eval-word ()
      (interactive)
      (let ((x (ess-edit-word-at-point)))
        (ess-eval-linewise (concat x)))
      )
    ;; key binding
    (define-key ess-mode-map (kbd "C-c r") 'ess-eval-word)
    ))

  ;; Add fix for read-only ESS REPL
  ;; https://github.com/emacs-ess/ESS/issues/300
  (add-hook 'inferior-ess-mode-hook #'(lambda ()
                                        (setq-local comint-use-prompt-regexp nil)
                                        (setq-local inhibit-field-text-motion nil)))

  (eval-after-load "ess-r-mode" spacemacs/ess-config)
  (eval-after-load "ess-julia" spacemacs/ess-config))


(defun essp/init-ess-smart-equals ()
  (use-package ess-smart-equals
    :defer t
    :if ess-enable-smart-equals
    :init
    (progn
      (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
      (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode))))

(defun essp/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (dolist (f '(ess-eval-buffer-and-go
                 ess-eval-function-and-go
                 ess-eval-line-and-go))
      (add-to-list 'golden-ratio-extra-commands f))))

(defun essp/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(R . t))))

(defun essp/init-julia-mode ()

  (defun tide-insert-assign-equal ()
    "Insert an assignment ="
    (interactive)
    (insert " = "))

  (defun tide-insert-pipe-alternative ()
    "Insert an assignment |>"
    (interactive)
    (insert " |> "))

  (use-package julia-mode
    :init
    ;; Notice that I'm putting ess-julia-mode rather than julia-mode.
    ;; This is because I want all julia files to have ess-julia-mode and
    ;; julia-mode keeps creeping up first in the auto-mode-alist, effectively
    ;; overriding ess-julia-mode from ESS. Based on https://emacs.stackexchange.com/questions/38573/dissasociate-auto-mode-for-specific-files-in-ess
    ;; I came up with this solution here.
    (push '("\\.jl\\'" . ess-julia-mode) auto-mode-alist)
    (delete-dups auto-mode-alist)

    ;; This binds the two key bindings to ess-julia-mode and inferior-ess-julia-mode.
    ;; It seems that julia-mode is the parent of ess-julia-mode and every customization
    ;; needs to happen to julia-mode RATHEr than ess-julia-mode. You spent quite a lot of time
    ;; trying to figure out how to set the key binding to work with ess-julia-mode and
    ;; figure out that they need to be at julia-mode rather than ess-julia-mode.
    :bind (("C-<" . tide-insert-assign-equal)
           ("C->" . tide-insert-pipe-alternative)))

  ;; (setq comint-output-filter-functions
  ;;       (remove 'ansi-color-process-output comint-output-filter-functions))

  ;; (add-hook 'inferior-julia-mode-hook
  ;;           (lambda ()
  ;;             ;; Disable font-locking in this buffer to improve performance
  ;;             (font-lock-mode -1)
  ;;             ;; Prevent font-locking from being re-enabled in this buffer
  ;;             (make-local-variable 'font-lock-function)
  ;;             (setq font-lock-function (lambda (_) nil))
  ;;             (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  (setq inferior-julia-args "--color=yes")
  )

(defun R-scratch ()
  (interactive)
  (progn
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.R"))
    (switch-to-buffer new-buf)
    (R-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (R))
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name)))

(defun julia-scratch ()
  (interactive)
  (progn
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.jl"))
    (switch-to-buffer new-buf)
    (ess-julia-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*julia*" (mapcar (function buffer-name) (buffer-list))))
        (julia))
    (set-window-buffer w2 "*julia*")
    (set-window-buffer w1 w1name)))

(defun my-ess--R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))

