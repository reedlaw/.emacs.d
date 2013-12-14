;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package Management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From http://blog.zhengdong.me/2012/03/14/how-i-manage-emacs-packages/
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(ag
    coffee-mode
    flycheck
    go-mode
    helm
    helm-projectile
    jump
    key-chord
    magit
    markdown-mode
    multiple-cursors
    projectile
    rinari
    ruby-refactor
    rspec-mode
		scss-mode
    slim-mode
    wrap-region)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'ruby-mode-hook 'wrap-region-mode)
(add-hook 'scss-mode-hook 'flymake-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'auto-mode-alist '(".rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '(".gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '(".scss$" . scss-mode))
(add-to-list 'auto-mode-alist '(".scss.erb$" . scss-mode))
(add-to-list 'auto-mode-alist '(".sass$" . scss-mode))
(add-to-list 'auto-mode-alist '(".sass.erb$" . scss-mode))

(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(global-auto-revert-mode)

(ido-mode t)

(load-theme 'tango-dark)

(projectile-global-mode)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq inhibit-startup-message t)
(setq js-indent-level 2)
(setq projectile-enable-caching t)
(setq scss-compile-at-save nil)

;; (setq comment-auto-fill-only-comments 1)
;; (setq-default auto-fill-function 'do-auto-fill)

(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KEY BINDINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f1] 'tab-next-buffer)
(global-set-key [f2] 'tab-prev-buffer)
(global-set-key [f3] 'kill-this-buffer)
(global-set-key [f4] 'ag-project)
(global-set-key [f5] 'ag-regexp-project-at-point)
(global-set-key [f11] 'delete-other-windows)
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)
(global-set-key (kbd "C-c m") 'rename-this-buffer-and-file)
(global-set-key (kbd "C-c f") 'find-grep-dired-do-search)
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c d") 'remove-dos-eol)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-c t") 'projectile-find-file)
(global-set-key (kbd "C-t") 'helm-projectile)
(global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "M-RET") 'complete-tag)

; remappings, from http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
(eval-after-load "inf-ruby"
  '(define-key inf-ruby-minor-mode-map (kbd "C-c C-r") nil))

(key-chord-mode 1)
(key-chord-define-global "hj" 'undo)

(windmove-default-keybindings 'shift)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TAB BROWSING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tab-next-ignore (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "-template-indent-buffer$" str)
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*scratch\\*" str)
   (string-match "^ " str)

   (memq str
         (mapcar
          (lambda (x)
            (buffer-name
             (window-buffer
              (frame-selected-window x))))
          (visible-frame-list)))
   ))

(defun tab-next (ls)
  "Switch to next buffer while skipping unwanted ones."
  (let* ((ptr ls)
         bf bn go
         )
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      (if (null (tab-next-ignore bn))
          (setq go bf)
        (setq ptr (cdr ptr))
        )
      )
    (if go
        (switch-to-buffer go))))

(defun tab-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (tab-next (reverse (buffer-list))))

(defun tab-next-buffer ()
  "Switch to the other buffer (2nd in list-buffers) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (tab-next (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions I found online
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from http://www.emacswiki.org/emacs/EmacsTags#toc6
(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))
(let ((my-tags-file (find-file-upwards "TAGS")))
  (when my-tags-file
    (message "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

;; from http://superuser.com/a/176629
(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;; from http://tuxicity.se/emacs/elisp/2010/11/16/delete-file-and-buffer-in-emacs.html
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; from http://tuxicity.se/emacs/elisp/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(fci-rule-color "#efefef")
 '(org-agenda-files (quote ("~/Documents/creation/main.org")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#c82829") (40 . "#f5871f") (60 . "#eab700") (80 . "#718c00") (100 . "#3e999f") (120 . "#4271ae") (140 . "#8959a8") (160 . "#c82829") (180 . "#f5871f") (200 . "#eab700") (220 . "#718c00") (240 . "#3e999f") (260 . "#4271ae") (280 . "#8959a8") (300 . "#c82829") (320 . "#f5871f") (340 . "#eab700") (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
