;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; init.el
;;;
;;; This is the first script to run when emacs loads.
;;; (just make sure you don't have a .emacs file in $HOME)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     STARTUP INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gotta Loads
(require 'uniquify)

;; Disable startscreen, drop into scratch.
(setq inhibit-startup-screen t)

;; Hide annoying toolbar.
(tool-bar-mode -1)

;; Make tramp be trampy
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Create variable dotfiles-dir that points to .emacs.d or equivalent.
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Add theme directory for themes installed with elpa
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "elpa"))

;; Start server so good stuff happens?
(server-start)
(setq server-name "main")

;; Try out desktop save mode
(desktop-save-mode 1)

;; Get rid of annoying tilde backup files
(setq make-backup-files nil) 








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Universal Key Remapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-z") 'undo)









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     PACKAGE INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable used by ELPA to write installed packages 
(setq package-user-dir (concat dotfiles-dir "elpa"))

;; Add the packages subdirectory to the load path.
;; I know most people use 'lisp' for libraries... I'm contrarian
 (let ((default-directory (concat dotfiles-dir "packages")))
   (add-to-list 'load-path default-directory)
   (setq load-path
         (append
          (let ((load-path (copy-sequence load-path)))
            (normal-top-level-add-subdirs-to-load-path))
          load-path)
     )
   )

;; Set the package archive URLs to be better than the default.
(setq package-archives '(
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        )
      )

;; Install these packages!
(defvar required-packages '(ample-zen-theme
			    ess
			    find-file-in-project
			    fastnav
			    idle-highlight-mode
			    helm
			    helm-ls-git
			    magit
			    org-ac
			    paredit
			    yasnippet
			    yasnippet-bundle
			    multiple-cursors
			    multi-term
			    neotree
			    auctex
			    )
  )
;; Don't config these, they require special sauce.
(defvar noinst-packages '(helm
			  ess
                          ample-zen-theme
			  auctex
			  )
  )

;; Apparently ELPA isn't in previous versions. Forget those versions.
(when (>= emacs-major-version 24)
  (package-initialize)
  
  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p  required-packages)
    (when (not (package-installed-p p)) (package-install p))
    (if (not (memq p noinst-packages)) (require p))
    )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     HELM INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)  ;; Load package
(helm-mode 1)           ;; and turn it on
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

 ;; From helm wiki for eshell completion
(add-hook 'eshell-mode-hook 
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))
;; Add eshell helm history 
(add-hook 'eshell-mode-hook
      #'(lambda ()
          (define-key eshell-mode-map
        (kbd "M-p")
        'helm-eshell-history)))

;; Some suggestions from http://tuhdo.github.io/helm-intro.html
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Probably does what it sounds like. Who uses curl in emacs?
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; Make helm not take over your editor.
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Mac Stuff I guess
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((eq system-type 'darwin)
  (setq mac-function-modifier 'hyper)  ; make Fn key do Hyper
  (global-set-key (kbd "H-m") 'eshell)
  (global-set-key (kbd "H-<right>") 'other-window)
  
  
  ;; VIM bindings with hyper! Finally did this... cause EVIL is too hard
  (global-set-key (kbd "H-j") 'next-line)
  (global-set-key (kbd "H-k") 'previous-line)
  (global-set-key (kbd "H-h") 'backward-word)
  (global-set-key (kbd "H-l") 'forward-word)
  
  (defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring"
    (interactive "p")
    (kill-ring-save (line-beginning-position)
            (+ -1 (line-beginning-position (+ 1 arg)))
            ))
  (defun vi-open-line-below ()
    "Insert a newline below the current line and put point at beginning."
    (interactive)
    (unless (eolp)
      (end-of-line))
    (newline-and-indent))
  (defun vi-paste-below ()
    (interactive)
    (vi-open-line-below)
    (yank))
  (global-set-key (kbd "H-y") 'copy-line)
  (global-set-key (kbd "H-d") 'kill-whole-line)
  (global-set-key (kbd "H-p") 'vi-paste-below)
  (global-set-key (kbd "H-o") 'vi-open-line-below)
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               HIGHLIGHT MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun idle-highlight-hook ()
   (make-local-variable 'column-number-mode)
   (column-number-mode t)
   (if window-system (hl-line-mode t))
   (idle-highlight-mode t))

(add-hook 'emacs-lisp-mode-hook 'idle-highlight-hook)
;; (add-hook 'org-mode-hook 'my-coding-hook)






 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               ORG MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete for orgmode
(require 'org-ac)
(org-ac/config-default)

(setq org-export-odt-preferred-output-format "docx")

(setq org-support-shift-select t)

;; SAS Stuff
(setq-default ess-sas-submit-command-options "-noovp -nosyntaxcheck -autoexec /home/gaulinmp/autoexec_lib.sas")

(defun launch-ess-sas-interactive ()
  (interactive)
  (delete-other-windows)
  (setq w1 (selected-window))
  (setq w1name (buffer-name))
  (setq w2 (split-window w1 nil t))
  (if (not (member "*iESS[SAS]*" (mapcar (function buffer-name) (buffer-list))))
      (ess-sas-interactive))
  (set-window-buffer w2 "*iESS[SAS]*")
  (set-window-buffer w1 w1name))

;; STATA Stuff
;; (defun launch-ess-stata-interactive ()
;;   (interactive)
;;   (delete-other-windows)
;;   (setq w1 (selected-window))
;;   (setq w1name (buffer-name))
;;   (setq w2 (split-window w1 nil t))
;;   (if (not (member "*stata*" (mapcar (function buffer-name) (buffer-list))))
;;       (stata))
;;   (set-window-buffer w2 "*stata*")
;;   (set-window-buffer w1 w1name))

;; ESS Stuff
(defun custom-ess-launch-hook ()
  (interactive)
  (cond
   ((equal ess-language "SAS")
    (launch-ess-sas-interactive)) ; Launch SAS
   ;; ((equal ess-language "STA")
   ;;  (launch-ess-stata-interactive)) ; Launch STATA
   )
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step))
  )

(add-hook 'ess-mode-hook
      '(lambda()
         (local-set-key [(shift return)] 'custom-ess-launch-hook))
      )

;;      (require 'ess-site)

;ESS hangs if you eval long statements;
(setq ess-eval-visibly-p nil)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (clojure . t)
;;    (ditaa . t)
;;    (emacs-lisp . t)
;;    (haskell . t)
;;    (js . t)
;;    (perl . t)
;;    (python . t)
;;    (R . t)
;;    (ruby . t)
;; ;;   (sas . t)  
;;    (sh . t)
;;    (sql . t)
;; ;;   (stata . t)
;;    )
;;  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               MULTI-TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq multi-term-program "/usr/bin/zsh")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               NEO TREE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f8] 'neotree-toggle)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               MULTIPLE-CURSORS INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)







 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'ample-zen t)
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

;; My personal preference is to use line numbers
(global-linum-mode 1)





 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               FastNav Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
;;(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
;;(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
;;(global-set-key "\M-r" 'fastnav-replace-char-forward)
;;(global-set-key "\M-R" 'fastnav-replace-char-backward)
;;(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
;;(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
;;(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
;;(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
(global-set-key "\M-k" 'fastnav-delete-char-forward)
(global-set-key "\M-K" 'fastnav-delete-char-backward)
;;(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
;;(global-set-key "\M-M" 'fastnav-mark-to-char-backward)
;;(global-set-key "\M-p" 'fastnav-sprint-forward)
;;(global-set-key "\M-P" 'fastnav-sprint-backward)



 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Custom crap from emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )







 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               ESS STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SAS Stuff
  ;;          (ess-sas-interactive))
  ;;   (setq-default ess-sas-submit-command-options "-rsasuser -noovp -nosyntaxcheck")
  ;;     (defun my-ess-start-SAS ()
  ;;       (interactive)
  ;;         (delete-other-windows)
  ;;         (setq w1 (selected-window))
  ;;         (setq w1name (buffer-name))
  ;;         (setq w2 (split-window w1 nil t))
  ;;         (if (not (member "*iESS[SAS]*" (mapcar (function buffer-name) (buffer-list))))
  ;;           (ess-sas-interactive))
  ;;         (set-window-buffer w2 "*iESS[SAS]*")
  ;;         (set-window-buffer w1 w1name))

  ;; ;; STATA Stuff
  ;;     (defun my-ess-start-STATA ()
  ;;       (interactive)
  ;;         (delete-other-windows)
  ;;         (setq w1 (selected-window))
  ;;         (setq w1name (buffer-name))
  ;;         (setq w2 (split-window w1 nil t))
  ;;         (if (not (member "*stata*" (mapcar (function buffer-name) (buffer-list))))
  ;;           (stata))
  ;;         (set-window-buffer w2 "*stata*")
  ;;         (set-window-buffer w1 w1name))

  ;; ;; ESS Stuff
  ;;     (defun my-ess-eval ()
  ;;       (interactive)
  ;;       (if (equal ess-language "R")
  ;;         (my-ess-start-R)) ; Launch R
  ;;       (if (equal ess-language "SAS")
  ;;         (my-ess-start-SAS)) ; Launch SAS
  ;;       (if (equal ess-language "STA")
  ;;         (my-ess-start-STATA)) ; Launch STATA

  ;;       (if (and transient-mark-mode mark-active)
  ;;           (call-interactively 'ess-eval-region)
  ;;         (call-interactively 'ess-eval-line-and-step)))
  ;;     (add-hook 'ess-mode-hook
  ;;               '(lambda()
  ;;                  (local-set-key [(shift return)] 'my-ess-eval)))
  ;;     (add-hook 'inferior-ess-mode-hook
  ;;               '(lambda()
  ;;                  (local-set-key [C-up] 'comint-previous-input)
  ;;                  (local-set-key [C-down] 'comint-next-input)))
  ;;    (add-hook 'Rnw-mode-hook
  ;;             '(lambda()
  ;;                (local-set-key [(shift return)] 'my-ess-eval)))


      (require 'ess-site)

;ESS hangs if you eval long statements;
;;(setq ess-eval-visibly-p nil)
