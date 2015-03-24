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
(load "server")
(unless (server-running-p) (server-start))

;; Get rid of annoying tilde backup files
(setq make-backup-files nil) 

;; Overwrite selected text
(delete-selection-mode 1)







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
			    projectile
			    helm-projectile
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
    (when (not (package-installed-p p)) 
      (message "Installing package %s ... 0%%" p)
      (package-install p)
      (message "Installing package %s ... done!" p)
      )
    (when (not (memq p noinst-packages)) 
      (message "Loading package %s ... 0%%" p)
      (require p)
      (message "Loading package %s ... done!" p)
      )
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
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;  ;; From helm wiki for eshell completion
;; (add-hook 'eshell-mode-hook 
;;           #'(lambda ()
;;               (define-key eshell-mode-map
;;                 [remap eshell-pcomplete]
;;                 'helm-esh-pcomplete)))
;; ;; Add eshell helm history 
;; (add-hook 'eshell-mode-hook
;;       #'(lambda ()
;;           (define-key eshell-mode-map
;;         (kbd "M-p")
;;         'helm-eshell-history)))

;; Some suggestions from http://tuhdo.github.io/helm-intro.html
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Probably does what it sounds like. Who uses curl in emacs?
;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; Make helm not take over your editor.
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; Projectile and Helm Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Mac Stuff
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
(add-hook 'python-mode-hook 'idle-highlight-hook)
;; (add-hook 'org-mode-hook 'my-coding-hook)






 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               ORG MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete for orgmode
;; (require 'org-ac)  ; Already loaded above
(message "Loading org-mode config ... start")
(org-ac/config-default)
(message "Loading org-mode config ... config-default")

(setq org-agenda-files (list "~/Dropbox/Documents/Personal/todo.org"))
(dolist (p org-agenda-files)
  (find-file p)
  )

;; Word wrap org mode!
(setq org-startup-truncated nil)

;; C-c C-e o exports to docx by default. Must install libreoffice.
(setq org-odt-preferred-output-format "docx")

;; Shift arrows selects text (except in headings)
(setq org-support-shift-select t)

;; Key bindings
(global-set-key (kbd "C-c l") 'copy-line)

(message "Loading org-mode config ... done!")




 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               ESS Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading ess config ... start")
;; This command is custom to me, launches with correct lib library (not normal autoexec.sas)
(setq-default ess-sas-submit-command-options "-noovp -nosyntaxcheck -autoexec /home/gaulinmp/autoexec_lib.sas")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

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

;ESS hangs if you eval long statements;
(setq ess-eval-visibly-p nil)

(message "Loading ess config ... set custom hooks")

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (clojure . t)
   ;; (ditaa . t)
   (emacs-lisp . t)
   ;; (haskell . t)
   (js . t)
   ;; (perl . t)
   (python . t)
   ;; (R . t)
   ;; (ruby . t)
   ;; (sas . t)  
   (sh . t)
   (sql . t)
;;   (stata . t)
   )
 )

(message "Loading ess config ... set custom languages")
(require 'ess-site)
(message "Loading ess config ... done!")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               MULTI-TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading multi-term config ... start")
(cond
 ((eq system-type 'darwin) (setq multi-term-program "/usr/local/homebrew/bin/zsh"))
 (t  (setq multi-term-program "/usr/bin/zsh"))
 )
 
(defun init-multi-term (new-name dir)
  (message "Loading zsh at %s ... start" dir)
  (let ((mt-buffer-name "*terminal<1>*"))
    (multi-term)
    (comint-send-string (get-buffer-process mt-buffer-name) (format "cd %s\n" dir))
    (with-current-buffer mt-buffer-name
      (rename-buffer new-name)))
  (message "Loading zsh at %s ... done!" dir)
  )

(defun init-multi-terms ()
  (interactive)
  (init-multi-term "*zsh*" "~/")
  ;; (init-multi-term "*zsh:python*" "~/Dropbox/Documents/Programming/Python/")
  ;; (init-multi-term "*zsh:school*" "~/Dropbox/Documents/School/Projects/")
  (switch-to-buffer "*scratch*")
)
(add-hook 'emacs-startup-hook 'init-multi-terms)

(message "Loading multi-term config ... done!")


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
(message "Loading theme config ... start")
(load-theme 'ample-zen t)
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

;; My personal preference is to use line numbers
(global-linum-mode 1)
(message "Loading theme config ... done!")





 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               FastNav Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key  (kbd "C-c d f") 'fastnav-zap-up-to-char-forward)
(global-set-key (kbd "C-c d b") 'fastnav-zap-up-to-char-backward)
(global-set-key (kbd "C-c t f") 'fastnav-jump-to-char-forward)
(global-set-key (kbd "C-c t b") 'fastnav-jump-to-char-backward)
;;(global-set-key "\M-r" 'fastnav-replace-char-forward)
;;(global-set-key "\M-R" 'fastnav-replace-char-backward)
;;(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
;;(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
;;(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
;;(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
;;(global-set-key "\M-z" 'fastnav-delete-char-forward)
;;(global-set-key "\M-Z" 'fastnav-delete-char-backward)
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
 '(ansi-color-names-vector ["#212121" "#CC5542" "#6aaf50" "#7d7c61" "#5180b3" "#DC8CC3" "#9b55c3" "#bdbdb3"])
 '(custom-safe-themes (quote ("8cf56691a70156f611ac86d0bbcbc7dee7673df195de5918f34bfdc6814ffd39" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" default)))
 '(elpy-modules (quote (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(fci-rule-color "#2e2e2e")
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map (quote ((20 . "#dd5542") (40 . "#CC5542") (60 . "#fb8512") (80 . "#baba36") (100 . "#bdbc61") (120 . "#7d7c61") (140 . "#6abd50") (160 . "#6aaf50") (180 . "#6aa350") (200 . "#6a9550") (220 . "#6a8550") (240 . "#6a7550") (260 . "#9b55c3") (280 . "#6CA0A3") (300 . "#528fd1") (320 . "#5180b3") (340 . "#6380b3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

