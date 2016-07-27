(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;-----------------------------------------------------------------------------------------
;;helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(helm-autoresize-mode t)
(setq helm-autoresize-max-height 20)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)



(require 'helm-swoop)
;; Change the keybinds to whatever you like :)
(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c s") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

(helm-mode 1)
;;-----------------------------------------------------------------------------------------
;;CEDET

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)

(semantic-add-system-include "/usr/include/boost")

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(semantic-mode 1)
;;-----------------------------------------------------------------------------------------
;;company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; TODO (add-to-list 'company-backends 'company-c-headers)
(global-set-key [(C tab)]  'company-complete)
;; (with-eval-after-load 'company 
;;   (define-key company-active-map (kbd "M-n") nil)
;;   (define-key company-active-map (kbd "M-p") nil)
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous))

;;-----------------------------------------------------------------------------------------
;;stuff
(nyan-mode 1)
(global-linum-mode t) ;;line numbers
(column-number-mode t) ; Shows the column number in the buffer's mode line
(setq c-default-style "linux" c-basic-offset 4)
(setq frame-title-format "emacs - %b") ;; Format the title-bar to always include the buffer name
;; make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; show matching parens:
(show-paren-mode 1)
(setq show-paren-delay 0)
;; fast yes or no 
(fset `yes-or-no-p `y-or-n-p)
;;parentheses auto pair. TODO: dont open pair if infront of symbol
(electric-pair-mode t)
;;My theme:
(if (display-graphic-p)
    (progn
      ;; if graphic
      ;; (require 'color-theme)
      ;; (color-theme-initialize)
      ;; (color-theme-gnome2)
      (add-to-list 'load-path "~/.emacs.d/themes")
      (require 'color-theme-tomorrow)
      (color-theme-tomorrow--define-theme night-eighties)
      ;;color current line
      (global-hl-line-mode 1)
      (set-face-background 'hl-line "#3e4446")
      (set-face-foreground 'highlight nil)
      )
  ;; else
  )

;;flash instead of bell
(setq visible-bell t )
(put 'dired-find-alternate-file 'disabled nil)
;; Enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
		 (buffer (car list)))
	(while buffer
	  (when (and (buffer-file-name buffer)
				 (not (buffer-modified-p buffer)))
		(set-buffer buffer)
		(revert-buffer t t t))
	  (setq list (cdr list))
	  (setq buffer (car list))))
  (message "Refreshed open files"))

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET
(setq-default indent-tabs-mode nil) ;; use space to indent by default
(setq-default tab-width 4)
;;-----------------------------------------------------------------------------------------
;;latex
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook
          (lambda () (visual-line-mode 1)))
;;-----------------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "white smoke"))))
 '(company-scrollbar-fg ((t (:background "dim gray"))))
 '(company-tooltip ((t (:background "dark gray" :foreground "black")))))

;;-----------------------------------------------------------------------------------------
;;eclim
;;(require 'eclim)
;; (global-eclim-mode)
;; (require 'eclimd)
;;-----------------------------------------------------------------------------------------
;;python
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;;-----------------------------------------------------------------------------------------
