(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;line numbers
(global-linum-mode t)


(setq c-default-style "linux" c-basic-offset 4)


;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; show matching parens:
(show-paren-mode 1)
(setq show-paren-delay 0)

;;ede
(global-ede-mode t)

;;semantic
(semantic-mode 1)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)
(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)

;; Enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; START enable C++ autocomplete for emacs
(require 'cl)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; Select candidates with C-n/C-p only when completion menu is displayed:
(setq ac-use-menu-map t)
(define-key ac-menu-map "C-n" 'ac-next)
(define-key ac-menu-map "C-p" 'ac-previous)
(ac-set-trigger-key "TAB")
(define-key ac-mode-map [(control tab)] 'auto-complete)
(setq ac-candidate-limit 100) ;; do not stall with too many results
(setq ac-auto-start 3);; how many chars before ac starts
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay 1)
(setq ac-use-fuzzy 1.5)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-expand-on-auto-complete nil)
(setq ac-quick-help-height 4)
(setq ac-menu-height 20)

(defun my-ac-config ()
  
  (setq-default ac-sources '(ac-source-abbrev ac-source-semantic-raw ac-source-semantic;; ac-source-dictionary ac-source-words-in-same-mode-buffers
			     ))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (Add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
  )
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;;(my-ac-config)

(require 'ido)
(ido-mode t)

;;parentheses auto pair. TODO: dont open pair if infront of symbol
(electric-pair-mode t)

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

(require 'xcscope )
(cscope-setup)
(setq cscope-index-recursively t)
(define-key global-map [(ctrl f3)] 'cscope-set-initial-directory)
(define-key global-map [(ctrl f4)] 'cscope-unset-initial-directory)
(define-key global-map [(ctrl f5)] 'cscope-find-this-symbol)
(define-key global-map [(ctrl f6)] 'cscope-find-global-definition)
(define-key global-map [(ctrl f7)] 'cscope-find-global-definition-no-prompting)
(define-key global-map [(ctrl f8)] 'cscope-pop-mark)
(define-key global-map [(ctrl f9)] 'cscope-history-forward-line-current-result)
(define-key global-map [(ctrl f10)] 'cscope-history-forward-file-current-result)
(define-key global-map [(ctrl f11)] 'cscope-history-backward-line-current-result)
(define-key global-map [(ctrl f12)] 'cscope-history-backward-file-current-result)
(define-key global-map [(meta f9)] 'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sr-speedbar-right-side t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;ibuffer
(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)


;;Python
(add-hook 'python-mode-hook 'jedi:setup)
;;(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:complete-on-dot t)      
; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)
(semantic-add-system-include "/usr/local/lib/python2.7/dist-packages" 'python-mode)
(put 'dired-find-alternate-file 'disabled nil)

;;web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))



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

;;shortcute                                                                                     
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "<f5>") 'compile)

;;TEX
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook (lambda () (visual-line-mode 1)))

;; fast yes or no                                                                               
(fset `yes-or-no-p `y-or-n-p) 
