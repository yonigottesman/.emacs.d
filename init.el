(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;ido
(require 'ido)
(ido-mode t)

;;smex
(require 'smex)
(smex-initialize) 
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))
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
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)
(setq company-minimum-prefix-length 2)
(global-set-key [(C tab)]  'company-complete)
(with-eval-after-load 'company 
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
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
      (set-face-attribute 'region nil :background "#ff9200" :foreground "#ffffff")
      )
  (load-theme 'zenburn t)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#3e4446")
  (set-face-foreground 'highlight nil)
  (set-face-attribute 'region nil :background "#ff9200" :foreground "#ffffff")
  
  ;; else
  )

(setq inhibit-startup-screen t)
;; flash instead of bell
;; (setq visible-bell t )
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


;;for terminal mode
(global-set-key (kbd "ESC <up>") 'backward-paragraph)
(global-set-key (kbd "ESC <down>") 'forward-paragraph)

(global-set-key [M-up] 'backward-paragraph)
(global-set-key [M-down] 'forward-paragraph)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET
(setq-default indent-tabs-mode nil) ;; use space to indent by default
(setq-default tab-width 4)


(global-set-key [(C-f5)] 'compile)
(global-set-key [(f5)] 'recompile)
;;-----------------------------------------------------------------------------------------
;;latex

(add-hook 'latex-mode-hook 'flyspell-mode)

(add-hook 'latex-mode-hook
          (lambda () (visual-line-mode 1)))

;;-----------------------------------------------------------------------------------------
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
;;  '(ecb-options-version "2.50")
;;  '(package-selected-packages
;;    (quote
;;     (sr-speedbar zenburn-theme yasnippet smex nyan-mode ecb company))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-scrollbar-bg ((t (:background "white smoke"))))
;;  '(company-scrollbar-fg ((t (:background "dim gray"))))
;;  '(company-tooltip ((t (:background "dark gray" :foreground "black")))))

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


;;-----------------------------------------------------------------------------------------
;;prjects

(require 'package)
(global-ede-mode t)



(ede-cpp-root-project "ycdb" :file "/Users/yonatang/gemini_search/ycdb/Makefile"
                      :include-path '("/include"))


(ede-cpp-root-project "datastore" :file "/Users/yonatang/gemini_search/datastores/Makefile"
                      :include-path '("/common/api"))


;;ECB
;; (require 'ecb)
;; (setq ecb-layout-name "left9")


;;gdb
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;;  (global-hl-line-mode 1)
;; (set-face-background 'hl-line "color-243")
;; ;; (set-face-foreground 'highlight nil)
;; ;; (set-face-attribute 'region nil :background "#ff9200" :foreground "#ffffff")
;; ;; ;;zeburn
;; ; Set cursor color to white
;; (load-theme 'zenburn)
;; (set-cursor-color "#ffffff") 

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(setq ring-bell-function 'ignore)


(require 'sr-speedbar)
(setq sr-speedbar-width 40)


;;copy paste from clipboard
(setq x-select-enable-clipboard t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme ## multi-web-mode yasnippet sr-speedbar smex nyan-mode jedi helm-swoop company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;web
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;;line length limit
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
