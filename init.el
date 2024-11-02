(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t) 
(package-initialize)

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

;;stuff
(nyan-mode 1)

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

(setq inhibit-startup-screen t)
;; flash instead of bell
;; (setq visible-bell t )
(put 'dired-find-alternate-file 'disabled nil)

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
;;-----------


;;latex

(add-hook 'latex-mode-hook 'flyspell-mode)

(add-hook 'latex-mode-hook
          (lambda () (visual-line-mode 1)))

(require 'package)
(global-ede-mode t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(setq ring-bell-function 'ignore)

;;copy paste from clipboard
(setq x-select-enable-clipboard t)


;;line length limit
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Some key bindings
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


(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)

(load-theme 'dracula t)
(global-display-line-numbers-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(nyan-mode gptel smex dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(gptel-make-anthropic "Claude"          ;Any name you want
  :stream t                             ;Streaming responses
  :key "")
(setq
 gptel-model 'claude-3-sonnet-latest ;  "claude-3-opus-20240229" also available
 gptel-backend (gptel-make-anthropic "Claude"
                 :stream t :key ""))


;; (set-face-attribute 'region nil :background "#ff9200" :foreground "#ffffff")

(global-set-key (kbd "C-c g") 'gptel-send)
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

