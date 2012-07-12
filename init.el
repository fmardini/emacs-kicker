;; emacs kicker --- kick start emacs setup
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: https://github.com/dimitri/emacs-kicker
;; Created: 2011-04-15
;; Keywords: emacs setup el-get kick-start starter-kit
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.

(require 'cl)        ; common lisp goodies, loop

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(add-to-list 'load-path (concat user-emacs-directory "src/lib"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move      ; have to add your own keys
    :after (progn
       (global-set-key (kbd "<C-S-up>")     'buf-move-up)
       (global-set-key (kbd "<C-S-down>")   'buf-move-down)
       (global-set-key (kbd "<C-S-left>")   'buf-move-left)
       (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex        ; a better (ido like) M-x
    :after (progn
       (setq smex-save-file "~/.emacs.d/.smex-items")
       (global-set-key (kbd "M-x") 'smex)
       (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit        ; git meet emacs, and a binding
    :after (progn
       (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name lisppaste
          :type elpa)

   (:name flymake-jshint
          :type elpa)

   (:name goto-last-change    ; move pointer back to last change
    :after (progn
       ;; when using AZERTY keyboard, consider C-x C-_
       (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get                             ; el-get is self-hosting
   switch-window                      ; takes over C-x o
   auto-complete                      ; complete as you type with overlays
   evil                               ; VIM
   flymake-cursor                     ;
   yaml-mode                          ;
   haml-mode                          ;
   python-pep8                        ;
   paredit                            ;
   highlight-parentheses              ;
   clojure-mode                       ;
   expand-region                      ;
   color-theme                        ; nice looking emacs
   color-theme-tango))                ; check out color-theme-solarized

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

(require 'evil)
(evil-mode 1)

;; on to the visual settings
(setq inhibit-splash-screen t)    ; no splash screen, thanks
(line-number-mode 1)      ; have line numbers and
(column-number-mode 1)      ; column numbers in the mode line

(tool-bar-mode -1)      ; no tool bar with icons
(menu-bar-mode -1)      ; no menu bar
(scroll-bar-mode -1)    ; no scroll bars
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode)      ; highlight current line
(global-linum-mode 1)      ; add line numbers on the left

(show-paren-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defalias 'yes-or-no-p 'y-or-n-p)


(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(defvar kicker-savefile-dir (concat user-emacs-directory "savefile/"))
(setq save-place-file (concat kicker-savefile-dir "saveplace"))
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

(setq custom-file (concat kicker-savefile-dir "custom.el"))

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat kicker-savefile-dir "savehist"))
(savehist-mode t)

;; save recent files
(setq recentf-save-file (concat kicker-savefile-dir "recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; flyspell-mode does spell-checking on the fly as you type
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive argument.  For use in hooks."
  (interactive)
  (flyspell-mode +1))

(add-hook 'message-mode-hook 'prelude-turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; parentheses
(dolist (hook '(scheme-mode-hook
                lisp-mode-hook
                emacs-lisp-mode-hook
                clojure-mode-hook))
  (add-hook hook (lambda ()
                   (paredit-mode t)
                   (highlight-parentheses-mode t))))

(setq parens-require-spaces nil)
(setq show-paren-style 'expression)
(show-paren-mode t)
(set-face-background 'show-paren-match "lightskyblue1")

(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)

  (let (osx-paths)
    (dolist (path '("/usr/local/bin" "/opt/local/bin" "/opt/local/sbin") (setenv "PATH" (concat osx-paths (getenv "PATH"))))
      (push path exec-path)
      (setq osx-paths (concat (concat path ":") osx-paths))))

  ;; Emacs users obviously have little need for Command and Option keys,
  ;; but they do need Meta and Super
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil
        :background "blue")

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)


;; my code
(defun increment-number-at-point ()
    (interactive)
      (skip-chars-backward "0123456789")
        (or (looking-at "[0123456789]+")
                  (error "No number at point"))
          (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c C-+") 'increment-number-at-point)
;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)'

(load-theme 'tango-dark)
