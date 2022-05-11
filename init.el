
;-----------------------
;       init.el        ;
;----------------------;

;; you can setq multiple variables in the same setq function call

(setq inhibit-startup-message t ; get rid of welcome screen  
      visible-bell t) ; Flash when the bell rings

;; TUrn off some unneeded ui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-fringe-mode 10) ;; provide some breathing room

(column-number-mode)
(global-display-line-numbers-mode 1)

;; disable line numbers for some modes
(dolist (mode '(eshell-mode-hook
		term-mode-hook
		shell-mode-hook
		org-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;;---------------------
;;       themes
;;--------------------
;; load the Modus Vivendi dark theme
(load-theme 'modus-vivendi t)
;;(load-theme 'deeper-blue 1) ;; if we are running earlier version of emacs
;; there is a good doc on customizing modus-vivendi. check it out. prot.


;; describe-variable C-h v - shows docs for any variable in emacs
;; describe-funciton  C-h f - shows docs for any function in emacs
;; describe-symbol C-h o - best of both worlds

;; Ctrl-Alt i - completion at the point when typing

(recentf-mode 1) ; use M-x recentf-open-files 

;; remembering minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)
;; now you can use
;;   M-n (next-history-element)
;;   M-p (previous-history-element)

;; save place mode
;; remembering the last place you visited in a file
(save-place-mode 1)

; dont show pop up UI dialg-boxes with buttons. keep things text oriented
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed.
;; pick up changes on disk. (like if you use git to update data on disk)
(global-auto-revert-mode 1)

(set-face-attribute 'default nil :font "SpaceMono Nerd Font Mono" :height 120)

;;---------------------
;;   PACKAGE MANAGER
;;---------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; remember to run M-x package-refresh-contents
(unless package-archive-contents
  (package-refresh-contents))
;; Initialize use-package

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; always load packages if they don't exist from the package repo(s)
(setq use-package-always-ensure t)

;; show window of commands
(use-package command-log-mode)
;; to turn on the command log buffer
;; M-x command-log-buffer
;; to show the command log buffer
;; clm/toggle-command-log-buffer

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;-------------;;
;;    Ivy      ;;
;;-------------;;
(use-package counsel)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))


;; customize the mode line
;; doom modeline is awesome
;; https://github.com/seagle0128/doom-modeline
;; to run you need to M-x all-the-icons-install-fonts
;; the docs have a bunch of configuration options
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))
  

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display the buffer name.
(setq doom-modeline-buffer-name t)

;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes t) ;; was nil

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)

;; Whether display the environment version.
(setq doom-modeline-env-version t)

;;----------------------------------------------
;; Emacs from scratch #2 video - Adding Helpful UI Improvements
;;https://www.youtube.com/watch?v=IspAZtNTslY&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ&index=2
;;----------------------------------------------

;;-----------------------
;; rainbow delimiters
;;-----------------------
;; color the delimiters in prog-mode (any programming mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; adds descriptions of key completions for hotkeys
;; eg if you type C-h it will show a minibuffer full of choices
;; or C-x for instance
;; you can set the delay so that the extra help doesn't show up
;; before a set amount of time
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ivy-rich
;; in addition to ivy
;; gives you extra info for a few of the built in commands
;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
;;(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^


;; helpful
;; https://github.com/Wilfred/helpful
;; augment's emac's help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helful-key))

