;; From a video series on youtube
;; github act
;; https://github.com/SystemCrafters
;;
;; example init.el here:
;;https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org 
;-----------------------
;       init.el        ;
;----------------------;

;; you can setq multiple variables in the same setq function call

(setq inhibit-startup-message t ; get rid of welcome screen  
      visible-bell t) ; Flash when the bell rings

;; TUrn off some unneeded ui elements
(menu-bar-mode 1)
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

;;-----------------------------------------;;
;;   Episode 3 - Keybindings and Evil      ;;
;;-----------------------------------------;;
;;https://www.youtube.com/watch?v=xaZMwNELaJY&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ&index=3

;; emacs has a number of functions that you can use for keybindings
;; global-set-key
;; Example
;;-----------------------------------------
;; map C-M-j to an enhansed buffer switcher based on counsel
;; (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; define-key - define a key binding in a particular mode
;; Example
;; ----------------------------------------------
;; (define-key emacs-list-mode-map (kbd "C-x M-t") 'counsel-load-theme)
;;
;;-------------------------------------;;
;;  general.el keybinding management   ;;
;;-------------------------------------;;
;; In general we will not use these directly. Instead, we will use general.el
;; https://github.com/noctuid/general.el
(use-package general
  :config
  (general-evil-setup t)
  ;; define a leader key 
  (general-create-definer rune/leader-keys
     :keymaps '(normal insert visual emacs)
     :prefix "SPC"
     :global-prefix "C-SPC")
  
  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

;;-----------------;;
;;   evil mode     ;;
;;-----------------;;
;; use vim keybindings
;; https://github.com/emacs-evil/evil
(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;:hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


;; evil-collection
;; setup for evil modes that "just work"
(use-package evil-collection
  :after (evil magit)
  :config
  (evil-collection-init))
;;
;; EVIL MODE
;;
;; You can always leave evil mode using C-z
;(evil-mode 1)

;;-----------------
;;     hydra
;;-----------------
;; key binding combinator
;; https://github.com/abo-abo/hydra
(use-package hydra
  :defer t)
;; define a hydra to scale text up and down
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; access via C-SPC t s
;; define a hydra to scale text up and do
(rune/leader-keys
 "ts" '(hydra-text-scale/body :which-key "scale text"))

;;-----------------------------------
;; Episode 4 - Projectile and Magit
;;-----------------------------------
;; url:
;; https://www.youtube.com/watch?v=INTu30BHZGk&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ&index=4

;;-----------------
;; Projectile
;;-----------------
;; a project management package
;;https://github.com/bbatsov/projectile
;; WE Bind C-c p to show the list of projectile hotkeys
;; Just hit C-c p and ivy will show us all of the potential commands
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (setq projectile-switch-project-action #'projectile-dired))

;; add additional functionality when searching
;; usage:
;; C-c p f
;; M-o
(use-package counsel-projectile
  :config (counsel-projectile-mode))


;;-----------------------
;; MAGIT

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; evil-magit removed from melpa. now part of evil-collection
;;(use-package evil-magit
;;  :after magit)
;;

;;------------------------;;
;;       ORG MODE         ;;
;;------------------------;;
;; https://orgmode.org/

(defun jg/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . jg/org-mode-setup)
  :config
  (setq org-ellipsis " ↓"
	org-hide-emphasis-markers t))
  
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
;;  :custom
;;  (org-bullets-bullet-list '()))

;; replace list hyphen with a dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; change the scale of the various headings
(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1))))

(defun jg/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . jg/org-mode-visual-fill))

;; Support for MARKDOWN
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; grip mode - markdown
;; Prerequisite: pip install grip in a shell
;; use keybindings
(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
	      ("g" . grip-mode)))

;; Org Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

;; this is needed as of Org 0.2
;; now all you have to do in org mode to create a
;; code block for, say python, is type
;; <py
;; and hit tab
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; set the background of the begin / end block in org mode to be 3% darker
(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

