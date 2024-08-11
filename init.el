;; Basic Setup
;;
;; Started August 10, 2024
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; require these for org mode literate programming of config.
(require 'org)
(require 'ob-tangle)

;; Load the tangled file generated from config.org
(load-file "~/.config/emacs/config.el")

(defun my/tangle-config ()
  "Tangle the Emacs configuration file."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/config.org"))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/tangle-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-modeline shrink-path org-bullets nerd-icons compat)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(org-block ((t (:background "#000000"))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "gray80" :font "ETBembo"))))
 '(org-meta-line ((t (:background "#333333" :foreground "#666666"))))
 '(org-src-block ((t (:background "#000000"))))
 '(org-src-code ((t (:background "#000000"))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))
