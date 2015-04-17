;; ============
;; Emacs config
;; ============

; save emacs sessions by default
;; (desktop-save-mode 1)

; open new stuff in new graphical windows (instead of buffers)
;; (setq pop-up-frames 'graphic-only) ; opens too many frames, need to figure it out
(setq display-buffer-reuse-frames t
      display-buffer-reuse-window t)

; fix window scroll jumping when point moves near beginning/end of buffer
(setq auto-window-vscroll nil)

(setq system-uses-terminfo nil)

;; ================
;; Spacemacs config
;; ================

(setq-default dotspacemacs-default-font
              '(
                "Menlo"
                :size 12
                :weight normal
                :width normal
                :powerline-scale 1.4
                ))

(setq-default dotspacemacs-themes
              '(
                solarized-dark
                solarized-light
                ))

(setq-default dotspacemacs-configuration-layer-path
              (list
               (concat user-emacs-directory "spacemacs-layers/")
               ))

(setq-default dotspacemacs-configuration-layers
              '(
                ; My own layers
                ra
                ra-org
                workgroups2

                ; Contrib layers (included in spacemacs)
                (git :variables
                     git-enable-github-support t
                     git-gutter-use-fringe t)
                osx
                dash
                (org :variables
                     org-startup-indented t)
                vim-empty-lines
                (auto-completion :variables
                                 company-idle-delay 0.0)
                syntax-checking
                ;; (perspectives :variables
                ;;               perspective-enable-persp-projectile t)

                ; Langs
                (ruby :variables
                      ruby-version-manager rvm
                      ruby-enable-ruby-on-rails-support t)
                (haskell :variables
                         haskell-enable-shm-support t)
                (html :variables
                      web-mode-markup-indent-offset 2
                      web-mode-css-indent-offset 2
                      web-mode-code-indent-offset 2
                      scss-compile-at-save nil)
                markdown
                ocaml

                ; Themes
                ;; themes-megapack
                ))

(setq dotspacemacs-excluded-packages
      '(
        org-bullets
        magit-gh-pulls
        ))

(defun dotspacemacs/init ()
  "Called at the beginning of spacemacs configuration sequence"

  ; some solarized options
  (setq solarized-distinct-fringe-background t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t)

  ; tabbing: 2 spaces
  (setq evil-shift-width 2
        helm-pt-insert-at-point nil)
  )

(defun dotspacemacs/config ()
  "Called at the end of spacemacs configuration sequence"

  (setq powerline-default-separator 'slant)

  ; enable left fringe, disable right fringe
  (fringe-mode '(nil . 0))
  (setq git-gutter-fr:side 'left-fringe)

  ; enable and load workgroups session
  (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  (golden-ratio-mode -1)
  ;; (workgroups-mode 1)
  (golden-ratio-mode 1)
  )

;; Spacemacs wants to be cloned directly into ~/.emacs.d
;;
;; The following loads spacemacs from ~/.emacs.d/spacemacs
;; by setting it as 'user-emacs-directory temporarily

(add-to-list 'load-path "~/.emacs.d/spacemacs/")
(setq backup-user-emacs-directory user-emacs-directory
      user-emacs-directory "~/.emacs.d/spacemacs/")

(load "~/.emacs.d/spacemacs/init")

(setq user-emacs-directory backup-user-emacs-directory)

