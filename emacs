;; -*- mode: dotspacemacs -*-

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

; some solarized options
(setq solarized-distinct-fringe-background t)
(setq solarized-scale-org-headlines nil)
(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
; may be unnecessary...
;; (setq solarized-height-minus-1 1)
;; (setq solarized-height-plus-1 1)
;; (setq solarized-height-plus-2 1)
;; (setq solarized-height-plus-3 1)
;; (setq solarized-height-plus-4 1)

;; ================
;; Spacemacs config
;; ================

(defun dotspacemacs/layers ()
  "Configuration Layers config"
  (setq-default
   ; My own private layers are in "~/.emacs.d/spacemacs-layers/"
   dotspacemacs-configuration-layer-path
    '("~/.emacs.d/spacemacs-layers/")
   dotspacemacs-configuration-layers
    '(
      ; Contrib layers (included in spacemacs)
      version-control
      (git :variables
            git-gutter-use-fringe t)
      github
      osx
      dash
      org
      vim-empty-lines
      (auto-completion :variables
                       company-idle-delay 0.0
                       auto-completion-private-snippets-directory "~/.emacs.d/snippets")
      syntax-checking
      shell
      shell-scripts
      ;; (perspectives :variables
      ;;               perspective-enable-persp-projectile t)

      ; Langs
      (ruby :variables
            ruby-version-manager 'rvm
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
      emacs-lisp
      lua

      ; Themes
      ;; themes-megapack

      ; My own layers
      ra
      ra-org
      workgroups2
      ;; ra-purescript
      )
   dotspacemacs-additional-packages
    '(
      swift-mode
      )
   dotspacemacs-excluded-packages
    '(
      org-bullets
      magit-gh-pulls
      )
   dotspacemacs-delete-orphan-packages t
   ))

(defun dotspacemacs/init ()
  "Called at the beginning of spacemacs configuration sequence"
  (setq-default
   dotspacemacs-themes
    '(
      solarized-light
      solarized-dark
      )
   dotspacemacs-default-font
    '(
      "Menlo"
      :size 12
      :weight normal
      :width normal
      :powerline-scale 1.4
      )
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-always-show-changelog t
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-editing-style 'vim
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-persistent-server t
   )

  (setq
   ; tabbing: 2 spaces
   evil-shift-width 2
   ; org
   org-startup-indented t
   org-todo-keywords
   '(
     (sequence "PLAN(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "STALE(s)" "BLOCKED(b@/!)" "VERIFY(v)" "|" "CANCELLED(c@/!)")
     )
   org-log-done 'time)
  )

(defun dotspacemacs/config ()
  "Called at the end of spacemacs configuration sequence"

  (setq powerline-default-separator 'slant)

  ; enable left fringe, disable right fringe
  (fringe-mode '(nil . 0))
  (setq diff-hl-side 'left)

  ; enable and load workgroups session
  (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  (workgroups-mode 1)
  (golden-ratio-mode 1)

  ; turn off ido-mode, so plugins (workgroups) don't think it's
  ; preferred to helm
  (ido-mode -1)

  ; turn on which-key
  (which-key-mode 1)

  (when (equal system-type 'darwin)
    ;; Make some OSX idioms work
    (global-set-key (kbd "s-n") 'make-frame-command)
    (global-set-key (kbd "s-a") 'mark-whole-buffer)
    (global-set-key (kbd "s-w") 'delete-window)
    (global-set-key (kbd "s-S-w") 'delete-frame)
    (global-set-key (kbd "s-<right>") 'evil-end-of-line)
    (global-set-key (kbd "s-<left>") 'evil-first-non-blank)
    (global-set-key (kbd "s-S-<up>") 'evil-window-up)
    (global-set-key (kbd "s-S-<down>") 'evil-window-down)
    (global-set-key (kbd "s-S-<left>") 'evil-window-left)
    (global-set-key (kbd "s-S-<right>") 'evil-window-right)
    )

  (unless (server-running-p)
    (server-start)
    )
  )

;; Spacemacs wants to be cloned directly into ~/.emacs.d
;;
;; The following loads spacemacs from ~/.emacs.d/spacemacs
;; by setting it as 'user-emacs-directory temporarily
;;
;; NOTE: this currently mostly works, but breaks the update checker

(add-to-list 'load-path "~/.emacs.d/spacemacs/")
(setq backup-user-emacs-directory user-emacs-directory
      user-emacs-directory "~/.emacs.d/spacemacs/")

(load "~/.emacs.d/spacemacs/init")

(setq user-emacs-directory backup-user-emacs-directory)

