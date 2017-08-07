;; -*- mode: dotspacemacs -*-

;; ================
;; Spacemacs config
;; ================

(defun dotspacemacs/layers ()
  "Configuration Layers config"
  (setq
   ; My own private layers are in "~/.emacs.d/spacemacs-layers/"
   dotspacemacs-configuration-layer-path
    '("~/.emacs.d/spacemacs-layers/")
   dotspacemacs-configuration-layers
   '(
      ; Contrib layers (included in spacemacs)
      (version-control :variables
                       version-control-diff-tool 'diff-hl)
      git
      github
      osx
      dash
      org
      vim-empty-lines
      ivy
      (auto-completion :variables
                       company-idle-delay 0.1
                       auto-completion-private-snippets-directory "~/.emacs.d/snippets"
                       auto-completion-enable-help-tooltip t)
      syntax-checking
      (shell :variables
             shell-default-shell 'multi-term
             shell-default-term-shell "/usr/local/bin/fish")
      shell-scripts
      ;; (perspectives :variables
      ;;               perspective-enable-persp-projectile t)
      (spacemacs-layouts :variables
                         spacemacs-layouts-directory "~/.emacs.d/persps-layouts/"
                         layouts-enable-autosave t
                         layouts-autosave-delay 1800)

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
      asciidoc
      ocaml
      emacs-lisp
      lua
      nixos
      elixir
      yaml
      pandoc
      nginx
      javascript
      purescript
      agda
      python
      sql
      csv
      (reason :variables
              reason-auto-refmt t)

      ; My own layer
      ra
      )
   dotspacemacs-additional-packages
    '(
      ox-reveal
      prettier-js
      )
   dotspacemacs-excluded-packages
    '(
      org-bullets
      magit-gh-pulls
      magithub
      magit-gitflow
      orgit
      robe
      ; can't ever get this to not complain at me...
      anaconda-mode
      editorconfig
      )
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   ))

(defun dotspacemacs/init ()
  "Called at the beginning of spacemacs configuration sequence"
  (setq
   dotspacemacs-themes
    '(
      solarized-light
      solarized-dark
      )
   dotspacemacs-default-font
    '(
      ;; "Menlo"
      "PragmataPro"
      :size 14
      :weight normal
      :width normal
      :powerline-scale 1.4
      )
   dotspacemacs-directory "~/.emacs.d/spacemacs"
   dotspacemacs-filepath "~/.spacemacs"
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
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-persistent-server t
   dotspacemacs-maximized-at-startup t
   dotspacemacs-elpa-https t
   dotspacemacs-check-for-update nil
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-folding-method 'origami
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

(defun dotspacemacs/user-init ()
  ;; This runs before spacemacs packages are loaded & configured

  ;; raise GC threshold to 200MB
  (setq gc-cons-threshold 200000000)

  ;; save emacs sessions by default
  ;; (desktop-save-mode 1)


  ;; open new stuff in new graphical windows (instead of buffers)
  ;; (setq pop-up-frames 'graphic-only) ; opens too many frames, need to figure it out
  (setq display-buffer-reuse-frames t
        display-buffer-reuse-window t)

  ;; fix window scroll jumping when point moves near beginning/end of buffer
  (setq auto-window-vscroll nil)

  (setq system-uses-terminfo nil)

  ;; some solarized options
  (setq solarized-distinct-fringe-background t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  ;; may be unnecessary...
  ;; (setq solarized-height-minus-1 1)
  ;; (setq solarized-height-plus-1 1)
  ;; (setq solarized-height-plus-2 1)
  ;; (setq solarized-height-plus-3 1)
  ;; (setq solarized-height-plus-4 1)
  )

(defun dotspacemacs/user-config ()
  "Called at the end of spacemacs configuration sequence"

  (setq powerline-default-separator 'slant)

  ; enable left fringe, disable right fringe
  (fringe-mode '(nil . 0))
  (setq diff-hl-side 'left)

  ; enable and load workgroups session
  ;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  ;; (workgroups-mode 1)
  (golden-ratio-mode 1)

  ; turn off ido-mode, so plugins (workgroups) don't think it's
  ; preferred to helm or ivy
  (ido-mode -1)

  ; turn on which-key
  (which-key-mode 1)

  ; turn off clean-aindent-mode
  (clean-aindent-mode -1)

  (global-company-mode 1)

  (when (equal system-type 'darwin)
    ;; Make some OSX idioms work
    (global-set-key (kbd "s-n") 'make-frame-command)
    (global-set-key (kbd "s-a") 'mark-whole-buffer)
    (global-set-key (kbd "s-w") 'delete-window)
    (global-set-key (kbd "s-<right>") 'evil-end-of-line)
    (global-set-key (kbd "s-<left>") 'evil-first-non-blank)
    (global-set-key (kbd "H-<up>") 'evil-window-up)
    (global-set-key (kbd "H-<down>") 'evil-window-down)
    (global-set-key (kbd "H-<left>") 'evil-window-left)
    (global-set-key (kbd "H-<right>") 'evil-window-right)
    (global-set-key (kbd "M-DEL") 'evil-delete-backward-word)
    )

  ;; This stuff is to ansi-colorize the compilation buffer after a rails test so the terminal colors come through.
  (define-derived-mode ansi-compilation-mode compilation-mode "ansi compilation"
    "Compilation mode that understands ansi colors."
    (require 'ansi-color)
    (let ((inhibit-read-only 1))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun colorize-compilation (one two)
    "ansi colorize the compilation buffer."
    (ansi-compilation-mode)
    )

  (setq compilation-finish-function 'colorize-compilation)


  ; This is available in iTerm but not ansi-term, and its absence
  ; breaks e.g. ruby scripts run from ansi-term
  (setenv "LANG" "en_US.UTF-8")


  (unless (server-running-p)
    (server-start)
    )
  )
