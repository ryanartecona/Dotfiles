;; ============
;; Emacs config
;; ============

; save emacs sessions by default
(desktop-save-mode 1)

; open new stuff in new graphical windows (instead of buffers)
;; (setq pop-up-frames 'graphic-only) ; opens too many frames, need to figure it out
(setq display-buffer-reuse-frames t
      display-buffer-reuse-window t)

;; UI
;; --

(setq solarized-high-contrast-mode-line t
      solarized-distinct-fringe-background t)

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
                solarized-light
                solarized-dark
                ))

(setq-default dotspacemacs-configuration-layers
              '(
                ; My own layer
                ra

                ; Contrib layers (included in spacemacs)
                (git :variables
                     git-enable-github-support t
                     git-gutter-use-fringe t)
                smex
                dash
                osx
                vim-empty-lines
                company-mode

                ; Langs
                (ruby :variables
                      ruby-version-manager rvm
                      ruby-enable-ruby-on-rails-support t)
                (haskell :variables
                         haskell-enable-shm-support t)
                html
                markdown
                ocaml

                ; Themes
                ;; themes-megapack
                ))

(defun dotspacemacs/config ()
  "Called at the end of spacemacs configuration sequence"
  (setq powerline-default-separator 'nil))

  ; OPAM OCaml stuff
  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (setq merlin-command 'opam)

;; Spacemacs wants to be cloned directly into ~/.emacs.d
;;
;; The following loads spacemacs from ~/.emacs.d/spacemacs
;; by setting it as 'user-emacs-directory temporarily

(add-to-list 'load-path "~/.emacs.d/spacemacs/")
(setq backup-user-emacs-directory user-emacs-directory
      user-emacs-directory "~/.emacs.d/spacemacs/")

(load "~/.emacs.d/spacemacs/init")

(setq user-emacs-directory backup-user-emacs-directory)
