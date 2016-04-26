;; -*- mode: dotspacemacs -*-

;; ============
;; Emacs config
;; ============

; raise GC threshold to 200MB
(setq gc-cons-threshold 200000000)

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
      (auto-completion :variables
                       company-idle-delay 0.1
                       auto-completion-private-snippets-directory "~/.emacs.d/snippets")
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

      ; Themes
      ;; themes-megapack

      ; My own layers
      ra
      ra-org
      ;; workgroups2
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
      :size 12
      :weight normal
      :width normal
      :powerline-scale 1.4
      )
   dotspacemacs-directory "~/.emacs.d/spacemacs"
   dotspacemacs-filepath "~/.emacs"
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
   ;; dotspacemacs-auto-resume-layouts t
   dotspacemacs-maximized-at-startup t
   dotspacemacs-enable-lazy-installation t
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
  ; preferred to helm
  (ido-mode -1)

  ; turn on which-key
  (which-key-mode 1)

  ; turn off clean-adindent-mode
  (clean-aindent-mode -1)

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

  ;; make Y yank a full line, not point-to-eol
  ;; (this is a patch over a spacemacs default, which will hopefully be changed)
  ;; (define-key evil-normal-state-map (kbd "Y") 'evil-yank-line)
  ;; (define-key evil-motion-state-map (kbd "Y") 'evil-yank-line)

  (unless (server-running-p)
    (server-start)
    )
  )

;; Spacemacs wants to be cloned directly into ~/.emacs.d
;;
;; The following loads spacemacs from ~/.emacs.d/spacemacs
;; by setting it as 'user-emacs-directory

(add-to-list 'load-path "~/.emacs.d/spacemacs/")
(setq backup-user-emacs-directory user-emacs-directory
      user-emacs-directory "~/.emacs.d/spacemacs/")

;; (setq init-file-debug t)
(load "~/.emacs.d/spacemacs/init")


;; ===== Customize =====

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4 t)
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(diff-hl-draw-borders nil)
 '(diff-hl-side (quote left))
 '(enh-ruby-deep-indent-paren nil)
 '(evil-shift-width 2)
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#eee8d5")
 '(git-gutter-fr:side (quote left-fringe) t)
 '(global-flycheck-mode t)
 '(haskell-indent-offset 2)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(if (version< emacs-version "24.4"))
 '(js-indent-level 2)
 '(lua-indent-level 1)
 '(mac-system-move-file-to-trash-use-finder t)
 '(magit-diff-use-overlays nil)
 '(magit-push-arguments nil)
 '(magit-use-overlays nil)
 '(markdown-command "pandoc")
 '(neo-show-updir-line t t)
 '(neo-theme (quote nerd))
 '(org-refile-targets
   (quote
    ((org-agenda-files :maxlevel . 2)
     (nil :maxlevel . 2))))
 '(org-tags-column -117)
 '(paradox-github-token t)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-default-separator (quote slant))
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(scroll-step 1)
 '(sh-basic-offset 2)
 '(sh-indent-after-else 0)
 '(sh-indentation 2)
 '(shm-display-quarantine nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(smooth-scroll-strict-margins nil)
 '(solarized-distinct-fringe-background t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(windmove-wrap-around t)
 '(writeroom-major-modes (quote (text-mode markdown-mode org-mode)))
 '(writeroom-width 120))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(markup-big-face ((t (:inherit markup-gen-face))))
 '(markup-code-face ((t (:inherit markup-gen-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray65" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown"))))
 '(markup-meta-hide-face ((t (:inherit markup-meta-face :foreground "gray75"))))
 '(markup-passthrough-face ((t (:inherit markup-gen-face))))
 '(markup-replacement-face ((t (:foreground "purple3"))))
 '(markup-small-face ((t (:inherit markup-gen-face))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :height 2.0))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :height 1.4))))
 '(markup-title-2-face ((t (:inherit markup-gen-face))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :weight bold))))
 '(markup-typewriter-face ((t (:inherit markup-gen-face))))
 '(variable-pitch ((t nil))))
