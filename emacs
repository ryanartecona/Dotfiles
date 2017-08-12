; -*- mode: emacs-lisp -*-

;; Spacemacs wants to be cloned directly into ~/.emacs.d
;;
;; The following loads spacemacs from ~/.emacs.d/spacemacs
;; by setting it as 'user-emacs-directory


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;(add-to-list 'load-path "~/.emacs.d/spacemacs/")
(setq backup-user-emacs-directory user-emacs-directory
      user-emacs-directory "~/.emacs.d/spacemacs/"
      custom-file "~/.emacs")

;; (setq init-file-debug t)
(load "~/.emacs.d/spacemacs/init")


;; =========
;; Customize
;; =========

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
 '(css-indent-offset 2)
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
 '(evil-want-Y-yank-to-eol nil)
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#eee8d5" t)
 '(git-commit-setup-hook (quote (with-editor-usage-message)))
 '(git-commit-summary-max-length 78)
 '(git-gutter-fr:side (quote left-fringe) t)
 '(global-flycheck-mode t)
 '(haskell-indent-offset 2)
 '(haskell-process-type (quote stack-ghci))
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
 '(ivy-height 25)
 '(js-indent-level 2)
 '(js2-include-node-externs t)
 '(lua-indent-level 1 t)
 '(mac-system-move-file-to-trash-use-finder t)
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
 '(magit-push-arguments nil)
 '(magit-use-overlays nil)
 '(markdown-command "pandoc")
 '(merlin-command "ocamlmerlin" t)
 '(neo-show-updir-line t t)
 '(neo-theme (quote nerd))
 '(org-default-notes-file "~/notes/notes.org")
 '(org-directory "~/notes")
 '(org-refile-targets
   (quote
    ((org-agenda-files :maxlevel . 2)
     (nil :maxlevel . 2))))
 '(org-tags-column -117)
 '(package-selected-packages
   (quote
    (prettier-js company-quickhelp wgrep smex ivy-purpose ivy-hydra counsel-projectile counsel-dash counsel swiper ivy web-mode magithub ghub+ apiwrap ghub info+ dante company-ghc yapfify yaml-mode xterm-color ws-butler writeroom-mode winum which-key web-beautify volatile-highlights uuidgen utop use-package tuareg toc-org tagedit symon string-inflection sql-indent spaceline solarized-theme smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor rubocop rspec-mode robe reveal-in-osx-finder restart-emacs rbenv rake rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode psci psc-ide popwin pip-requirements persp-mode pbcopy password-generator paradox pandoc-mode osx-trash osx-dictionary origami orgit org-projectile org-present org-pomodoro org-download open-junk-file ocp-indent nix-mode nginx-mode neotree multi-term move-text mmm-mode minitest merlin markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl json-mode js2-refactor js-doc intero insert-shebang indent-guide hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-nixos-options helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist ghc gh-md fuzzy flycheck-pos-tip flycheck-mix flycheck-haskell flycheck-credo flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump diff-hl dash-at-point cython-mode csv-mode company-web company-tern company-statistics company-shell company-nixos-options company-lua company-ghci company-cabal company-anaconda column-enforce-mode coffee-mode cmm-mode clean-aindent-mode chruby bundler browse-at-remote bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-default-separator (quote slant))
 '(projectile-enable-caching t)
 '(projectile-indexing-method (quote alien))
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(refmt-width-mode (quote fill))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(sass-indent-offset 1)
 '(scroll-step 1)
 '(sh-basic-offset 2)
 '(sh-indent-after-else 0)
 '(sh-indentation 2)
 '(shm-display-quarantine nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(smie-indent-basic 2)
 '(smooth-scroll-strict-margins nil)
 '(solarized-distinct-fringe-background t)
 '(standard-indent 2)
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
 '(fixed-pitch ((t nil)))
 '(fixed-pitch-serif ((t nil)))
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
