(setq ra-purescript-packages
  '(
    purescript-mode
    psci
    )
  )

(setq ra-purescript-excluded-packages '())

(defun ra-purescript/init-purescript-mode ()
  "Setup purescript-mode"
  (require 'purescript-mode-autoloads)
  (evil-leader/set-key-for-mode 'purescript-mode
    "mh" 'purescript-pursuit

    "md" 'purescript-delete-nested
    "mk" 'purescript-kill-nested
    "mH" 'purescript-move-nested-left
    "mL" 'purescript-move-nested-right

    "mgi" 'purescript-navigate-imports-go
    "mgr" 'purescript-navigate-imports-return

    "mm" nil
    "mma" 'purescript-align-imports
    "mms" 'purescript-sort-imports
    "mmf" 'purescript-mode-format-imports

    "mie" 'purescript-indent-insert-equal
    "mig" 'purescript-indent-insert-guard
    "mio" 'purescript-indent-insert-otherwise
    "miw" 'purescript-indent-insert-where

    "mti" 'purescript-indent-mode
    "mts" 'purescript-simple-indent-mode
    "mtd" 'purescript-decl-scan-mode
    )
  )

(defun ra-purescript/init-psci ()
  "Setup psci mode"
  (use-package psci
    :init
    (progn
      ;; inferior-psci-mode doesn't offer much beyond a binding
      ;; for 'psci, which I can do myself, and it behaves weirdly
      ;; sometimes when inserting newlines in purescript-mode.
      ;; (add-hook 'purescript-mode-hook 'inferior-psci-mode)
      (evil-leader/set-key-for-mode 'purescript-mode
        "mr" 'psci)
      )
    )
  )
