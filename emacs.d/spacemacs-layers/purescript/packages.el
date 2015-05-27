(defvar purescript-packages
  '(
    purescript-mode
    psci
    )
  "List of all purescript packages to install and/or initialize.")

(defvar purescript-excluded-packages '()
  "List of packages to exclude.")

(defun purescript/init-purescript-mode ()
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

(defun purescript/init-psci ()
  "Setup psci mode"
  (use-package psci
    :init
    (progn
      (add-hook 'purescript-mode-hook 'inferior-psci-mode)
      (evil-leader/set-key-for-mode 'purescript-mode
        "mr" 'psci)
      )
    )
  )
