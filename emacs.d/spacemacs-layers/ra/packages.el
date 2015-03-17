(defvar ra-packages
  '(
    flycheck
    coffee-mode
    sass-mode
    )
  "List of all packages to install and/or initialize.")

(defvar ra-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function ra/init-<package-ra>
;;
;; (defun ra/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun ra/init-coffee-mode ()
  "Setup coffee-mode"
  (use-package coffee-mode
    :defer t
    :mode ("\\.coffee\\'" . coffee-mode)))

(defun ra/init-sass-mode ()
  "Setup sass-mode"
  (use-package sass-mode
    :defer t
    :mode ("\\.sass\\'" . sass-mode)))

(defun ra/init-flycheck ()
  "Add flycheck-mode hooks"
  (add-hook 'coffee-mode-hook 'flycheck-mode)
  (add-hook 'sass-mode-hook 'flycheck-mode))
