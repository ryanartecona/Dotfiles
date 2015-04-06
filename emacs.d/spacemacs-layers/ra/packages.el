(defvar ra-packages
  '(
    coffee-mode
    sass-mode
    flycheck
    rainbow-mode
    magit
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

(defun ra/init-flycheck () ())

(defun ra/init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      ; From http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
      (defun endless/add-PR-fetch ()
        "If refs/pull is not defined on a GH repo, define it."
        (let ((fetch-address
              "+refs/pull/*/head:refs/pull/origin/*")
              (magit-remotes
              (magit-get-all "remote" "origin" "fetch")))
          (unless (or (not magit-remotes)
                      (member fetch-address magit-remotes))
            (when (string-match
                  "github" (magit-get "remote" "origin" "url"))
              (magit-git-string
              "config" "--add" "remote.origin.fetch"
              fetch-address)))))

      (add-hook 'magit-mode-hook #'endless/add-PR-fetch)
    )))
