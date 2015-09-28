(setq ra-packages
  '(
    coffee-mode
    flycheck
    magit
    pandoc-mode
    multi-term
    writeroom-mode
    )
  )

(setq ra-excluded-packages '()
  )


(defun ra/init-coffee-mode ()
  "Setup coffee-mode"
  (use-package coffee-mode
    :defer t
    :mode ("\\.coffee\\'" . coffee-mode)))

(defun ra/post-init-flycheck ()
  "Add flycheck-mode hooks"
  (add-hook 'coffee-mode-hook 'flycheck-mode))

(defun ra/post-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
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

(defun ra/post-init-pandoc-mode ()
  "Add pandoc-mode hooks"
  (spacemacs|use-package-add-hook pandoc-mode
    :post-init
    (progn
      (add-hook 'markdown-mode-hook 'pandoc-mode)
      (add-hook 'org-mode-hook 'pandoc-mode)
      )
    :post-config
    (progn
      (spacemacs|diminish pandoc-mode " ⇔" " pd"))
    ))

(defun ra/post-init-multi-term ()
  (evil-define-key 'insert term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
  (evil-define-key 'insert term-raw-map (kbd "M-<left>") 'term-send-backward-word)
  (evil-define-key 'insert term-raw-map (kbd "M-<right>") 'term-send-forward-word)
  )

(defun ra/init-writeroom-mode ()
  (use-package writeroom-mode))
