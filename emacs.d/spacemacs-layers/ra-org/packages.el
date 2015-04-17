(defvar org-packages
  '(
    org
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar org-excluded-packages '()
  "List of packages to exclude.")

(defun org/init-org ()
  "Define decent org-mode evil keybindings."
  (use-package org
    :config
    (evil-leader/set-key-for-mode 'org-mode
      "mh" 'org-metaleft
      "mj" 'org-metadown
      "mk" 'org-metaup
      "ml" 'org-metaright
      "mH" 'org-shiftmetaleft
      "mJ" 'org-shiftmetadown
      "mK" 'org-shiftmetaup
      "mL" 'org-shiftmetaright
      "mS" 'org-sort
      "mpj" 'org-priority-down
      "mpk" 'org-priority-up
      "mt" nil "mtl" 'org-toggle-link-display
      "mi" 'org-clock-in
      "mo" 'org-clock-out
     ))
  )
