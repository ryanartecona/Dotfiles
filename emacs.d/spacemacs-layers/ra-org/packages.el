(setq ra-org-packages
  '(
    org
    )
  )

(setq ra-org-excluded-packages '())

(defun ra-org/init-org ()
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
      "mr" 'org-refile
      "mpj" 'org-priority-down
      "mpk" 'org-priority-up
      "mt" nil "mtl" 'org-toggle-link-display
      "mi" 'org-clock-in
      "mo" 'org-clock-out
     ))
  )
