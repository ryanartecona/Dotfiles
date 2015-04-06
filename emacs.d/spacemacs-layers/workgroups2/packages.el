(defvar workgroups2-packages
  '(
    workgroups2
    )
  "List of all packages to install to support workgroups2")

(defvar workgroups2-excluded-packages
  '(
    workgroups
    )
  "List of packages that conflict with workgroups2")


(defun workgroups2/init-workgroups2 ()
  "Setup workgroups2"
  (use-package workgroups2
    ;; :defer t
    :demand t
    ;; :commands workgroups-mode

    :init
    (progn
      ;; (setq wg-restore-associated-buffers nil)
      ;; (setq wg-remember-frame-for-each-wg t)
      ; if non-nil, the spacemacs powerline gets clobbered
      (setq wg-mode-line-display-on nil)
      )

    :config
    (progn
      (spacemacs/declare-prefix "W" "workgroups")
      (evil-leader/set-key
        "Wf" 'wg-open-session
        "Ws" 'wg-save-session
        "Wc" 'wg-create-workgroup
        "WC" 'wg-clone-workgroup
        "WW" 'wg-switch-to-workgroup
        "W TAB" 'wg-switch-to-previous-workgroup
        "Wn" 'wg-switch-to-workgroup-left
        "Wp" 'wg-switch-to-workgroup-right
        "Wj" 'wg-switch-to-workgroup-at-index
        "W1" 'wg-switch-to-workgroup-at-index-1
        "W2" 'wg-switch-to-workgroup-at-index-2
        "W3" 'wg-switch-to-workgroup-at-index-3
        "W4" 'wg-switch-to-workgroup-at-index-4
        "W5" 'wg-switch-to-workgroup-at-index-5
        "W6" 'wg-switch-to-workgroup-at-index-6
        "W7" 'wg-switch-to-workgroup-at-index-7
        "W8" 'wg-switch-to-workgroup-at-index-8
        "W9" 'wg-switch-to-workgroup-at-index-9
        "W0" 'wg-switch-to-workgroup-at-index-0
        "WA" 'wg-rename-workgroup
        "WR" 'wg-revert-workgroup
        "Wk" 'wg-kill-workgroup
        "W C-k" 'wg-kill-workgroup-and-buffers
        ;; "WS" 'wg-save-wconfig
        "Wu" 'wg-undo-wconfig-change
        "WU" 'wg-redo-wconfig-change
        "W C-r" 'wg-revert-all-workgroups
        "W!" 'wg-reset
        "W?" 'wg-help
        )
      )
    ))
