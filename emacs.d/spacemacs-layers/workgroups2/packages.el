(setq workgroups2-packages
  '(
    workgroups2
    )
  )

(setq workgroups2-excluded-packages
  '(
    workgroups
    )
  )

(defun workgroups2/init-workgroups2 ()
  "Setup workgroups2"
  (use-package workgroups2
    :config
    (progn
      ;; The following needs to happen after (require 'workgroups2),
      ;; but before the call to (workgroups-mode 1). It blocks workgroups-mode
      ;; from clobbering the spacemacs powerline.
      (defun wg-change-modeline () nil)

      ;; The rest of the config can be lazy.
      (add-hook
       'workgroups-mode-hook
       (lambda ()
         (setq frame-title-format '(:eval (if (and workgroups-mode (wg-current-workgroup))
                                              (wg-workgroup-name (wg-current-workgroup))
                                            "---")))

         (spacemacs|diminish workgroups-mode " ⧉" " wg")
         (spacemacs/declare-prefix "W" "workgroups")
         (evil-leader/set-key
           ;; "W"  nil
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

         (add-hook
          'wg-before-switch-to-workgroup-hook
          (lambda () (golden-ratio-mode -1)))
         ))
      )
    ))
