;; (defun workgroups2/init-cl-lib () ())
;; (defun workgroups2/init-f () ())
;; (defun workgroups2/init-dash () ())
;; (defun workgroups2/init-ring () ())
;; (defun workgroups2/init-anaphora () ())

;; (defun workgroups2/init-powerline ()
;;   (message "wg2:1 init powerline")
;;   (use-package powerline
;;     :demand t))

;; (defun workgroups2/init-workgroups2()
;;   (message "wg2:2 dummy init workgroups2 package"))

(defun workgroups2/init-workgroups2 ()
  "Setup workgroups2"
  (use-package workgroups2
    :defer t
    ;; :demand t
    ;; :commands workgroups-mode

    :init
    (progn
      ; need to stub this before loading workgroups2,
      ; since workgroups2 takes over the modeline if loaded
      ; before powerline :/
      ;; EDIT: this doesn't work :(
      (defcustom wg-mode-line-display-on nil
        "Toggles Workgroups' mode-line display."
        :type 'boolean
        :group 'spacemacs)

      (evil-leader/set-key
        "W" 'workgroups-mode)
      )

    :config
    (progn
      (spacemacs/declare-prefix "W" "workgroups")
      (evil-leader/set-key
        "W"  nil
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

      (setq frame-title-format '(:eval (wg-workgroup-name (wg-current-workgroup))))
      )
    ))
