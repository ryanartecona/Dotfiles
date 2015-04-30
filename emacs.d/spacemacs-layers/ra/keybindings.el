(spacemacs/declare-prefix "wf" "frames")
(evil-leader/set-key
  "wfn" 'make-frame-command
  "wfc" 'delete-frame
  "wfC" 'delete-other-frames
  "wfo" 'other-frame)

(when (equal system-type 'darwin)
  ;; Make some OSX idioms work
  (global-set-key (kbd "s-n") 'make-frame-command)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-<right>") 'evil-end-of-line)
  (global-set-key (kbd "s-<left>") 'evil-first-non-blank)
  (global-set-key (kbd "s-S-<up>") 'evil-window-up)
  (global-set-key (kbd "s-S-<down>") 'evil-window-down)
  (global-set-key (kbd "s-S-<left>") 'evil-window-left)
  (global-set-key (kbd "s-S-<right>") 'evil-window-right)
  )
