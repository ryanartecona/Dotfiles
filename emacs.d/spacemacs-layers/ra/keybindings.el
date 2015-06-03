;; add some frame-related bindings
(spacemacs/declare-prefix "wf" "frames")
(evil-leader/set-key
  "wfn" 'make-frame-command
  "wfc" 'delete-frame
  "wfC" 'delete-other-frames
  "wfo" 'other-frame)

;; make j/k work on visual lines instead of buffer lines (when wrapped)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
