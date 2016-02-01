;; add some frame-related bindings
(spacemacs/declare-prefix "F" "frames")
(evil-leader/set-key
  "Fn" 'make-frame-command
  "Fc" 'delete-frame
  "FC" 'delete-other-frames
  "Fo" 'other-frame)

;; make j/k work on visual lines instead of buffer lines (when wrapped)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
