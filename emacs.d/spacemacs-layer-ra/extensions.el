(defvar ra-pre-extensions
  '(
    ;; pre extension ras go here
    )
  "List of all extensions to load before the packages.")

(defvar ra-post-extensions
  '(
    ;; post extension ras go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function ra/init-<extension-ra>
;;
;; (defun ra/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
