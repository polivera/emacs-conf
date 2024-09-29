;;; env-loader.el --- Load environment file from the project root -*- lexical-binding: t; -*-

;;; Commentary:

;; Create a package that loads an env file from the root of the project.
;; It should be able to:
;;   - Load a file of environment variables.
;;   - Edit an environment variable for the session.
;;   - Disable an environment variable for the session.
;;   - Update an environment variable on the file.
;;   - Delete an environment variable from the file.

;;; Code:




;; Example using projectile to find the root project path.
;; (require 'projectile)

;; (defun get-project-root ()
;;   "Get the root directory of the current Projectile project."
;;   (if (projectile-project-p)
;;       (projectile-project-root)
;;     (message "Not in a project!")))

;; ;; Example usage:
;; (message "Project root: %s" (get-project-root))

(provide 'env-loader)
;;; env-loader.el ends here.
