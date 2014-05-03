;;; init.el --- Setup Emacs
;;; Commentary:
;;; This is where all the magic happens.
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking)

(defconst *is-a-mac* (eq system-type 'darwin))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 's)
(require 'f)
(require 'git)
(require 'ert)
(require 'evm)
(require 'dash)
(require 'use-package)

(defun load-x (file)
  "Load FILE relative to `user-emacs-directory'."
  (load (f-expand file user-emacs-directory)))

(setq inhibit-startup-message t)

(use-package color-theme-monokai
  :init
  (load-theme 'monokai t))

(load-x "defuns")
(load-x "misc")
(load-x "packages")

(when *is-a-mac*
  (load-x "osx"))

(require 'server)
(unless (server-running-p)
  (server-start))

(load-x "evil-custom")
(load-x "helm-custom")
(load-x "powerline-custom")
(load-x "ruby-custom")

(add-hook 'after-init-hook
          (lambda ()
            (message "Initialisation took %.2fms"
                     (jcf/time-subtract-millis
                      after-init-time
                      before-init-time))))
;;; init.el ends here
