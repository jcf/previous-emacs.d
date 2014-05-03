;;; init-packages.el --- Starts up cask and use-package
;;; Commentary:
;;; Code:

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

(provide 'init-packages)
;; init-packages.el ends here
