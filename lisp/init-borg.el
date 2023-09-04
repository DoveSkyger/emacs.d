;;; init-borg.el --- Initialize borg configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2023 Wei Zong

;; Author: Wei Zong <sky-ger@live.com>
;; URL: https://github.com/Doveskyger/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Borg configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Initialize borg
(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;; Setup `use-package'
(eval-and-compile ; `use-package'
  (setq use-package-enable-imenu-support t
        use-package-expand-minimally t
        use-package-compute-statistics t
	use-package-always-defer t)
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)

;; Browse the Emacsmirror package database
(use-package epkg
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))

;; Setup `auto-compile'
(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(provide 'init-borg)

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init-borg.el ends here
