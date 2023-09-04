;;; init-vcs.el --- Initialize version control system configurations. -*- lexical-binding: t -*-

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
;; Version control systems.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Git
;; See `magit-maybe-define-global-key-bindings'
(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Access Git forges from Magit
  (use-package forge
    :demand t
    :defines (forge-database-connector forge-topic-list-columns)
    :custom-face
    (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
    :init
    (setq forge-database-connector (if (and (require 'emacsql-sqlite-builtin nil t)
                                            (functionp 'emacsql-sqlite-builtin)
                                            (functionp 'sqlite-open))
                                       'sqlite-builtin
                                     'sqlite)
          forge-topic-list-columns
          '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
            ("Title" 60 t nil title  nil)
            ("State" 6 t nil state nil)
            ("Updated" 10 t nil updated nil)))))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))

;; Git configuration modes
(use-package git-modes)

(provide 'init-vcs)

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init-vcs.el ends here
