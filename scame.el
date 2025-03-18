;;; scame.el --- simiple configurable agreeable modular emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Felix Dumbeck

;; Author: Felix Dumbeck <felix@dumbeck.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(provide 'scame)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(load "keybindings.el" nil t)
(load "org-mode.el" nil t)
(load "startup.el" nil t)
(load "user-interface.el" nil t)
(load "completion.el" nil t)
(load "var.el" nil t)
(load "dired-settings.el" nil t)
(load "editor-config.el" nil t)
