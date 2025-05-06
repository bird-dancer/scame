;;; base.el --- simiple configurable agreeable modular emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Felix Dumbeck

;; Author: Felix Dumbeck <felix@dumbeck.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the Lesser GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "base/startup.el" nil t)
(load "base/user-interface.el" nil t)
(load "base/keybindings.el" nil t)
(load "base/completion.el" nil t)
(load "base/org-mode.el" nil t)
(load "base/var.el" nil t)
(load "base/dired-settings.el" nil t)
(load "base/editor-config.el" nil t)
(load "base/melpa.el" nil t)
