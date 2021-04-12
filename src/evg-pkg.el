;;; -*- lexical-binding: t -*-

;; Copyright (C) 2021 Patrick Freed

;; Author: Patrick Freed
;; URL: https://github.com/patrickfreed/evg.el

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

;;; Commentary:

;;; A frontend for the Evergreen CI system, implemented as an Emacs package.

;;; Code:

(define-package
  "evg"
  "0.1.0"
  "A frontend for the Evergreen CI system, implemented as an Emacs package."
  '((request "0.3.2")))
