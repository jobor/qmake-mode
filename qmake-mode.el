;; qmake-mode.el -- Support for qmake project files
;;
;; Copyright (C) 2020 Joerg Bornemann
;; Copyright (C) 2010 Carl Olsen
;;
;; Author: Joerg Bornemann
;;     Carl Olsen
;; Keywords: languages
;; Homepage: https://github.com/jobor/qmake-mode/
;;

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a fork of Carl Olsen's qmake.el focusing on QMake's language.
;; Every attempt of managing projects has been removed together with all UI elements.
;; The syntax handling is a complete rewrite.

;;; Code:

(require 'cl)

(defvar qmake-indent-width 4
  "Indentation width for qmake-mode")

(defvar qmake-functions
  '("absolute_path"
    "basename"
    "break"
    "cache"
    "cat"
    "clean_path"
    "clear"
    "contains"
    "count"
    "debug"
    "defined"
    "defineReplace"
    "defineTest"
    "dirname"
    "discard_from"
    "else"
    "enumerate_vars"
    "equals"
    "error"
    "escape_expand"
    "eval"
    "exists"
    "export"
    "files"
    "find"
    "first"
    "for"
    "format_number"
    "fromfile"
    "getenv"
    "greaterThan"
    "if"
    "include"
    "infile"
    "isActiveConfig"
    "isEmpty"
    "isEqual"
    "join"
    "last"
    "lessThan"
    "list"
    "load"
    "log"
    "lower"
    "member"
    "message"
    "mkpath"
    "next"
    "num_add"
    "parseJson"
    "prompt"
    "qoute"
    "quote"
    "re_escape"
    "relative_path"
    "replace"
    "requires"
    "resolve_depends"
    "return"
    "reverse"
    "section"
    "shadowed"
    "shell_path"
    "shell_quote"
    "size"
    "sort_depends"
    "sorted"
    "split"
    "sprintf"
    "str_member"
    "str_size"
    "system"
    "system_path"
    "system_quote"
    "take_first"
    "take_last"
    "title"
    "touch"
    "unique"
    "unix"
    "unset"
    "upper"
    "val_escape"
    "warning"
    "win32"
    "write_file"
    "mac")
  "QMake functions")

(defvar qmake-variables
  '("CONFIG"
    "DEFINES"
    "DEF_FILE"
    "DEPENDPATH"
    "DEPLOYMENT_PLUGIN"
    "DESTDIR"
    "DESTDIR_TARGET"
    "DISTFILES"
    "DLLDESTDIR"
    "DSP_TEMPLATE"
    "FORMS"
    "FORMS3"
    "GUID"
    "HEADERS"
    "ICON"
    "IDLSOURCES"
    "INCLUDEPATH"
    "INSTALLS"
    "LEXIMPLS"
    "LEXOBJECTS"
    "LEXSOURCES"
    "LIBS"
    "LITERAL_DOLLAR"
    "LITERAL_HASH"
    "LITERAL_WHITESPACE"
    "MAKEFILE"
    "MAKEFILE_GENERATOR"
    "MOC_DIR"
    "MSVCPROJ_*"
    "OBJECTS"
    "OBJECTS_DIR"
    "OBJMOC"
    "OTHER_FILES"
    "OUT_PWD"
    "POST_TARGETDEPS"
    "PRECOMPILED_HEADER"
    "PRE_TARGETDEPS"
    "PWD"
    "QMAKE"
    "QMAKESPEC"
    "QMAKE_APP_FLAG"
    "QMAKE_APP_OR_DLL"
    "QMAKE_AR_CMD"
    "QMAKE_BUNDLE_DATA"
    "QMAKE_BUNDLE_EXTENSION"
    "QMAKE_CC"
    "QMAKE_CFLAGS"
    "QMAKE_CFLAGS_DEBUG"
    "QMAKE_CFLAGS_MT"
    "QMAKE_CFLAGS_MT_DBG"
    "QMAKE_CFLAGS_MT_DLL"
    "QMAKE_CFLAGS_MT_DLLDBG"
    "QMAKE_CFLAGS_RELEASE"
    "QMAKE_CFLAGS_SHLIB"
    "QMAKE_CFLAGS_THREAD"
    "QMAKE_CFLAGS_WARN_OFF"
    "QMAKE_CFLAGS_WARN_ON"
    "QMAKE_CLEAN"
    "QMAKE_CXX"
    "QMAKE_CXXFLAGS"
    "QMAKE_CXXFLAGS_DEBUG"
    "QMAKE_CXXFLAGS_MT"
    "QMAKE_CXXFLAGS_MT_DBG"
    "QMAKE_CXXFLAGS_MT_DLL"
    "QMAKE_CXXFLAGS_MT_DLLDBG"
    "QMAKE_CXXFLAGS_RELEASE"
    "QMAKE_CXXFLAGS_SHLIB"
    "QMAKE_CXXFLAGS_THREAD"
    "QMAKE_CXXFLAGS_WARN_OFF"
    "QMAKE_CXXFLAGS_WARN_ON"
    "QMAKE_DISTCLEAN"
    "QMAKE_EXTENSION_SHLIB"
    "QMAKE_EXTENSION_STATICLIB"
    "QMAKE_EXTRA_COMPILERS"
    "QMAKE_EXTRA_TARGETS"
    "QMAKE_EXT_CPP"
    "QMAKE_EXT_H"
    "QMAKE_EXT_LEX"
    "QMAKE_EXT_MOC"
    "QMAKE_EXT_OBJ"
    "QMAKE_EXT_PRL"
    "QMAKE_EXT_UI"
    "QMAKE_EXT_YACC"
    "QMAKE_FAILED_REQUIREMENTS"
    "QMAKE_FILETAGS"
    "QMAKE_FRAMEWORK_BUNDLE_NAME"
    "QMAKE_FRAMEWORK_VERSION"
    "QMAKE_HOST"
    "QMAKE_INCDIR"
    "QMAKE_INCDIR_EGL"
    "QMAKE_INCDIR_OPENGL"
    "QMAKE_INCDIR_OPENGL_ES2"
    "QMAKE_INCDIR_OPENVG"
    "QMAKE_INCDIR_QT"
    "QMAKE_INCDIR_THREAD"
    "QMAKE_INCDIR_X11"
    "QMAKE_INFO_PLIST"
    "QMAKE_LFLAGS"
    "QMAKE_LFLAGS_APP"
    "QMAKE_LFLAGS_CONSOLE"
    "QMAKE_LFLAGS_CONSOLE_DLL"
    "QMAKE_LFLAGS_DEBUG"
    "QMAKE_LFLAGS_PLUGIN"
    "QMAKE_LFLAGS_QT_DLL"
    "QMAKE_LFLAGS_RELEASE"
    "QMAKE_LFLAGS_REL_RPATH"
    "QMAKE_LFLAGS_RPATH"
    "QMAKE_LFLAGS_RPATHLINK"
    "QMAKE_LFLAGS_SHAPP"
    "QMAKE_LFLAGS_SHLIB"
    "QMAKE_LFLAGS_SONAME"
    "QMAKE_LFLAGS_THREAD"
    "QMAKE_LFLAGS_WINDOWS"
    "QMAKE_LFLAGS_WINDOWS_DLL"
    "QMAKE_LIBDIR"
    "QMAKE_LIBDIR_EGL"
    "QMAKE_LIBDIR_FLAGS"
    "QMAKE_LIBDIR_OPENGL"
    "QMAKE_LIBDIR_OPENVG"
    "QMAKE_LIBDIR_QT"
    "QMAKE_LIBDIR_X11"
    "QMAKE_LIBS"
    "QMAKE_LIBS_CONSOLE"
    "QMAKE_LIBS_EGL"
    "QMAKE_LIBS_OPENGL"
    "QMAKE_LIBS_OPENGL_ES1, QMAKE_LIBS_OPENGL_ES2"
    "QMAKE_LIBS_OPENGL_QT"
    "QMAKE_LIBS_OPENVG"
    "QMAKE_LIBS_QT"
    "QMAKE_LIBS_QT_DLL"
    "QMAKE_LIBS_QT_OPENGL"
    "QMAKE_LIBS_QT_THREAD"
    "QMAKE_LIBS_RT"
    "QMAKE_LIBS_RTMT"
    "QMAKE_LIBS_THREAD"
    "QMAKE_LIBS_WINDOWS"
    "QMAKE_LIBS_X11"
    "QMAKE_LIBS_X11SM"
    "QMAKE_LIB_FLAG"
    "QMAKE_LINK_SHLIB_CMD"
    "QMAKE_LN_SHLIB"
    "QMAKE_MACOSX_DEPLOYMENT_TARGET"
    "QMAKE_MAC_SDK"
    "QMAKE_MAKEFILE"
    "QMAKE_MOC_SRC"
    "QMAKE_POST_LINK"
    "QMAKE_PRE_LINK"
    "QMAKE_PROJECT_NAME"
    "QMAKE_QMAKE"
    "QMAKE_QT_DLL"
    "QMAKE_REL_RPATH_BASE"
    "QMAKE_RESOURCE_FLAGS"
    "QMAKE_RPATHDIR"
    "QMAKE_RPATHLINKDIR"
    "QMAKE_RUN_CC"
    "QMAKE_RUN_CC_IMP"
    "QMAKE_RUN_CXX"
    "QMAKE_RUN_CXX_IMP"
    "QMAKE_SONAME_PREFIX"
    "QMAKE_TARGET"
    "QMAKE_TARGET_COMPANY"
    "QMAKE_TARGET_COPYRIGHT"
    "QMAKE_TARGET_DESCRIPTION"
    "QMAKE_TARGET_PRODUCT"
    "QMAKE_UIC"
    "QMAKE_USE"
    "QMAKE_USE_PRIVATE"
    "QT"
    "QTPLUGIN"
    "QT_MAJOR_VERSION"
    "QT_MINOR_VERSION"
    "QT_PATCH_VERSION"
    "QT_VERSION"
    "RCC_DIR"
    "RC_CODEPAGE"
    "RC_DEFINES"
    "RC_FILE"
    "RC_ICONS"
    "RC_INCLUDEPATH"
    "RC_LANG"
    "REQUIRES"
    "RESOURCES"
    "RES_FILE"
    "SIGNATURE_FILE"
    "SOURCES"
    "SRCMOC"
    "SUBDIRS"
    "TARGET"
    "TARGET_EXT"
    "TARGET_x"
    "TARGET_x.y.z"
    "TEMPLATE"
    "TRANSLATIONS"
    "UICIMPLS"
    "UICOBJECTS"
    "UI_DIR"
    "UI_HEADERS_DIR"
    "UI_SOURCES_DIR"
    "VERSION"
    "VERSION_PE_HEADER"
    "VER_MAJ"
    "VER_MIN"
    "VER_PAT"
    "VPATH"
    "WINRT_MANIFEST"
    "YACCIMPLS"
    "YACCOBJECTS"
    "YACCSOURCES"
    "_LINE_"
    "_FILE_"
    "_PRO_FILE_"
    "_PRO_FILE_PWD_")
  "QMake variables")

(defvar qmake-functions-regexp (regexp-opt qmake-functions 'words))
(defvar qmake-variables-regexp (regexp-opt qmake-variables 'words))

(setq qmake-functions nil)
(setq qmake-variables nil)

(setq qmake-key-words
      (list
       '("#.*" . font-lock-comment-face)
       `(,qmake-functions-regexp . ,font-lock-function-name-face)
       `(,qmake-variables-regexp . ,font-lock-builtin-face)
       )
      )

(add-to-list 'auto-mode-alist '("\\.pr[oifl]\\'" . qmake-mode))

(defvar qmake-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?$ "'" table)
    table)
  "Syntax table for qmake-mode.")

(define-derived-mode qmake-mode prog-mode "QMake"
  "Major mode for qmake project files"
  :syntax-table qmake-mode-syntax-table
  (setq font-lock-defaults '(qmake-key-words))
  (set (make-local-variable 'indent-line-function) 'qmake-indent-line)
  (set (make-local-variable 'comment-start) "# ")
)

(defun qmake-indent-line ()
  "Indent current line as QMake code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (qmake-calculate-indentation)))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun qmake--looking-at-continuation ()
  "Check if we're looking-at a line continuation, e.g. \"    mainwindow.h \\\"."
  (looking-at ".*\\\\[[:space:]]*\\(#.*\\)?$"))

(defun qmake--looking-at-continuation-start ()
  "Check if we're looking-at the start of a line continuation."
  (when (qmake--looking-at-continuation)
    (if (= (line-number-at-pos) 1)
        t
      (let (result)
        (previous-line)
        (setf result (not (qmake--looking-at-continuation)))
        (forward-line)
        result))))

(defun qmake--looking-at-empty-line-or-comment ()
  (looking-at "[[:space:]]*\\(#.*\\)?$"))

(defun qmake--looking-at-relevant-content ()
  (not (qmake--looking-at-empty-line-or-comment)))

(cl-defun qmake--indentation-of-last-continuation-start ()
  (save-excursion
    (let ((indentation 0))
      (while (> (line-number-at-pos) 1)
        (previous-line)
        (beginning-of-line)
        (setf indentation (current-indentation))
        (if (qmake--looking-at-continuation-start)
            (return-from qmake--indentation-of-last-continuation-start indentation))))))

(cl-defun qmake--scan-backwards ()
  (save-excursion
    (let ((indentation 0))
      (while (> (line-number-at-pos) 1)
        (catch 'continue
          (previous-line)
          (beginning-of-line)
          (if (qmake--looking-at-empty-line-or-comment)
              (throw 'continue nil))
          (setf indentation (current-indentation))
          (if (looking-at ".*{[[:space:]]*\\(#.*\\)?$")
              (return-from qmake--scan-backwards
                (values 'opening-curly-bracket indentation)))
          (when (qmake--looking-at-continuation)
            (when (= (line-number-at-pos) 1)
              (return-from qmake--scan-backwards
                (values 'continuation-start indentation)))
            (when (> (line-number-at-pos) 1)
              (previous-line)
              (if (not (qmake--looking-at-continuation))
                  (return-from qmake--scan-backwards
                    (values 'continuation-start indentation))))
            (return-from qmake--scan-backwards
              (values 'continuation indentation)))
          (when (qmake--looking-at-relevant-content)
            (when (> (line-number-at-pos) 1)
              (previous-line)
              (if (qmake--looking-at-continuation)
                  (return-from qmake--scan-backwards
                    (values 'continuation-end indentation))))
            (return-from qmake--scan-backwards
              (values 'relevant-content indentation)))))))
  (values 'unknown-content 0))

(cl-defun qmake-calculate-indentation()
   "This function calculates the indentation for the current line"
   (if (= (line-number-at-pos) 1)
       (return-from qmake-calculate-indentation 0))

   ;; Handle closing curly brace.
   ;; Return the indentation of the matching opening curly brace.
   ;; If there is no matching brace, indent like normal text.
   (save-excursion
     (ignore-errors
       (beginning-of-line)
       (when (looking-at "[[:space:]]*}")
         (backward-up-list)
         (return-from qmake-calculate-indentation (current-indentation)))))

   ;; Scan backwards to the last "interesting" location, to find out
   ;; in which context we're currently in.
   (multiple-value-bind (type indentation) (qmake--scan-backwards)
     (case type
       ((opening-curly-bracket
         continuation-start)
        (return-from qmake-calculate-indentation (+ indentation qmake-indent-width)))
       ((continuation-end)
        (return-from qmake-calculate-indentation (qmake--indentation-of-last-continuation-start))))
     indentation))

(defun qmake-ffap (arg)
  "find-file-at-point helper function for qmake-mode.

Add this function to your ffap-alist,
and you can ffap on strings like
  $$PWD/main.cpp

Example:
(add-to-list 'ffap-alist '(qmake-mode . qmake-ffap))
"
  (cond ((string-match "\\$?\\$PWD/\\(.*\\)" arg)
         (concat "./" (match-string 1 arg)))
        ((string-match "\\$?\\${PWD}/\\(.*\\)" arg)
         (concat "./" (match-string 1 arg)))))

(provide 'qmake-mode)

;;; qmake-mode.el ends here
