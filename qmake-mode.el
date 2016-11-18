;; qmake.el -- qmake mode for emacs
;;
;; Author: Carl Olsen
;;
;; Copyright (c) 2010, carl-olsen
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.

;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.

;;     * Neither the name of the CoCode nor the names of its
;;       contributors may be used to endorse or promote products
;;       derived from this software without specific prior written
;;       permission.


;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;
;; Many thanks to the contributors:
;; Stephan Creutz



;; -------------------------------------------------------
;; provides qmake-mode for emacs
;; qmake is Qt specific make file.
;; Since there exists no other (what i know of)
;; qmake mode for emacs, i decided to create one.
;; If you think this looks messy, its because
;; Im by know way any hacker when it comes to
;; lisp and in particular elisp. If you find
;; any errors or have any good suggestions,
;; don't hesitate to give me a mail, or a
;; diff.
;;
;;-------------------------------------------------------
(provide 'qmake-mode)

;; User define variable, set to which qmake to use also add 
;; possible arguments for qmake.
;; To set it use M-x set-variable <ret> qmake-command-str 
;; <ret> "/usr/local/bin/qmake"
(defvar qmake-command-str "qmake -Wall"
  "*This variable will set the  qmake whereabout and options for compiling"
  )



;;-------------------------------------------------------
;; There is no more user defined variables under this line.
;;-------------------------------------------------------
(defvar qmake-mode-hook nil)

(defface qmake-face-platform
      '((t (:foreground "#190eef")))
      "Blue")

(setq qmake-indent-width 4)

(defvar qmake-functions-variables
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
  "Qmake function types"
  )

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
  "Qmake variables"
  )

(defvar qmake-functions-regexp (regexp-opt qmake-functions-variables 'words))
(defvar qmake-variables-regexp (regexp-opt qmake-variables 'words))


(setq qmake-functions-variables nil)
(setq  qmake-variables nil)



(setq qmake-key-words
      (list
       '("#.*" . font-lock-comment-face)
       `(,qmake-functions-regexp . ,font-lock-function-name-face)
       `(,qmake-variables-regexp . ,font-lock-builtin-face)
       )
      )


(add-to-list 'auto-mode-alist '("\\.pro\\'" . qmake-mode))



(defvar qmake-mode-syntax-table
  (let ((new-table (make-syntax-table)))
		(modify-syntax-entry ?# "<" new-table)
		(modify-syntax-entry ?\n ">" new-table)
		new-table)
  "Syntax table for qmake-mode.")

(define-derived-mode qmake-mode fundamental-mode
  "Major mode for qmake project files"
  :syntax-table qmake-mode-syntax-table
  (setq font-lock-defaults '(qmake-key-words))
  (setq mode-name "qmake")
  (set (make-local-variable 'indent-line-function) 'qmake-ident-line)
  (set (make-local-variable 'comment-start) "# ")
)


(defun qmake-ident-line()
  "Trying to make a qmake identation"
(interactive)
(let ((savep (> (current-column) (current-indentation)))
      (indent (condition-case nil (max (qmake-calculate-indentation) 0)
                (error 0))))
  (if savep
      (save-excursion (indent-line-to indent))
    (indent-line-to indent)
    )
  )
)


(defun qmake-calculate-indentation()
   "This fucntion calculates the indentation for the "
   (beginning-of-line)

   (let (
         (new-indent (get-prev-line-indent))
         ( prev-indent (get-prev-line-indent) )
         )

     ;; If we are at the start of the line
     ;; there is no indentation.
     (if (bobp)
         0
       ;; Check if the previous line is a {
       ;; In that case add on a new indent width...
       (if (is-prev-curly-forward)
           (setq new-indent (+ prev-indent qmake-indent-width))
         )
       ;; If the current line is a } closing bracket,
       ;; remove identation width
       (if (looking-at "[ \t]*}")
           (setq new-indent (- prev-indent qmake-indent-width))
        )


       )


     (if (> 0 new-indent)
         (progn
           (message "Damn %d" new-indent)
           0)
       (progn
         (message "indent %d" new-indent)
         new-indent)
       )
     )
   )






(defun get-prev-line-indent()
  "Gets the previous line indentation"
  (progn
    (forward-line -1)
    (let (
          ( prev-indent (current-indentation))
          )
      (forward-line 1)
      prev-indent)
    )
  )






(defun is-prev-curly-forward()
  "Checks if the previous line contains a curly bracket, that is \"{\" "
  (interactive)
  (beginning-of-line)
  (forward-line -1)
  (if (looking-at "[ \t]*\\(}[a-z]*\\|\\([a-z]*\\)\\((.+,.+)\\|(.+)\\|(.+,.+,.+)\\)\\|\\)[ \t]*{")
      (progn
        (message "Curly forward")
        (forward-line 1)
        t)
    (progn
      (message "Ingen bracket")
      (forward-line 1)
      nil)
    )
  )

;; code to remove the whole menu panel
;;(global-unset-key [menu-bar qmake-menu])
(defun qmake-compile()
  "Testing compile"
  (interactive)
  (let (
        (file-name (buffer-file-name))
        (cur-buffer (buffer-name))
        (line-number-list ())
        (first-error nil)
        )
    (progn 
      (shell-command (concat qmake-command-str " " file-name))
      (switch-to-buffer-other-window "*Shell Command Output*")
      (qmake-highlight-error)
      ;;Need to check wheter car returns nil
      (setq first-error (car (qmake-compile-search-for-errors)))
      (if first-error
          (progn
           (goto-char first-error)
           (setq line-number-list (qmake-compile-get-line-nr-from-error))
           (switch-to-buffer-other-window cur-buffer)
           (goto-line (car line-number-list))
           )
        (progn
          (delete-windows-on "*Shell Command Output*")
          (message "Compiled successfully")
          )
        
      )
    )
  )
)
  





(defun qmake-compile-search-for-errors()
  "interactive"
  (interactive)
  (goto-char (point-min))
  (let (
        (my-point (search-forward-regexp ":[0-9]*:" (point-max) t))
        (qmake-compile-error-points ())
        )
    (while (integerp my-point)
      (progn 
        (push my-point qmake-compile-error-points)
        (goto-char my-point)
        (setq my-point (search-forward-regexp ":[0-9]*:" (point-max) t))
        )
      )
    qmake-compile-error-points
    )
  )




(defun qmake-highlight-error()
  "Highlight error in the compile buffer"
  (save-excursion)
  (highlight-regexp ":[0-9]*:")
  
  )


;; The basic idea is to jump to the line number of error

(defun qmake-compile-get-line-nr-from-error()
  "reads the number for which the compilation has failed and returns a list
   on this"
  (interactive)
  (goto-char (point-min))
  (let (
        (point-list (qmake-compile-list-error))
        (line-nr-list ())
        ( current-point )
        )
    (while (> (length point-list) 0)
      (setq current-point (pop point-list))
      (goto-char current-point)
      (push (qmake-compile-error-line-nr current-point) line-nr-list)
      )
    line-nr-list
    )
  )



(defun qmake-compile-list-error()
  "Search through the buffer after compile errors, 
   it then returns a list of these points"
  (let (
        (my-point (search-forward-regexp ":[0-9]*:" (point-max) t))
        (error-points ())
        )
    (while (integerp my-point)
      (progn 
        (push my-point error-points)
        (goto-char my-point)
        (setq my-point (search-forward-regexp ":[0-9]*:" (point-max) t))
        )
      )
    error-points
    )
  )


(defun qmake-compile-error-line-nr(current-pos)
  "Returns the number for which the error occured while compiling
   For example asdha/asdas:19: 
                              ^- Current-pos (after search-forward-regexp)
   Will return number 19"
  (let (
        (start-of-line (progn
                         (goto-char current-pos)
                         (move-to-column 0)
                         (point)
                         )
                       )
        (error-line-text nil)
        (st-error-point nil)
        (end-error-point nil)
        )
    (progn 
      (setq error-line-text (buffer-substring-no-properties start-of-line current-pos))
      (setq st-error-point (+ (+ (string-match ":[0-9]+" error-line-text) 1) start-of-line))
      (string-to-number (buffer-substring-no-properties st-error-point  current-pos))
      )
    )
  )
