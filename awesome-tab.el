;;; awesome-tab.el --- Provide an out of box configuration to use tab in Emacs.

;; Filename: awesome-tab.el
;; Description: Provide an out of box configuration to use awesome-tab in Emacs.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-17 22:14:34
;; Version: 6.5
;; Last-Updated: 2020-02-13 13:15:09
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-tab.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl' `cl-lib' `color' `which-func'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Provide an out of box configuration to use tab in Emacs.
;;

;;; Installation:
;;
;; Put awesome-tab.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-tab)
;; (awesome-tab-mode t)
;;
;; No need more.
;;
;; You can use below commands for you need:
;;
;; `awesome-tab-switch-group'
;; `awesome-tab-select-beg-tab'
;; `awesome-tab-select-end-tab'
;; `awesome-tab-forward-tab-other-window'
;; `awesome-tab-backward-tab-other-window'
;; `awesome-tab-kill-other-buffers-in-current-group'
;; `awesome-tab-kill-all-buffers-in-current-group'
;; `awesome-tab-kill-match-buffers-in-current-group'
;; `awesome-tab-keep-match-buffers-in-current-group'
;; `awesome-tab-move-current-tab-to-left'
;; `awesome-tab-move-current-tab-to-right'
;;
;; If you're helm fans, you need add below code in your helm config:
;;
;; (awesome-tab-build-helm-source)
;;

;;; Customize:
;;
;; `awesome-tab-cycle-scope'
;; `awesome-tab-label-fixed-length'
;; `awesome-tab-auto-scroll-flag'
;; `awesome-tab-common-group-name'
;; `awesometab-hide-tabs-hooks'
;; `awesome-tab-height'
;; `awesome-tab-face-height'
;; `awesome-tab-style'
;; `awesome-tab-display-sticky-function-name'
;; `awesome-tab-display-icon'
;;

;;; Change log:
;;
;; 2020/02/13
;;      * Add `awesome-tab-all-the-icons-is-load-p' option.
;;      * Fix window-live-p error when click tab.
;;
;; 2020/01/13
;;      * Add new option `awesome-tab-label-max-length'.
;;
;; 2019/12/22
;;      * Add flycheck temp buffer in hide black list.
;;
;; 2019/10/10
;;      * Fix issue #58 click tab select window of tab first.
;;
;; 2019/09/12
;;      * Fix top-line above tab issue.
;;
;; 2019/08/13
;;      * Add new option `awesome-tab-face-height'.
;;
;; 2019/08/03
;;      * Adjust default value of `awesome-tab-ace-quit-keys'.
;;
;; 2019/08/02
;;      * Refactroy `awesome-tab-ace-jump'.
;;      * Refactory variable name.
;;      * Remove `awesome-tab-adjust-buffer-order' since `awesome-tab-ace-jump' is enough.
;;
;; 2019/08/01
;;      * Quit when user press Ctrl + g.
;;      * Make ace string use foreground same as `font-lock-function-name-face'.
;;      * Remove `awesome-tab-prefix-map'
;;
;; 2019/07/18
;;      * Use ema2159's way to render icon.
;;
;; 2019/07/17
;;      * Init `header-line' height from `default' face,
;;        `header-line' default inhibit from `mode-line',
;;        awesome-tab icon will disappear if `mode-line' height set with 0.1 by other plugins (such as awesome-tray).
;;      * Use `stringp' instead `ignore-errors' in `awesome-tab-icon-for-tab'.
;;
;; 2019/07/15
;;      * Don't call `awesome-tab-adjust-buffer-order' if user use mouse click tab.
;;
;; 2019/07/01
;;      * Make awesome-tab's colors change with user selected theme, thank you so much AmaiKinono.
;;      * Adjust dark mode background tab's color.
;;      * Remove local-mode code.
;;      * Refactory code.
;;
;; 2019/06/30
;;      * Add customize option `awesome-tab-display-icon' .
;;
;; 2019/06/28
;;      * Fix messages buffer icon an FontAwesome errors, thanks ema2159. ;)
;;      * Set height of tab face, avoid tab render error when user don't load any third theme.
;;      * Make `header-line' background same as default face.
;;
;; 2019/06/26
;;      * Fix error of void function awesome-tab-separator-separator-height
;;
;; 2019/06/25
;;      * Still display tab if all-the-icons cause "Error during redisplay" error in MacOS.
;;
;; 2019/06/24
;;      * Use ema2159's patch to fix icon face performance.
;;
;; 2019/06/23
;;      * Render file icon in tab when `all-the-icons' is load.
;;      * Use `all-the-icons-icon-for-buffer' to display icon for dired mode.
;;      * Support color icon.
;;      * Don't customize background of tab, use `default' face's background as tab background.
;;
;; 2019/04/14
;;      * Make `awesome-tab-last-sticky-func-name' default with nil.
;;
;; 2019/03/21
;;      * Make `awesome-tab-last-sticky-func-name' as buffer variable.
;;
;; 2019/03/19
;;      * If `tab-index' more than length of visible tabs, selet the last tab.
;;
;; 2019/03/18
;;      * Add new command `awesome-tab-select-visible-tab'.
;;
;; 2019/03/16
;;      * Fix integerp error.
;;
;; 2019/03/14
;;      * Try to fix numberp error.
;;
;; 2019/03/12
;;      * Display sticky function name in tab.
;;
;; 2019/03/09
;;      * Absorb powerline code, keep single file.
;;      * Remove some separator face that not suitable for displaying tab.
;;      * Add option `awesome-tab-style'.
;;
;; 2019/03/07
;;      * Add `cl' dependence.
;;
;; 2019/03/03
;;      * Automatically adsorb tabs after switching tabs, making switch tabs quickly.
;;      * Fix many typo errors.
;;      * Add `awesome-tab-adjust-buffer-order-function'.
;;      * Don't trigger by awesome-tab command, it's annoying.
;;
;; 2019/02/23
;;      * Significantly optimize the performance of switching tab by avoiding excessive calls `project-current'.
;;      * Use `powerline' render tab, it's beautiful!!!
;;
;; 2018/12/27
;;      * Tab will hide if ```awesome-tab-hide-tab-function``` return t, you can write your own code to customize hide rules.
;;
;; 2018/11/16
;;  * Open new tab on right of current one.
;;
;; 2018/11/14
;;      * Remove wheel features, emacser should only use the keyboard to operate Emacs.
;;
;; 2018/11/01
;;      * Remove `projectile' depend.
;;
;; 2018/10/29
;;      * Add `mwheel' depend.
;;
;; 2018/09/29
;;      * Add new command `awesome-tab-kill-other-buffers-in-current-group'
;;      * Not enable mode default.
;;
;; 2018/09/25
;;      * Adjust magit regexp to only match magit buffer, not file that named with magit.
;;
;; 2018/09/22
;;      * Adjust `awesome-tab-buffer-list' to hide unused buffer to user.
;;
;; 2018/09/20
;;      * Remove empty header line from magit buffers.
;;      * Add new function `awesome-tab-kill-match-buffers-in-current-group', it's handy in mixin mode, such as web-mode.
;;      * Add new function `awesome-tab-keep-match-buffers-in-current-group', it's handy in mixin mode, such as web-mode.
;;      * Fix error cause by `awesome-tab-kill-buffer-match-rule'.
;;
;; 2018/09/18
;;      * Fix unselect tab height and add option `awesome-tab-hide-tab-rules'
;;      * Use `awesome-tab-groups-hash' store every buffer's project name avoid performance issue cause by `projectile-project-name'.
;;
;; 2018/09/17
;;      * First released.
;;

;;; Acknowledgements:
;;
;; casouri: documentation and many useful patches.
;; AmaiKinono: contributed to the patch that make tab color change with the theme automatically
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'cl)
(require 'cl-lib)
(require 'color)
(require 'which-func)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;; Awesome-Tab source code ;;;;;;;;;;;;;;;;;;;;;;;

(defgroup awesome-tab nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defcustom awesome-tab-cycle-scope nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- default
    Navigate through visible tabs, then through tab groups."
  :group 'awesome-tab
  :type '(choice :tag "Cycle through..."
                 (const :tag "Visible Tabs Only" tabs)
                 (const :tag "Tab Groups Only" groups)
                 (const :tag "Visible Tabs then Tab Groups" nil)))

(defcustom awesome-tab-auto-scroll-flag t
  "*Non-nil means to automatically scroll the tab bar.
That is, when a tab is selected outside of the tab bar visible area,
the tab bar is scrolled horizontally so the selected tab becomes
visible."
  :group 'awesome-tab
  :type 'boolean)

(defcustom awesome-tab-common-group-name "Common"
  "If the current buffer does not belong to any project,
the group name uses the name of this variable."
  :group 'awesome-tab
  :type 'string)

(defcustom awesome-tab-label-fixed-length 0
  "Fixed length of label. Set to 0 if dynamic."
  :group 'awesome-tab
  :type 'int)

(defcustom awesome-tab-label-max-length 30
  "Max length of label. Set to 0 if dynamic."
  :group 'awesome-tab
  :type 'int)

(defcustom awesometab-hide-tabs-hooks
  '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook)
  "Some buffer's header line is empty that make its window insufficient of space to display all content.
Feel free to add hook in this option. ;)"
  :type '(repeat symbol)
  :group 'awesome-tab)

(defcustom awesome-tab-height 22
  "The height of tab."
  :group 'awesome-tab
  :type 'int)

(defcustom awesome-tab-style "wave"
  "The style of tab."
  :group 'awesome-tab
  :type 'string)

(defcustom awesome-tab-display-sticky-function-name nil
  "Non-nil to display sticky function name in tab.
Sticky function is the function at the top of the current window sticky."
  :group 'awesome-tab
  :type 'boolean)

(defcustom awesome-tab-display-icon t
  "Non-nil to display icon in tab, this feature need `all-the-icons' is loaded.
Set this option with nil if you don't like icon in tab."
  :group 'awesome-tab
  :type 'boolean)

(defcustom awesome-tab-ace-keys '(?j ?k ?l ?s ?d ?f)
  "Keys used for `awesome-tab-ace-jump'."
  :group 'awesome-tab
  :set #'(lambda (symbol value)
           (set-default symbol value)
           (let ((1k-seqs nil)
                 (2k-seqs nil))
             (dolist (a value)
               (dolist (b value)
                 (push (list a b) 2k-seqs))
               (push (list a) 1k-seqs))
             (setq awesome-tab-ace-2-key-seqs (nreverse 2k-seqs))
             (setq awesome-tab-ace-1-key-seqs (nreverse 1k-seqs))))
  :type '(repeat :tag "Keys" character))

(defcustom awesome-tab-ace-quit-keys '(?\C-g ?q ?\s)
  "Keys used to quit from ace jumping."
  :group 'awesome-tab
  :type '(repeat :tag "Keys" character))

(defcustom awesome-tab-ace-str-style 'replace-icon
  "Position of ace strings."
  :group 'awesome-tab
  :type '(choice
          (const :tag "Replace icon" replace-icon)
          (const :tag "Left" left)
          (const :tag "Right" right)))

(defcustom awesome-tab-face-height 130
  "The height of tab face."
  :group 'awesome-tab
  :type 'int)

(defcustom awesome-tab-icon-v-adjust -0.1
  "The v-adjust of tab icon."
  :group 'awesome-tab
  :type 'float)

(defcustom awesome-tab-icon-height 0.9
  "The height of icon.
It will render top-line on tab when you set this variable bigger than 0.9."
  :group 'awesome-tab
  :type 'float)

(defvar-local awesome-tab-ace-state nil
  "Whether current buffer is doing `awesome-tab-ace-jump' or not.")

(defvar awesome-tab-hide-tab-function 'awesome-tab-hide-tab
  "Function to hide tab.
This fucntion accepet tab name, tab will hide if this function return ni.")

(defvar awesome-tab-current-tabset-function nil
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar.")

(defvar awesome-tab-select-tab-function nil
  "Function that select a tab.
The function is passed a tab, and should make it the
selected tab.")

(defvar awesome-tab-buffer-list-function 'awesome-tab-buffer-list
  "Function that returns the list of buffers to show in tabs.
That function is called with no arguments and must return a list of
buffers.")

(defvar awesome-tab-buffer-groups-function 'awesome-tab-buffer-groups
  "Function that gives the group names the current buffer belongs to.
It must return a list of group names, or nil if the buffer has no
group.  Notice that it is better that a buffer belongs to one group.")

(defvar awesome-tab-ace-1-key-seqs nil
  "List of 1-key sequences used by `awesome-tab-ace-jump'")

(defvar awesome-tab-ace-2-key-seqs nil
  "List of 2-key sequences used by `awesome-tab-ace-jump'")

(defvar awesome-tab-all-the-icons-is-load-p (ignore-errors (require 'all-the-icons))
  "Return non-nil if `all-the-icons' is load, `require' will have performance problem, so don't call it dynamically.")

;;; Misc.
;;
(eval-and-compile
  (defalias 'awesome-tab-display-update
    (if (fboundp 'force-window-update)
        #'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update)))

;; Copied from s.el
(defun awesome-tab-truncate-string (len s &optional ellipsis)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters
long.

When not specified, ELLIPSIS defaults to ‘...’."
  (declare (pure t) (side-effect-free t))
  (unless ellipsis
    (setq ellipsis "..."))
  (if (> (length s) len)
      (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
    (concat s (make-string (- len (length s)) ? ))))

(defun awesome-tab-refresh-display ()
  "Refresh the display of tabs. Put this in your user-defined hooks to
make sure the face colors are always right."
  (interactive)
  (awesome-tab-map-tabsets (lambda (x) (awesome-tab-set-template x nil)))
  (awesome-tab-display-update))

;;; Tab and tab set
;;
(defsubst awesome-tab-make-tab (object tabset)
  "Return a new tab with value OBJECT.
TABSET is the tab set the tab belongs to."
  (cons object tabset))

(defsubst awesome-tab-tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

(defsubst awesome-tab-tab-tabset (tab)
  "Return the tab set TAB belongs to."
  (cdr tab))

(defvar awesome-tab-tabsets nil
  "The tab sets store.")

(defvar awesome-tab-tabsets-tabset nil
  "The special tab set of existing tab sets.")

(defvar awesome-tab-current-tabset nil
  "The tab set currently displayed on the tab bar.")
(make-variable-buffer-local 'awesome-tab-current-tabset)

(defvar awesome-tab-init-hook nil
  "Hook run after tab bar data has been initialized.
You should use this hook to initialize dependent data.")

(defsubst awesome-tab-init-tabsets-store ()
  "Initialize the tab set store."
  (setq awesome-tab-tabsets (make-vector 31 0)
        awesome-tab-tabsets-tabset (make-symbol "awesome-tab-tabsets-tabset"))
  (put awesome-tab-tabsets-tabset 'start 0)
  (run-hooks 'awesome-tab-init-hook))

(defvar awesome-tab-quit-hook nil
  "Hook run after tab bar data has been freed.
You should use this hook to reset dependent data.")

(defsubst awesome-tab-free-tabsets-store ()
  "Free the tab set store."
  (setq awesome-tab-tabsets nil
        awesome-tab-tabsets-tabset nil)
  (run-hooks 'awesome-tab-quit-hook))

;; Define an "hygienic" function free of side effect between its local
;; variables and those of the callee.
(eval-and-compile
  (defalias 'awesome-tab-map-tabsets
    (let ((function (make-symbol "function"))
          (result   (make-symbol "result"))
          (tabset   (make-symbol "tabset")))
      `(lambda (,function)
         "Apply FUNCTION to each tab set, and make a list of the results.
The result is a list just as long as the number of existing tab sets."
         (let (,result)
           (mapatoms
            #'(lambda (,tabset)
                (push (funcall ,function ,tabset) ,result))
            awesome-tab-tabsets)
           ,result)))))

(defun awesome-tab-make-tabset (name &rest objects)
  "Make a new tab set whose name is the string NAME.
It is initialized with tabs build from the list of OBJECTS."
  (let* ((tabset (intern name awesome-tab-tabsets))
         (tabs (mapcar #'(lambda (object)
                           (awesome-tab-make-tab object tabset))
                       objects)))
    (set tabset tabs)
    (put tabset 'select (car tabs))
    (put tabset 'start 0)
    tabset))

(defsubst awesome-tab-get-tabset (name)
  "Return the tab set whose name is the string NAME.
Return nil if not found."
  (intern-soft name awesome-tab-tabsets))

(defsubst awesome-tab-delete-tabset (tabset)
  "Delete the tab set TABSET.
That is, remove it from the tab sets store."
  (unintern tabset awesome-tab-tabsets))

(defsubst awesome-tab-tabs (tabset)
  "Return the list of tabs in TABSET."
  (symbol-value tabset))

(defsubst awesome-tab-tab-values (tabset)
  "Return the list of tab values in TABSET."
  (mapcar 'awesome-tab-tab-value (awesome-tab-tabs tabset)))

(defsubst awesome-tab-get-tab (object tabset)
  "Search for a tab with value OBJECT in TABSET.
Return the tab found, or nil if not found."
  (assoc object (awesome-tab-tabs tabset)))

(defsubst awesome-tab-member (tab tabset)
  "Return non-nil if TAB is in TABSET."
  (or (eq (awesome-tab-tab-tabset tab) tabset)
      (memq tab (awesome-tab-tabs tabset))))

(defsubst awesome-tab-template (tabset)
  "Return the cached visual representation of TABSET.
That is, a `header-line-format' template, or nil if the cache is
empty."
  (get tabset 'template))

(defsubst awesome-tab-set-template (tabset template)
  "Set the cached visual representation of TABSET to TEMPLATE.
TEMPLATE must be a valid `header-line-format' template, or nil to
cleanup the cache."
  (put tabset 'template template))

(defsubst awesome-tab-selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst awesome-tab-selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (awesome-tab-tab-value (awesome-tab-selected-tab tabset)))

(defsubst awesome-tab-selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (awesome-tab-selected-tab tabset)))

(defvar awesome-tab--track-selected nil)

(defsubst awesome-tab-select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (awesome-tab-member tab tabset)
    (unless (awesome-tab-selected-p tab tabset)
      (awesome-tab-set-template tabset nil)
      (setq awesome-tab--track-selected awesome-tab-auto-scroll-flag))
    (put tabset 'select tab)))

(defsubst awesome-tab-select-tab-value (object tabset)
  "Make the tab with value OBJECT, the selected tab in TABSET.
Does nothing if a tab with value OBJECT is not found in TABSET.
Return the tab selected, or nil if nothing was selected."
  (awesome-tab-select-tab (awesome-tab-get-tab object tabset) tabset))

(defsubst awesome-tab-start (tabset)
  "Return the index of the first visible tab in TABSET."
  (get tabset 'start))

(defsubst awesome-tab-view (tabset)
  "Return the list of visible tabs in TABSET.
That is, the sub-list of tabs starting at the first visible one."
  (nthcdr (awesome-tab-start tabset) (awesome-tab-tabs tabset)))

(defun awesome-tab-add-tab (tabset object)
  "Return tab if it has opend.
Otherwise insert new tab on right of current tab."
  (let ((tabs (awesome-tab-tabs tabset)))
    (if (awesome-tab-get-tab object tabset)
        tabs
      (let* ((tab (awesome-tab-make-tab object tabset))
             (selected (awesome-tab-selected-tab tabset))
             (selected-index (cl-position (car selected) (mapcar 'car tabs))))
        (awesome-tab-set-template tabset nil)
        (set tabset (awesome-tab-insert-at tabs selected-index tab))
        ))))

(defun awesome-tab-insert-at (list index insert-element)
  (let ((counter 0)
        (result '()))
    (dolist (element list)
      (if (equal counter index)
          (setq result (append result (list element insert-element)))
        (setq result (append result (list element))))
      (setq counter (+ 1 counter)))
    result))

(defun awesome-tab-delete-tab (tab)
  "Remove TAB from its tab set."
  (let* ((tabset (awesome-tab-tab-tabset tab))
         (tabs   (awesome-tab-tabs tabset))
         (sel    (eq tab (awesome-tab-selected-tab tabset)))
         (next   (and sel (cdr (memq tab tabs)))))
    (awesome-tab-set-template tabset nil)
    (setq tabs (delq tab tabs))
    ;; When the selected tab is deleted, select the next one, if
    ;; available, or the last one otherwise.
    (and sel (awesome-tab-select-tab (car (or next (last tabs))) tabset))
    (set tabset tabs)))

(defun awesome-tab-scroll (tabset count)
  "Scroll the visible tabs in TABSET of COUNT units.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (awesome-tab-start tabset) count))
                    (1- (length (awesome-tab-tabs tabset))))))
    (when (/= start (awesome-tab-start tabset))
      (awesome-tab-set-template tabset nil)
      (put tabset 'start start))))

(defun awesome-tab-tab-next (tabset tab &optional before)
  "Search in TABSET for the tab after TAB.
If optional argument BEFORE is non-nil, search for the tab before
TAB.  Return the tab found, or nil otherwise."
  (let* (last (tabs (awesome-tab-tabs tabset)))
    (while (and tabs (not (eq tab (car tabs))))
      (setq last (car tabs)
            tabs (cdr tabs)))
    (and tabs (if before last (nth 1 tabs)))))

(defun awesome-tab-current-tabset (&optional update)
  "Return the tab set currently displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`awesome-tab-current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (and update awesome-tab-current-tabset-function
       (setq awesome-tab-current-tabset
             (funcall awesome-tab-current-tabset-function)))
  awesome-tab-current-tabset)

(defun awesome-tab-get-tabsets-tabset ()
  "Return the tab set of selected tabs in existing tab sets."
  (set awesome-tab-tabsets-tabset (awesome-tab-map-tabsets 'awesome-tab-selected-tab))
  (awesome-tab-scroll awesome-tab-tabsets-tabset 0)
  (awesome-tab-set-template awesome-tab-tabsets-tabset nil)
  awesome-tab-tabsets-tabset)

;;; Faces
;;

(defface awesome-tab-unselected
  '((t))
  "Face used for unselected tabs."
  :group 'awesome-tab)

(defface awesome-tab-selected
  '((t))
  "Face used for the selected tab."
  :group 'awesome-tab)

(defface awesome-tab-unselected-ace-str
  '((t))
  "Face used for ace string on unselected tabs."
  :group 'awesome-tab)

(defface awesome-tab-selected-ace-str
  '((t))
  "Face used for ace string on selected tabs."
  :group 'awesome-tab)

;;; Tabs
;;
(defun awesome-tab-make-header-line-mouse-map (mouse function)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line mouse) function)
    map))

(defun awesome-tab-color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (apply #'(lambda (r g b)
             (format "#%02x%02x%02x"
                     (ash r -8)
                     (ash g -8)
                     (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun awesome-tab-adjust-color-with-theme ()
  "We need adjust awesome-tab's colors when user switch new theme."
  (let* ((white "#FFFFFF")
         (black "#000000")
         (bg-mode (frame-parameter nil 'background-mode))
         (bg-unspecified (string= (face-background 'default) "unspecified-bg"))
         (fg-unspecified (string= (face-foreground 'default) "unspecified-fg"))
         (fg (cond
              ((and fg-unspecified (eq bg-mode 'dark)) "gray80")
              ((and fg-unspecified (eq bg-mode 'light)) "gray20")
              (t (face-foreground 'default))))
         (bg (cond
              ((and bg-unspecified (eq bg-mode 'dark)) "gray20")
              ((and bg-unspecified (eq bg-mode 'light)) "gray80")
              (t (face-background 'default))))
         ;; Ace string foreground.
         (ace-str-foreground (face-foreground 'font-lock-function-name-face))
         ;; for light themes
         (bg-dark (awesome-tab-color-blend black bg 0.1))
         (bg-more-dark (awesome-tab-color-blend black bg 0.25))
         (fg-dark (awesome-tab-color-blend fg bg-dark 0.7))
         (fg-more-dark (awesome-tab-color-blend black fg 0.3))
         (fg-ace-dark (awesome-tab-color-blend black fg 0.1))
         ;; for dark themes
         (bg-light (awesome-tab-color-blend white bg 0.1))
         (bg-more-light (awesome-tab-color-blend white bg 0.2))
         (fg-light (awesome-tab-color-blend fg bg 0.7))
         (fg-more-light (awesome-tab-color-blend white fg 0.3))
         (fg-ace-light (awesome-tab-color-blend white fg 0.1))
         )
    ;; Because tab separator is XPM object, we need re-init those XPM object after change theme.
    (awesome-tab-separator-init-vars)
    ;; Make `header-line' background same as default face.
    (set-face-attribute 'header-line nil :background bg :height (face-attribute 'default :height))
    ;; Make tab background same as default face.
    (awesome-tab-select-separator-style awesome-tab-style)
    ;; Make tab foreground change with theme.
    (cond
     ((eq bg-mode 'dark)
      (set-face-attribute 'awesome-tab-unselected nil
                          :height awesome-tab-face-height
                          :background bg-light
                          :foreground fg-dark)
      (set-face-attribute 'awesome-tab-selected nil
                          :height awesome-tab-face-height
                          :background bg-more-light
                          :foreground fg-more-light)
      (set-face-attribute 'awesome-tab-unselected-ace-str nil
                          :height awesome-tab-face-height
                          :background bg-light
                          :foreground ace-str-foreground)
      (set-face-attribute 'awesome-tab-selected-ace-str nil
                          :height awesome-tab-face-height
                          :background bg-more-light
                          :foreground ace-str-foreground))
     (t
      (set-face-attribute 'awesome-tab-unselected nil
                          :height awesome-tab-face-height
                          :background bg-dark
                          :foreground fg-light)
      (set-face-attribute 'awesome-tab-selected nil
                          :height awesome-tab-face-height
                          :background bg-more-dark
                          :foreground fg-more-dark)
      (set-face-attribute 'awesome-tab-unselected-ace-str nil
                          :height awesome-tab-face-height
                          :background bg-dark
                          :foreground ace-str-foreground)
      (set-face-attribute 'awesome-tab-selected-ace-str nil
                          :height awesome-tab-face-height
                          :background bg-more-dark
                          :foreground ace-str-foreground)))
    ))

(defun awesome-tab-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  ;; Adjust color with theme.
  (awesome-tab-adjust-color-with-theme)
  ;; Reder tab line.
  (let* ((sel (awesome-tab-selected-tab tabset))
         (tabs (awesome-tab-view tabset))
         (bg-mode (frame-parameter nil 'background-mode))
         (bg-unspecified (string= (face-background 'default) "unspecified-bg"))
         (padcolor (cond
                    ((and bg-unspecified (eq bg-mode 'dark)) "gray20")
                    ((and bg-unspecified (eq bg-mode 'light)) "gray80")
                    (t (face-background  'default))))
         atsel elts)
    ;; Track the selected tab to ensure it is always visible.
    (when awesome-tab--track-selected
      (while (not (memq sel tabs))
        (awesome-tab-scroll tabset -1)
        (setq tabs (awesome-tab-view tabset)))
      (while (and tabs (not atsel))
        (setq elts  (cons (awesome-tab-line-tab (car tabs)) elts)
              atsel (eq (car tabs) sel)
              tabs  (cdr tabs)))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (awesome-tab-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq awesome-tab--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (awesome-tab-line-tab (car tabs)) elts)
            tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (awesome-tab-set-template
     tabset
     (list (nreverse elts)
           (propertize "%-"
                       'face (list :background padcolor
                                   :foreground padcolor)
                       'pointer 'arrow)))
    ))

(defun awesome-tab-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window `awesome-tab-hide-tab-function' return nil."
  (cond
   ((awesome-tab-hide-tab-cached (current-buffer))
    ;; Don't show the tab bar.
    (setq header-line-format nil))
   ((awesome-tab-current-tabset t)
    ;; When available, use a cached tab bar value, else recompute it.
    (or (awesome-tab-template awesome-tab-current-tabset)
        (awesome-tab-line-format awesome-tab-current-tabset)))))

(defconst awesome-tab-header-line-format '(:eval (awesome-tab-line))
  "The tab bar header line format.")

;;; Cyclic navigation through tabs
;;
(defun awesome-tab-cycle (&optional backward type)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `awesome-tab-cycle-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead."
  (let* ((tabset (awesome-tab-current-tabset t))
         (ttabset (awesome-tab-get-tabsets-tabset))
         ;; If navigation through groups is requested, and there is
         ;; only one group, navigate through visible tabs.
         (cycle (if (and (eq awesome-tab-cycle-scope 'groups)
                         (not (cdr (awesome-tab-tabs ttabset))))
                    'tabs
                  awesome-tab-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (awesome-tab-selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq cycle 'tabs)
        (setq tab (awesome-tab-tab-next tabset selected backward))
        ;; When there is no tab after/before the selected one, cycle
        ;; to the first/last visible tab.
        (unless tab
          (setq tabset (awesome-tab-tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       ;; Cycle through tab groups only.
       ((eq cycle 'groups)
        (setq tab (awesome-tab-tab-next ttabset selected backward))
        ;; When there is no group after/before the selected one, cycle
        ;; to the first/last available group.
        (unless tab
          (setq tabset (awesome-tab-tabs ttabset)
                tab (car (if backward (last tabset) tabset))))
        )
       (t
        ;; Cycle through visible tabs then tab groups.
        (setq tab (awesome-tab-tab-next tabset selected backward))
        ;; When there is no visible tab after/before the selected one,
        ;; cycle to the next/previous available group.
        (unless tab
          (setq tab (awesome-tab-tab-next ttabset selected backward))
          ;; When there is no next/previous group, cycle to the
          ;; first/last available group.
          (unless tab
            (setq tabset (awesome-tab-tabs ttabset)
                  tab (car (if backward (last tabset) tabset))))
          ;; Select the first/last visible tab of the new group.
          (setq tabset (awesome-tab-tabs (awesome-tab-tab-tabset tab))
                tab (car (if backward (last tabset) tabset))))
        ))
      (awesome-tab-buffer-select-tab tab))))

;;;###autoload
(defun awesome-tab-backward ()
  "Select the previous available tab.
Depend on the setting of the option `awesome-tab-cycle-scope'."
  (interactive)
  (awesome-tab-cycle t))

;;;###autoload
(defun awesome-tab-forward ()
  "Select the next available tab.
Depend on the setting of the option `awesome-tab-cycle-scope'."
  (interactive)
  (awesome-tab-cycle))

;;;###autoload
(defun awesome-tab-backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((awesome-tab-cycle-scope 'groups))
    (awesome-tab-cycle t)))

;;;###autoload
(defun awesome-tab-forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((awesome-tab-cycle-scope 'groups))
    (awesome-tab-cycle)))

;;;###autoload
(defun awesome-tab-backward-tab ()
  "Select the previous visible tab."
  (interactive)
  (let ((awesome-tab-cycle-scope 'tabs))
    (awesome-tab-cycle t)))

;;;###autoload
(defun awesome-tab-forward-tab ()
  "Select the next visible tab."
  (interactive)
  (let ((awesome-tab-cycle-scope 'tabs))
    (awesome-tab-cycle)))

;;; Minor modes
;;
(defsubst awesome-tab-mode-on-p ()
  "Return non-nil if Awesome-Tab mode is on."
  (eq (default-value 'header-line-format)
      awesome-tab-header-line-format))

;;; Awesome-Tab mode
;;
(defvar awesome-tab-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap to use in Awesome-Tab mode.")

(defvar awesome-tab--global-hlf nil)

;;;###autoload
(define-minor-mode awesome-tab-mode
  "Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{awesome-tab-mode-map}"
  :group 'awesome-tab
  :require 'awesome-tab
  :global t
  :keymap awesome-tab-mode-map
  (if awesome-tab-mode
;;; ON
      (unless (awesome-tab-mode-on-p)
        ;; Save current default value of `header-line-format'.
        (setq awesome-tab--global-hlf (default-value 'header-line-format))
        (awesome-tab-init-tabsets-store)
        (setq-default header-line-format awesome-tab-header-line-format))
;;; OFF
    (when (awesome-tab-mode-on-p)
      ;; Restore previous `header-line-format'.
      (setq-default header-line-format awesome-tab--global-hlf)
      (awesome-tab-free-tabsets-store))
    ))

;;; Buffer tabs
;;
(defgroup awesome-tab-buffer nil
  "Display buffers in the tab bar."
  :group 'awesome-tab)

(defun awesome-tab-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun awesome-tab-filter-out (condp lst)
  (delq nil
        (mapcar (lambda (x) (if (funcall condp x) nil x)) lst)))

(defun awesome-tab-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
  (awesome-tab-filter-out
   'awesome-tab-hide-tab-cached
   (delq nil
         (mapcar #'(lambda (b)
                     (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((buffer-file-name b) b)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      ((buffer-live-p b) b)))
                 (buffer-list)))))

(defun awesome-tab-buffer-mode-derived-p (mode parents)
  "Return non-nil if MODE derives from a mode in PARENTS."
  (let (derived)
    (while (and (not derived) mode)
      (if (memq mode parents)
          (setq derived t)
        (setq mode (get mode 'derived-mode-parent))))
    derived))

;;; Group buffers in tab sets.
;;
(defvar awesome-tab--buffers nil)

(defun awesome-tab-buffer-update-groups ()
  "Update tab sets from groups of existing buffers.
Return the the first group where the current buffer is."
  (let ((bl (sort
             (mapcar
              #'(lambda (b)
                  (with-current-buffer b
                    (list (current-buffer)
                          (buffer-name)
                          (if awesome-tab-buffer-groups-function
                              (funcall awesome-tab-buffer-groups-function)
                            '(awesome-tab-common-group-name)))))
              (and awesome-tab-buffer-list-function
                   (funcall awesome-tab-buffer-list-function)))
             #'(lambda (e1 e2)
                 (string-lessp (nth 1 e1) (nth 1 e2))))))
    ;; If the cache has changed, update the tab sets.
    (unless (equal bl awesome-tab--buffers)
      ;; Add new buffers, or update changed ones.
      (dolist (e bl)
        (dolist (g (nth 2 e))
          (let ((tabset (awesome-tab-get-tabset g)))
            (if tabset
                (unless (equal e (assq (car e) awesome-tab--buffers))
                  ;; This is a new buffer, or a previously existing
                  ;; buffer that has been renamed, or moved to another
                  ;; group.  Update the tab set, and the display.
                  (awesome-tab-add-tab tabset (car e))
                  (awesome-tab-set-template tabset nil))
              (awesome-tab-make-tabset g (car e))))))
      ;; Remove tabs for buffers not found in cache or moved to other
      ;; groups, and remove empty tabsets.
      (mapc 'awesome-tab-delete-tabset
            (awesome-tab-map-tabsets
             #'(lambda (tabset)
                 (dolist (tab (awesome-tab-tabs tabset))
                   (let ((e (assq (awesome-tab-tab-value tab) bl)))
                     (or (and e (memq tabset
                                      (mapcar 'awesome-tab-get-tabset
                                              (nth 2 e))))
                         (awesome-tab-delete-tab tab))))
                 ;; Return empty tab sets
                 (unless (awesome-tab-tabs tabset)
                   tabset))))
      ;; The new cache becomes the current one.
      (setq awesome-tab--buffers bl)))
  ;; Return the first group the current buffer belongs to.
  (car (nth 2 (assq (current-buffer) awesome-tab--buffers))))

;;; Tab bar callbacks
;;
(defvar awesome-tab--buffer-show-groups nil)

(defsubst awesome-tab-buffer-show-groups (flag)
  "Set display of tabs for groups of buffers to FLAG."
  (setq awesome-tab--buffer-show-groups flag))

(defun awesome-tab-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((tabset (awesome-tab-get-tabset (awesome-tab-buffer-update-groups))))
    (awesome-tab-select-tab-value (current-buffer) tabset)
    (when awesome-tab--buffer-show-groups
      (setq tabset (awesome-tab-get-tabsets-tabset))
      (awesome-tab-select-tab-value (current-buffer) tabset))
    tabset))

;;; Separator
;;
(defvar awesome-tab-image-apple-rgb
  (and (eq (window-system) 'ns)
       ns-use-srgb-colorspace
       (< 11
          (string-to-number
           (and (string-match "darwin\\([0-9]+\\)" system-configuration)
                (match-string-no-properties 1 system-configuration)))))
  "Boolean variable to determine whether to use Apple RGB colorspace to render images.

t on macOS 10.7+ and `ns-use-srgb-colorspace' is t, nil otherwise.

This variable is automatically set, there's no need to modify it.")

(defun awesome-tab-separator-interpolate (color1 color2)
  "Interpolate between COLOR1 and COLOR2.

COLOR1 and COLOR2 must be supplied as hex strings with a leading #."
  (let* ((c1 (color-name-to-rgb color1))
         (c2 (color-name-to-rgb color2))
         (red (/ (+ (nth 0 c1) (nth 0 c2)) 2))
         (green (/ (+ (nth 1 c1) (nth 1 c2)) 2))
         (blue (/ (+ (nth 2 c1) (nth 2 c2)) 2)))
    (color-rgb-to-hex red green blue)))

(defun awesome-tab-separator-color-xyz-to-apple-rgb (X Y Z)
  "Convert CIE X Y Z colors to Apple RGB color space."
  (let ((r (+ (* 3.2404542 X) (* -1.5371385 Y) (* -0.4985314 Z)))
        (g (+ (* -0.9692660 X) (* 1.8760108 Y) (* 0.0415560 Z)))
        (b (+ (* 0.0556434 X) (* -0.2040259 Y) (* 1.0572252 Z))))
    (list (expt r (/ 1.8)) (expt g (/ 1.8)) (expt b (/ 1.8)))))

(defun awesome-tab-separator-color-srgb-to-apple-rgb (red green blue)
  "Convert RED GREEN BLUE colors from sRGB color space to Apple RGB.
RED, GREEN and BLUE should be between 0.0 and 1.0, inclusive."
  (apply 'awesome-tab-separator-color-xyz-to-apple-rgb (color-srgb-to-xyz red green blue)))

(defun awesome-tab-separator-hex-color (color)
  "Get the hexadecimal value of COLOR."
  (when color
    (let ((srgb-color (color-name-to-rgb color)))
      (if awesome-tab-image-apple-rgb
          (apply 'color-rgb-to-hex (apply 'awesome-tab-separator-color-srgb-to-apple-rgb srgb-color))
        (apply 'color-rgb-to-hex srgb-color)))))

(defun awesome-tab-separator-pattern (lst)
  "Turn LST into an infinite pattern."
  (when lst
    (let ((pattern (cl-copy-list lst)))
      (setcdr (last pattern) pattern))))

(defun awesome-tab-separator-pattern-to-string (pattern)
  "Convert a PATTERN into a string that can be used in an XPM."
  (concat "\"" (mapconcat 'number-to-string pattern "") "\","))

(defun awesome-tab-separator-reverse-pattern (pattern)
  "Reverse each line in PATTERN."
  (mapcar 'reverse pattern))

(defun awesome-tab-separator-row-pattern (fill total &optional fade)
  "Make a list that has FILL 0s out of TOTAL 1s with FADE 2s to the right of the fill."
  (unless fade
    (setq fade 0))
  (let ((fill (min fill total))
        (fade (min fade (max (- total fill) 0))))
    (append (make-list fill 0)
            (make-list fade 2)
            (make-list (- total fill fade) 1))))

(defun awesome-tab-separator-pattern-bindings-body (patterns height-exp pattern-height-sym
                                                             second-pattern-height-sym)
  "Create let-var bindings and a function body from PATTERNS.
The `car' and `cdr' parts of the result can be passed to the
function `awesome-tab-separator-wrap-defun' as its `let-vars' and `body' arguments,
respectively.  HEIGHT-EXP is an expression calculating the image
height and it should contain a free variable `height'.
PATTERN-HEIGHT-SYM and SECOND-PATTERN-HEIGHT-SYM are symbols used
for let-var binding variables."
  (let* ((pattern (awesome-tab-separator-pattern (mapcar 'awesome-tab-separator-pattern-to-string (car patterns))))
         (header (mapcar 'awesome-tab-separator-pattern-to-string (nth 1 patterns)))
         (footer (mapcar 'awesome-tab-separator-pattern-to-string (nth 2 patterns)))
         (second-pattern (awesome-tab-separator-pattern (mapcar 'awesome-tab-separator-pattern-to-string (nth 3 patterns))))
         (center (mapcar 'awesome-tab-separator-pattern-to-string (nth 4 patterns)))
         (reserve (+ (length header) (length footer) (length center))))
    (when pattern
      (cons `((,pattern-height-sym (max (- ,height-exp ,reserve) 0))
              (,second-pattern-height-sym (/ ,pattern-height-sym 2))
              (,pattern-height-sym ,(if second-pattern `(ceiling ,pattern-height-sym 2) `,pattern-height-sym)))
            (list (when header `(mapconcat 'identity ',header ""))
                  `(mapconcat 'identity
                              (cl-subseq ',pattern 0 ,pattern-height-sym) "")
                  (when center `(mapconcat 'identity ',center ""))
                  (when second-pattern
                    `(mapconcat 'identity
                                (cl-subseq ',second-pattern
                                           0 ,second-pattern-height-sym) ""))
                  (when footer `(mapconcat 'identity ',footer "")))))))

(defun awesome-tab-separator-pattern-defun (name dir width &rest patterns)
  "Create a powerline function of NAME in DIR with WIDTH for PATTERNS.

PATTERNS is of the form (PATTERN HEADER FOOTER SECOND-PATTERN CENTER
PATTERN-2X HEADER-2X FOOTER-2X SECOND-PATTERN-2X CENTER-2X).
PATTERN is required, all other components are optional.
The first 5 components are for the standard resolution image.
The remaining ones are for the high resolution image where both
width and height are doubled.  If PATTERN-2X is nil or not given,
then the remaining components are ignored and the standard
resolution image with magnification and interpolation will be
used in high resolution environments

All generated functions generate the form:
HEADER
PATTERN ...
CENTER
SECOND-PATTERN ...
FOOTER

PATTERN and SECOND-PATTERN repeat infinitely to fill the space needed to generate a full height XPM.

PATTERN, HEADER, FOOTER, SECOND-PATTERN, CENTER are of the form ((COLOR ...) (COLOR ...) ...).

COLOR can be one of 0, 1, or 2, where 0 is the source color, 1 is the
destination color, and 2 is the interpolated color between 0 and 1."
  (when (eq dir 'right)
    (setq patterns (mapcar 'awesome-tab-separator-reverse-pattern patterns)))
  (let ((bindings-body (awesome-tab-separator-pattern-bindings-body patterns
                                                                    'height
                                                                    'pattern-height
                                                                    'second-pattern-height))
        (bindings-body-2x (awesome-tab-separator-pattern-bindings-body (nthcdr 5 patterns)
                                                                       '(* height 2)
                                                                       'pattern-height-2x
                                                                       'second-pattern-height-2x)))
    (awesome-tab-separator-wrap-defun name dir width
                                      (append (car bindings-body) (car bindings-body-2x))
                                      (cdr bindings-body) (cdr bindings-body-2x))))

(defun awesome-tab-separator-background-color (face)
  (face-attribute face
                  (if (face-attribute face :inverse-video nil 'default)
                      :foreground
                    :background)
                  nil
                  'default))

(defun awesome-tab-separator-wrap-defun (name dir width let-vars body &optional body-2x)
  "Generate a powerline function of NAME in DIR with WIDTH using LET-VARS and BODY."
  (let* ((src-face (if (eq dir 'left) 'face1 'face2))
         (dst-face (if (eq dir 'left) 'face2 'face1)))
    `(defun ,(intern (format "powerline-%s-%s" name (symbol-name dir)))
         (face1 face2 &optional height)
       (when window-system
         (unless height (setq height awesome-tab-height))
         (let* ,(append `((color1 (when ,src-face
                                    (awesome-tab-separator-hex-color (awesome-tab-separator-background-color ,src-face))))
                          (color2 (when ,dst-face
                                    (awesome-tab-separator-hex-color (awesome-tab-separator-background-color ,dst-face))))
                          (colori (when (and color1 color2) (awesome-tab-separator-interpolate color1 color2)))
                          (color1 (or color1 "None"))
                          (color2 (or color2 "None"))
                          (colori (or colori "None")))
                        let-vars)
           (apply 'create-image
                  ,(append `(concat (format "/* XPM */ static char * %s_%s[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
                                            ,(replace-regexp-in-string "-" "_" name)
                                            (symbol-name ',dir)
                                            ,width
                                            height
                                            color1
                                            color2
                                            colori))
                           body
                           '("};"))
                  'xpm t
                  :ascent 'center
                  :face (when (and face1 face2)
                          ,dst-face)
                  ,(and body-2x
                        `(and (featurep 'mac)
                              (list :data-2x
                                    ,(append `(concat (format "/* XPM */ static char * %s_%s_2x[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
                                                              ,(replace-regexp-in-string "-" "_" name)
                                                              (symbol-name ',dir)
                                                              (* ,width 2)
                                                              (* height 2)
                                                              color1
                                                              color2
                                                              colori))
                                             body-2x
                                             '("};")))))))))))

(defmacro awesome-tab-separator-alternate (dir)
  "Generate an alternating pattern XPM function for DIR."
  (awesome-tab-separator-pattern-defun "alternate" dir 4
                                       '((2 2 1 1)
                                         (0 0 2 2))
                                       nil nil nil nil
                                       ;; 2x
                                       '((2 2 2 2 1 1 1 1)
                                         (2 2 2 2 1 1 1 1)
                                         (0 0 0 0 2 2 2 2)
                                         (0 0 0 0 2 2 2 2))))

(defmacro awesome-tab-separator-bar (dir)
  "Generate a bar XPM function for DIR."
  (awesome-tab-separator-pattern-defun "bar" dir 2
                                       '((2 2))))

(defmacro awesome-tab-separator-box (dir)
  "Generate a box XPM function for DIR."
  (awesome-tab-separator-pattern-defun "box" dir 2
                                       '((0 0)
                                         (0 0)
                                         (1 1)
                                         (1 1))
                                       nil nil nil nil
                                       ;; 2x
                                       '((0 0 0 0)
                                         (0 0 0 0)
                                         (0 0 0 0)
                                         (0 0 0 0)
                                         (1 1 1 1)
                                         (1 1 1 1)
                                         (1 1 1 1)
                                         (1 1 1 1))))

(defmacro awesome-tab-separator-chamfer (dir)
  "Generate a chamfer XPM function for DIR."
  (awesome-tab-separator-pattern-defun "chamfer" dir 3
                                       '((0 0 0))
                                       '((1 1 1)
                                         (0 1 1)
                                         (0 0 1))
                                       nil nil nil
                                       ;; 2x
                                       '((0 0 0 0 0 0))
                                       '((1 1 1 1 1 1)
                                         (0 1 1 1 1 1)
                                         (0 0 1 1 1 1)
                                         (0 0 0 1 1 1)
                                         (0 0 0 0 1 1)
                                         (0 0 0 0 0 1))))

(defmacro awesome-tab-separator-rounded (dir)
  "Generate a rounded XPM function for DIR."
  (awesome-tab-separator-pattern-defun "rounded" dir 6
                                       '((0 0 0 0 0 0))
                                       '((2 1 1 1 1 1)
                                         (0 0 2 1 1 1)
                                         (0 0 0 0 1 1)
                                         (0 0 0 0 2 1)
                                         (0 0 0 0 0 1)
                                         (0 0 0 0 0 2))
                                       nil nil nil
                                       ;; 2x
                                       '((0 0 0 0 0 0 0 0 0 0 0 0))
                                       '((1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 2 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 2 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 2 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 1))))

(defmacro awesome-tab-separator-slant (dir)
  "Generate a slant XPM function for DIR."
  (let* ((row-modifier (if (eq dir 'left) 'identity 'reverse)))
    (awesome-tab-separator-wrap-defun "slant" dir 'width
                                      '((width (1- (ceiling height 2))))
                                      `((cl-loop for i from 0 to (1- height)
                                                 concat (awesome-tab-separator-pattern-to-string (,row-modifier (awesome-tab-separator-row-pattern (/ i 2) width)))))
                                      `((cl-loop for i from 0 to (1- (* height 2))
                                                 concat (awesome-tab-separator-pattern-to-string (,row-modifier (awesome-tab-separator-row-pattern (/ i 2) (* width 2)))))))))

(defmacro awesome-tab-separator-wave (dir)
  "Generate a wave XPM function for DIR."
  (awesome-tab-separator-pattern-defun "wave" dir 11
                                       '((0 0 0 0 0 0 1 1 1 1 1))
                                       '((2 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 1 1 1 1 1 1 1 1)
                                         (0 0 0 2 1 1 1 1 1 1 1)
                                         (0 0 0 0 1 1 1 1 1 1 1)
                                         (0 0 0 0 2 1 1 1 1 1 1)
                                         (0 0 0 0 0 1 1 1 1 1 1)
                                         (0 0 0 0 0 1 1 1 1 1 1)
                                         (0 0 0 0 0 2 1 1 1 1 1))
                                       '((0 0 0 0 0 0 2 1 1 1 1)
                                         (0 0 0 0 0 0 0 1 1 1 1)
                                         (0 0 0 0 0 0 0 1 1 1 1)
                                         (0 0 0 0 0 0 0 2 1 1 1)
                                         (0 0 0 0 0 0 0 0 1 1 1)
                                         (0 0 0 0 0 0 0 0 2 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 2))
                                       nil nil
                                       ;; 2x
                                       '((0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1))
                                       '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1))
                                       '((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                                         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defmacro awesome-tab-separator-zigzag (dir)
  "Generate a zigzag pattern XPM function for DIR."
  (awesome-tab-separator-pattern-defun "zigzag" dir 3
                                       '((1 1 1)
                                         (0 1 1)
                                         (0 0 1)
                                         (0 0 0)
                                         (0 0 1)
                                         (0 1 1))
                                       nil nil nil nil
                                       ;; 2x
                                       '((1 1 1 1 1 1)
                                         (0 1 1 1 1 1)
                                         (0 0 1 1 1 1)
                                         (0 0 0 1 1 1)
                                         (0 0 0 0 1 1)
                                         (0 0 0 0 0 1)
                                         (0 0 0 0 0 0)
                                         (0 0 0 0 0 1)
                                         (0 0 0 0 1 1)
                                         (0 0 0 1 1 1)
                                         (0 0 1 1 1 1)
                                         (0 1 1 1 1 1))))

(defun awesome-tab-separator-memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the memoized function over
the original function.  Use frame-local memoization."
  (cl-typecase func
    (symbol (fset func (awesome-tab-separator-memoize-wrap-frame-local (symbol-function func))) func)
    (function (awesome-tab-separator-memoize-wrap-frame-local func))))

(defun awesome-tab-separator-memoize-wrap-frame-local (func)
  "Return the memoized version of FUNC.
The memoization cache is frame-local."
  (let ((funcid (cl-gensym)))
    `(lambda (&rest args)
       ,(concat (documentation func) (format "\n(memoized function %s)" funcid))
       (let* ((cache (awesome-tab-separator-create-or-get-cache))
              (key (cons ',funcid args))
              (val (gethash key cache)))
         (if val
             val
           (puthash key (apply ,func args) cache))))))

(defun awesome-tab-separator-create-or-get-cache ()
  "Return a frame-local hash table that acts as a memoization cache for powerline. Create one if the frame doesn't have one yet."
  (let ((table (frame-parameter nil 'powerline-cache)))
    (if (hash-table-p table) table (awesome-tab-separator-reset-cache))))

(defun awesome-tab-separator-reset-cache ()
  "Reset and return the frame-local hash table used for a memoization cache."
  (let ((table (make-hash-table :test 'equal)))
    ;; Store it as a frame-local variable
    (modify-frame-parameters nil `((powerline-cache . ,table)))
    table))

(defun awesome-tab-separator-init-vars ()
  (awesome-tab-separator-memoize (awesome-tab-separator-alternate left))
  (awesome-tab-separator-memoize (awesome-tab-separator-alternate right))
  (awesome-tab-separator-memoize (awesome-tab-separator-bar left))
  (awesome-tab-separator-memoize (awesome-tab-separator-bar right))
  (awesome-tab-separator-memoize (awesome-tab-separator-box left))
  (awesome-tab-separator-memoize (awesome-tab-separator-box right))
  (awesome-tab-separator-memoize (awesome-tab-separator-chamfer left))
  (awesome-tab-separator-memoize (awesome-tab-separator-chamfer right))
  (awesome-tab-separator-memoize (awesome-tab-separator-rounded left))
  (awesome-tab-separator-memoize (awesome-tab-separator-rounded right))
  (awesome-tab-separator-memoize (awesome-tab-separator-slant left))
  (awesome-tab-separator-memoize (awesome-tab-separator-slant right))
  (awesome-tab-separator-memoize (awesome-tab-separator-wave left))
  (awesome-tab-separator-memoize (awesome-tab-separator-wave right))
  (awesome-tab-separator-memoize (awesome-tab-separator-zigzag left))
  (awesome-tab-separator-memoize (awesome-tab-separator-zigzag right))
  )

(awesome-tab-separator-init-vars)

(defvar awesome-tab-style-left nil)
(defvar awesome-tab-style-right nil)

(defun awesome-tab-select-separator-style (tab-style)
  (setq awesome-tab-style-left (funcall (intern (format "powerline-%s-right" tab-style)) 'default nil awesome-tab-height))
  (setq awesome-tab-style-right (funcall (intern (format "powerline-%s-left" tab-style)) nil 'default awesome-tab-height)))

(defsubst awesome-tab-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element."
  (propertize
   (awesome-tab-buffer-tab-label tab)
   'pointer 'hand
   'local-map (purecopy (awesome-tab-make-header-line-mouse-map
                         'mouse-1
                         `(lambda (event) (interactive "e")
                            (let ((tab-window (window-at (cadr (mouse-position))
                                                         (cddr (mouse-position))
                                                         (car (mouse-position)))))
                              (when tab-window
                                (select-window tab-window)
                                (awesome-tab-buffer-select-tab ',tab))))))))

(defun awesome-tab-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let* ((is-active-tab (awesome-tab-selected-p tab (awesome-tab-current-tabset)))
         (tab-face (if is-active-tab 'awesome-tab-selected 'awesome-tab-unselected))
         (ace-str-face (if is-active-tab 'awesome-tab-selected-ace-str
                         'awesome-tab-unselected-ace-str))
         (current-buffer-index
          (cl-position tab (awesome-tab-view awesome-tab-current-tabset)))
         (ace-str (if awesome-tab-ace-state
                      (elt ace-strs current-buffer-index) ""))
         (ace-state awesome-tab-ace-state))
    (concat
     ;; Tab left edge.
     (awesome-tab-separator-render awesome-tab-style-left tab-face)
     ;; Ace string.
     (when (and ace-state (eq awesome-tab-ace-str-style 'left))
       (propertize ace-str 'face ace-str-face))
     ;; Tab icon.
     (if (and ace-state (eq awesome-tab-ace-str-style 'replace-icon))
         (propertize ace-str 'face ace-str-face)
       (awesome-tab-icon-for-tab tab tab-face))
     ;; Tab label.
     (propertize (awesome-tab-tab-name tab) 'face tab-face)
     ;; Ace string.
     (when (and ace-state (eq awesome-tab-ace-str-style 'right))
       (propertize ace-str 'face ace-str-face))
     ;; Tab right edge.
     (awesome-tab-separator-render awesome-tab-style-right tab-face)
     )))

(defun awesome-tab-tab-name (tab)
  "Render tab's name.
Tab name will truncate if option `awesome-tab-truncate-string' big than zero."
  (format " %s "
          (let ((bufname (awesome-tab-buffer-name (car tab))))
            (cond ((> awesome-tab-label-fixed-length 0)
                   (awesome-tab-truncate-string  awesome-tab-label-fixed-length bufname))
                  ((> awesome-tab-label-max-length 0)
                   (let ((ellipsis "..."))
                     (if (> (length bufname) awesome-tab-label-max-length)
                         (format "%s%s" (substring bufname 0 (- awesome-tab-label-max-length (length ellipsis))) ellipsis)
                       bufname)))
                  (t
                   bufname))
            )))

(defun awesome-tab-icon-for-tab (tab face)
  "When tab buffer's file is exists, use `all-the-icons-icon-for-file' to fetch file icon.
Otherwise use `all-the-icons-icon-for-buffer' to fetch icon for buffer."
  (when (and awesome-tab-display-icon
             awesome-tab-all-the-icons-is-load-p)
    (let* ((tab-buffer (car tab))
           (tab-file (buffer-file-name tab-buffer))
           (background (face-background face))
           (icon
            (cond
             ;; Use `all-the-icons-icon-for-file' if current file is exists.
             ((and
               tab-file
               (file-exists-p tab-file))
              (all-the-icons-icon-for-file tab-file :v-adjust awesome-tab-icon-v-adjust :height awesome-tab-icon-height))
             ;; Use `all-the-icons-icon-for-mode' for current tab buffer at last.
             (t
              (with-current-buffer tab-buffer
                (all-the-icons-icon-for-mode major-mode :v-adjust awesome-tab-icon-v-adjust :height awesome-tab-icon-height)
                )))))
      (when (and icon
                 ;; `get-text-property' need icon is string type.
                 (stringp icon))
        ;; Thanks ema2159 for code block ;)
        (propertize
         icon
         'face `(:inherit ,(get-text-property 0 'face icon)
                          :background ,background
                          ))))))

(defun awesome-tab-buffer-name (tab-buffer)
  "Get buffer name of tab.
Will merge sticky function name in tab if option `awesome-tab-display-sticky-function-name' is non-nil."
  (if (and awesome-tab-display-sticky-function-name
           (boundp 'awesome-tab-last-sticky-func-name)
           awesome-tab-last-sticky-func-name
           (equal tab-buffer (current-buffer)))
      (format "%s [%s]" (buffer-name tab-buffer) awesome-tab-last-sticky-func-name)
    (buffer-name tab-buffer)))

(defvar awesome-tab-last-scroll-y 0
  "Holds the scroll y of window from the last run of post-command-hooks.")

(defun awesome-tab-monitor-window-scroll ()
  "This function is used to monitor the window scroll.
Currently, this function is only use for option `awesome-tab-display-sticky-function-name'."
  (when awesome-tab-display-sticky-function-name
    (let ((scroll-y (window-start)))
      (when (and scroll-y
                 (integerp scroll-y))
        (unless (equal scroll-y awesome-tab-last-scroll-y)
          (let ((func-name (save-excursion
                             (goto-char scroll-y)
                             (which-function))))
            (when (or
                   (not (boundp 'awesome-tab-last-sticky-func-name))
                   (not (equal func-name awesome-tab-last-sticky-func-name)))
              (set (make-local-variable 'awesome-tab-last-sticky-func-name) func-name)

              ;; Use `ignore-errors' avoid integerp error when execute `awesome-tab-line-format'.
              (ignore-errors
                (awesome-tab-line-format awesome-tab-current-tabset))
              ))))
      (setq awesome-tab-last-scroll-y scroll-y))))

(add-hook 'post-command-hook 'awesome-tab-monitor-window-scroll)

(defun awesome-tab-separator-render (item face)
  "Render ITEM using FACE."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item 'face face))
   (t item)))

(defun awesome-tab-buffer-select-tab (tab)
  "Select tab."
  (let ((buffer (awesome-tab-tab-value tab)))
    (switch-to-buffer buffer)
    (awesome-tab-buffer-show-groups nil)
    (awesome-tab-display-update)
    ))

(defun awesome-tab-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Awesome-Tab mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and (eq header-line-format awesome-tab-header-line-format)
       (eq awesome-tab-current-tabset-function 'awesome-tab-buffer-tabs)
       (eq (current-buffer) (window-buffer (selected-window)))
       (let ((bl (awesome-tab-tab-values (awesome-tab-current-tabset)))
             (b  (current-buffer))
             found sibling)
         (while (and bl (not found))
           (if (eq b (car bl))
               (setq found t)
             (setq sibling (car bl)))
           (setq bl (cdr bl)))
         (when (and (setq sibling (or (car bl) sibling))
                    (buffer-live-p sibling))
           ;; Move sibling buffer in front of the buffer list.
           (save-current-buffer
             (switch-to-buffer sibling))))))

;;; Tab bar buffer setup
;;
(defun awesome-tab-buffer-init ()
  "Initialize tab bar buffer data.
Run as `awesome-tab-init-hook'."
  (setq awesome-tab--buffers nil
        awesome-tab--buffer-show-groups nil
        awesome-tab-current-tabset-function 'awesome-tab-buffer-tabs
        awesome-tab-select-tab-function 'awesome-tab-buffer-select-tab
        )
  (add-hook 'kill-buffer-hook 'awesome-tab-buffer-track-killed))

(defun awesome-tab-buffer-quit ()
  "Quit tab bar buffer.
Run as `awesome-tab-quit-hook'."
  (setq awesome-tab--buffers nil
        awesome-tab--buffer-show-groups nil
        awesome-tab-current-tabset-function nil
        awesome-tab-select-tab-function nil
        )
  (remove-hook 'kill-buffer-hook 'awesome-tab-buffer-track-killed))

(add-hook 'awesome-tab-init-hook 'awesome-tab-buffer-init)
(add-hook 'awesome-tab-quit-hook 'awesome-tab-buffer-quit)

;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun awesome-tab-switch-group (&optional groupname)
  "Switch tab groups using ido."
  (interactive)
  (let* ((tab-buffer-list (mapcar
                           #'(lambda (b)
                               (with-current-buffer b
                                 (list (current-buffer)
                                       (buffer-name)
                                       (funcall awesome-tab-buffer-groups-function) )))
                           (funcall awesome-tab-buffer-list-function)))
         (groups (awesome-tab-get-groups))
         (group-name (or groupname (ido-completing-read "Groups: " groups))) )
    (catch 'done
      (mapc
       #'(lambda (group)
           (when (equal group-name (car (car (cdr (cdr group)))))
             (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list) )))

(defun awesome-tab-select-end-tab ()
  "Select end tab of current tabset."
  (interactive)
  (awesome-tab-select-beg-tab t))

(defun awesome-tab-select-beg-tab (&optional backward type)
  "Select beginning tab of current tabs.
If BACKWARD is non-nil, move backward, otherwise move forward.
TYPE is default option."
  (interactive)
  (let* ((tabset (awesome-tab-current-tabset t))
         (ttabset (awesome-tab-get-tabsets-tabset))
         (cycle (if (and (eq awesome-tab-cycle-scope 'groups)
                         (not (cdr (awesome-tab-tabs ttabset))))
                    'tabs
                  awesome-tab-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (awesome-tab-selected-tab tabset))
      (setq tabset (awesome-tab-tabs tabset)
            tab (car (if backward (last tabset) tabset)))
      (awesome-tab-buffer-select-tab tab))))

(defun awesome-tab-backward-tab-other-window (&optional reversed)
  "Move to left tab in other window.
Optional argument REVERSED default is move backward, if reversed is non-nil move forward."
  (interactive)
  (other-window 1)
  (if reversed
      (awesome-tab-forward-tab)
    (awesome-tab-backward-tab))
  (other-window -1))

(defun awesome-tab-forward-tab-other-window ()
  "Move to right tab in other window."
  (interactive)
  (awesome-tab-backward-tab-other-window t))

(defun awesome-tab-move-current-tab-to-right ()
  "Move current tab one place right, unless it's already the rightmost."
  (interactive)
  (let* ((bufset (awesome-tab-current-tabset t))
         (old-bufs (awesome-tab-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (while (and
            old-bufs
            (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
      (push (car old-bufs) new-bufs)
      (setq old-bufs (cdr old-bufs)))
    (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
        (progn
          (setq the-buffer (car old-bufs))
          (setq old-bufs (cdr old-bufs))
          (if old-bufs ; if this is false, then the current tab is the rightmost
              (push (car old-bufs) new-bufs))
          (push the-buffer new-bufs)) ; this is the tab that was to be moved
      (error "Error: current buffer's name was not found in Awesome-Tab's buffer list."))
    (setq new-bufs (reverse new-bufs))
    (setq new-bufs (append new-bufs (cdr old-bufs)))
    (set bufset new-bufs)
    (awesome-tab-set-template bufset nil)
    (awesome-tab-display-update)))

(defun awesome-tab-move-current-tab-to-left ()
  "Move current tab one place left, unless it's already the leftmost."
  (interactive)
  (let* ((bufset (awesome-tab-current-tabset t))
         (old-bufs (awesome-tab-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (if (string= (buffer-name) (format "%s" (car first-buf)))
        old-bufs                     ; the current tab is the leftmost
      (setq not-yet-this-buf first-buf)
      (setq old-bufs (cdr old-bufs))
      (while (and
              old-bufs
              (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
        (push not-yet-this-buf new-bufs)
        (setq not-yet-this-buf (car old-bufs))
        (setq old-bufs (cdr old-bufs)))
      (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
          (progn
            (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
            (push not-yet-this-buf new-bufs)
            (setq new-bufs (reverse new-bufs))
            (setq new-bufs (append new-bufs (cdr old-bufs))))
        (error "Error: current buffer's name was not found in Awesome-Tab's buffer list."))
      (set bufset new-bufs)
      (awesome-tab-set-template bufset nil)
      (awesome-tab-display-update))))

(defun awesome-tab-move-current-tab-to-beg ()
  "Move current tab to the first position."
  (interactive)
  (let* ((bufset (awesome-tab-current-tabset t))
         (bufs (copy-sequence (awesome-tab-tabs bufset)))
         (current-tab-index
          (cl-position (current-buffer) (mapcar #'car bufs)))
         (current-tab (elt bufs current-tab-index)))
    (setq bufs (delete current-tab bufs))
    (push current-tab bufs)
    (set bufset bufs)
    (awesome-tab-set-template bufset nil)
    (awesome-tab-display-update)))

(defun awesome-tab-kill-all-buffers-in-current-group ()
  "Kill all buffers in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t)))))
    ;; Kill all buffers in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer) t))
    ;; Switch to next group.
    (awesome-tab-forward-group)
    ))

(defun awesome-tab-kill-other-buffers-in-current-group ()
  "Kill all buffers except current buffer in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
         (currentbuffer (current-buffer)))
    ;; Kill all buffers in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer) (not (equal buffer currentbuffer))))
    ))

(defun awesome-tab-kill-match-buffers-in-current-group ()
  "Kill all buffers match extension in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
         (extension-names (awesome-tab-get-extensions))
         match-extension)
    ;; Read extension need to kill.
    (setq match-extension (ido-completing-read "Kill buffers suffix with: " extension-names))
    ;; Kill all buffers match extension in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer)
       (let ((filename (buffer-file-name buffer)))
         (and filename (string-equal (file-name-extension filename) match-extension))
         )))
    ;; Switch to next group if last file killed.
    (when (equal (length extension-names) 1)
      (awesome-tab-forward-group))
    ))

(defun awesome-tab-keep-match-buffers-in-current-group ()
  "Keep all buffers match extension in current group."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
         (extension-names (awesome-tab-get-extensions))
         match-extension)
    ;; Read extension need to kill.
    (setq match-extension (ido-completing-read "Just keep buffers suffix with: " extension-names))
    ;; Kill all buffers match extension in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer)
       (let ((filename (buffer-file-name buffer)))
         (and filename (not (string-equal (file-name-extension filename) match-extension)))
         )))
    ;; Switch to next group if last file killed.
    (when (equal (length extension-names) 1)
      (awesome-tab-forward-group))))

(defun awesome-tab-select-visible-nth-tab (tab-index)
  "Select visible tab with `tab-index'.
Example, when `tab-index' is 1, this function will select the leftmost label in the visible area,
instead of the first label in the current group.

If `tab-index' more than length of visible tabs, selet the last tab.

If `tab-index' is 0, select last tab."
  (let ((visible-tabs (awesome-tab-view awesome-tab-current-tabset)))
    (switch-to-buffer
     (car
      (if (or (equal tab-index 0)
              (> tab-index (length visible-tabs)))
          (car (last visible-tabs))
        (nth (- tab-index 1) visible-tabs))))))

(defun awesome-tab-select-visible-tab ()
  "Bind this function with number keystroke, such as s-1, s-2, s-3 ... etc.

This function automatically recognizes the number at the end of the keystroke
and switches to the tab of the corresponding index.

Note that this function switches to the visible range,
not the actual logical index position of the current group."
  (interactive)
  (let* ((event last-command-event)
         (key (make-vector 1 event))
         (key-desc (key-description key)))
    (awesome-tab-select-visible-nth-tab
     (string-to-number (nth 1 (split-string key-desc "-"))))))

(defun awesome-tab-build-ace-strs (len key-number seqs)
  "Build strings for `awesome-tab-ace-jump'.
LEN is the number of strings, should be the number of current visible
tabs. NKEYS should be 1 or 2."
  (let ((i 0)
        (str nil))
    (when (>= key-number 3)
      (error "NKEYS should be 1 or 2"))
    (while (< i len)
      (push (apply #'string (elt seqs i)) str)
      (setq i (1+ i)))
    (nreverse str)))

(defun awesome-tab-ace-jump ()
  "Jump to a visible tab by 1 or 2 chars."
  (interactive)
  (catch 'quit
    (let* ((visible-tabs (awesome-tab-view awesome-tab-current-tabset))
           (visible-tabs-length (length visible-tabs))
           done-flag
           (lower-bound 0)
           (upper-bound visible-tabs-length)
           (ace-keys (length awesome-tab-ace-keys))
           (key-number (cond
                        ((<= visible-tabs-length ace-keys) 1)
                        ((<= visible-tabs-length (* ace-keys ace-keys)) 2)
                        (t (error "Too many visible tabs. Put more keys into `awesome-tab-ace-keys'."))))
           (visible-seqs
            (cl-subseq
             (symbol-value
              (intern
               (concat "awesome-tab-ace-" (number-to-string key-number) "-key-seqs")))
             0 visible-tabs-length))
           (ace-strs (awesome-tab-build-ace-strs visible-tabs-length key-number visible-seqs)))
      (setq awesome-tab-ace-state t)
      (awesome-tab-refresh-display)
      (dotimes (i key-number)
        (while (not done-flag)
          (let ((char (with-local-quit (read-key (format "Awesome Tab Ace Jump (%d):" (1+ i))))))
            (if (not (member char awesome-tab-ace-quit-keys))
                (let ((current-chars (mapcar #'car visible-seqs)))
                  (when (member char current-chars)
                    (setq done-flag t)
                    (setq lower-bound (cl-position char current-chars))
                    (setq upper-bound (1- (- visible-tabs-length (cl-position char (nreverse current-chars)))))
                    (dotimes (lower-index lower-bound)
                      (setcar (nthcdr lower-index visible-seqs) nil))
                    (setq upper-index (1+ upper-bound))
                    (while (< upper-index visible-tabs-length)
                      (setcar (nthcdr upper-index visible-seqs) nil)
                      (setq upper-index (1+ upper-index)))
                    (setq upper-index 0)
                    ))
              ;; Quit when user press Ctrl + g.
              (setq awesome-tab-ace-state nil)
              (awesome-tab-refresh-display)
              (throw 'quit nil))))
        (setq done-flag nil)
        (setq visible-seqs (mapcar #'cdr visible-seqs))
        (setq ace-strs (awesome-tab-build-ace-strs visible-tabs-length key-number visible-seqs))
        (awesome-tab-refresh-display))
      (setq awesome-tab-ace-state nil)
      (awesome-tab-refresh-display)
      (awesome-tab-buffer-select-tab (nth lower-bound visible-tabs)))))

;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;
(defun awesome-tab-get-groups ()
  ;; Refresh groups.
  (set awesome-tab-tabsets-tabset (awesome-tab-map-tabsets 'awesome-tab-selected-tab))
  (mapcar #'(lambda (group)
              (format "%s" (cdr group)))
          (awesome-tab-tabs awesome-tab-tabsets-tabset)))

(defun awesome-tab-get-extensions ()
  ;; Refresh groups.
  (set awesome-tab-tabsets-tabset (awesome-tab-map-tabsets 'awesome-tab-selected-tab))
  (let ((extension-names '()))
    (mapc #'(lambda (buffer)
              (with-current-buffer buffer
                (when (string-equal current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
                  (when (buffer-file-name buffer)
                    (add-to-list 'extension-names (file-name-extension (buffer-file-name buffer))))
                  )))
          (buffer-list))
    extension-names))

(defmacro awesome-tab-kill-buffer-match-rule (match-rule)
  `(save-excursion
     (mapc #'(lambda (buffer)
               (with-current-buffer buffer
                 (when (string-equal current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
                   (when (funcall ,match-rule buffer)
                     (kill-buffer buffer))
                   )))
           (buffer-list))))

;;;;;;;;;;;;;;;;;;;;;;; Default configurations ;;;;;;;;;;;;;;;;;;;;;;;

;; Uniquify tab name when open multiple buffers with same filename.
(setq uniquify-separator "/")
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)

(dolist (hook awesometab-hide-tabs-hooks)
  (add-hook hook '(lambda () (setq-local header-line-format nil))))

;; Rules to control buffer's group rules.
(defvar awesome-tab-groups-hash (make-hash-table :test 'equal))
(defvar awesome-tab-hide-hash (make-hash-table :test 'equal))

(defun awesome-tab-project-name ()
  (let ((project-name (cdr (project-current))))
    (if project-name
        (format "Project: %s" (expand-file-name project-name))
      awesome-tab-common-group-name)))

(defun awesome-tab-get-group-name (buf)
  (let ((group-name (gethash buf awesome-tab-groups-hash)))
    ;; Return group name cache if it exists for improve performance.
    (if group-name
        group-name
      ;; Otherwise try get group name with `project-current'.
      ;; `project-current' is very slow, it will slow down Emacs if you call it when switch buffer.
      (with-current-buffer buf
        (let ((project-name (awesome-tab-project-name)))
          (puthash buf project-name awesome-tab-groups-hash)
          project-name)))))

(defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    ((derived-mode-p 'eaf-mode)
     "EAF")
    (t
     (awesome-tab-get-group-name (current-buffer))))))

;; Helm source for switching group in helm.
(defvar helm-source-awesome-tab-group nil)

(defun awesome-tab-build-helm-source ()
  (interactive)
  (setq helm-source-awesome-tab-group
        (when (ignore-errors require 'helm)
          (helm-build-sync-source "Awesome-Tab Group"
                                  :candidates #'awesome-tab-get-groups
                                  :action '(("Switch to group" . awesome-tab-switch-group))))))

;;;###autoload
(defun awesome-tab-counsel-switch-group ()
  "Switch group of awesome-tab."
  (interactive)
  (when (ignore-errors require 'ivy)
    (ivy-read
     "Awesome-Tab Groups:"
     (awesome-tab-get-groups)
     :action #'awesome-tab-switch-group
     :caller 'awesome-tab-counsel-switch-group)))

(defun awesome-tab-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*flycheck" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(defun awesome-tab-hide-tab-cached (buf)
  (let ((hide (gethash buf awesome-tab-hide-hash 'not-found)))
    (when (eq hide 'not-found)
      (setq hide (funcall awesome-tab-hide-tab-function buf))
      (puthash buf hide awesome-tab-hide-hash))
    hide))

(defvar awesome-tab-last-focus-buffer nil
  "The last focus buffer.")

(defvar awesome-tab-last-focus-buffer-group nil
  "The group name of last focus buffer.")

(defun awesome-tab-remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun awesome-tab-insert-after (list aft-el el)
  "Insert EL after AFT-EL in LIST."
  (push el (cdr (member aft-el list)))
  list)

(defun awesome-tab-insert-before (list bef-el el)
  "Insert EL before BEF-EL in LIST."
  (nreverse (awesome-tab-insert-after (nreverse list) bef-el el)))

(provide 'awesome-tab)

;;; awesome-tab.el ends here
