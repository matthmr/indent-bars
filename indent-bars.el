;;; indent-bars.el --- Highlight indentation with bars -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2024  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/indent-bars
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.5"))
;; Version: 0.7.0
;; Keywords: convenience
;; Prefix: indent-bars
;; Separator: -

;; indent-bars is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; indent-bars is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; indent-bars highlights indentation with configurable vertical bars,
;; using stipples.  The color and appearance (weight, pattern,
;; position within the character, zigzag, etc.) are all configurable.
;; Options include depth-varying colors and highlighting the
;; indentation depth of the current line.  Bars span blank lines, by
;; default.  indent-bars works in any mode using fixed tab or
;; space-based indentation.  In the terminal (or on request) it uses
;; vertical bar characters instead of stipple patterns.

;; For Developers:
;;
;; To efficiently accommodate simultaneous alternative bar styling, we
;; do two things:
;;
;;  1. Collect all the style related information (color, stipple
;;     pattern, etc.) into a single struct, operating on one such
;;     "current" style struct at a time.
;;
;;  2. Provide convenience functions for replicating "alternative"
;;     custom style variables the user can configure; see
;;     `indent-bars--style'.  These variables can "inherit" nil or
;;     omitted plist variables from their parent var.
;;
;; Note the shorthand substitution for style related slot;
;; see file-local-variables at the end:
;; 
;;    ibs/  => indent-bars-style-

;;;; TODO: the default way the propertizing of the bars is done in code is that
;;;; it propertizes each individual bar of a bar-string (because it had so much
;;;; customazablity to begin with), instead of the full returning string
;;;; TODO: `indent-bars-highlight-current-depth' is still broken

;;; Code:
;;;; Requires
(require 'cl-lib)
(require 'timer)
(require 'font-lock)
(require 'face-remap)
(require 'compat)

;;;; Variables
(defvar indent-bars-mode)
(defvar-local indent-bars--regexp nil)

;;;;; Bar Colors
(defvar indent-bars-face nil
  "Face for `indent-bars's bars")

;;;;; Other
(defvar indent-bars-display-on-blank-lines t
  "Whether to display bars on blank lines.")

(defvar indent-bars-no-descend-string t
  "Configure bar behavior inside strings.
If non-nil, displayed bars inside the string will go no deeper
than the indent level of the string's starting line.")

(defvar indent-bars-no-descend-lists t
  "Configure bar behavior inside lists.
If non-nil, displayed bars will go no deeper than the indent
level at the starting line of the innermost containing list.")

(defvar indent-bars-char ?│
  "Character to display when stipple is unavailable (as in the terminal).")

(defvar indent-bars-starting-column 0
  "The starting column on which to display the first bar.
Set to nil, for the default behavior (first bar at the first
indent level) or an integer value for some other column.")

(defvar indent-bars-spacing-override nil
  "Override for default, major-mode based indentation spacing.
Set only if the default guessed spacing is incorrect.  Becomes
buffer-local automatically.")

(defvar indent-bars-bar nil
  "Propertized value of the bar character")

(defun indent-bars-with-style (style)
  (setq indent-bars-bar (propertize (string indent-bars-char) 'face style)))

;;;; Indentation and Drawing
(defvar-local indent-bars-spacing nil)
(defvar-local indent-bars--offset nil)

(defsubst indent-bars--depth (len)
  "Number of possible bars for initial blank string of length LEN.
Note that the first bar is expected at `indent-bars-starting-column'."
  (if (> len indent-bars--offset)
      (1+ (/ (- len indent-bars--offset 1) indent-bars-spacing))
    0))

(defun indent-bars--context-depth ()
  "Return the maximum `current-indentation' around current line.
Skips any fully blank lines."
  (let (b (p (point)))
    (beginning-of-line)
    (skip-chars-backward "[:space:]\n")
    (setq b (current-indentation))
    (goto-char p)
    (forward-line 1)
    (skip-chars-forward "[:space:]\n")
    (prog1
        (max (current-indentation) b)
      (goto-char p))))

(defvar indent-bars--update-depth-function nil)
(defvar indent-bars--ppss nil)
(defun indent-bars--current-indentation-depth (&optional on-bar)
  "Calculate current indentation depth.
If ON-BAR is nil, return the depth of the last visible bar on the
line.  If ON-BAR is non-nil and content begins at a column where
a bar would otherwise have fallen, report the depth of
that (undrawn) bar.  If ON-BAR is the symbol `context', and the
first non-blank line immediately above or below the current line
is not at a deeper indentation level (by at least one bar
spacing), disable on-bar and use the last-visible-bar depth for
that line instead.

If `indent-bars-no-descend-string' is non-nil and point at line
beginning is inside a string, do not add bars deeper than one
more than the string's start.  If `indent-bars-no-descend-lists'
is non-nil, perform the same check for lists.

If `indent-bars--update-depth-function' is non-nil, it will be
called with the indentation depth (prior to the ON-BAR check),
and can return an updated depth."
  (let* ((c (current-indentation))
         (d (indent-bars--depth c))         ;last visible bar
         ppss-ind)
    (when indent-bars--ppss
      (let* ((p (prog1 (point) (forward-line 0)))
             (ppss (syntax-ppss))         ; moves point!
             (ss (and indent-bars-no-descend-string (nth 8 ppss)))
             (sl (and indent-bars-no-descend-lists (nth 1 ppss))))
        (when (setq ppss-ind (if (and ss sl) (max ss sl) (or ss sl)))
          (goto-char ppss-ind)
          (let* ((cnew (current-indentation))
                 (dnew (1+ (indent-bars--depth cnew))))
            (when (< dnew d) (setq d dnew c cnew))))
        (goto-char p)))
    (when (and indent-bars--update-depth-function (not ppss-ind))
      (setq d (funcall indent-bars--update-depth-function d)))
    (when (and (eq on-bar 'context)
               (< (indent-bars--context-depth) (+ c indent-bars-spacing)))
      (setq on-bar nil))
    (if (and on-bar (= c (+ indent-bars--offset (* d indent-bars-spacing))))
        (1+ d) d)))

(defun indent-bars--blank-string (off nbars bar-from &optional width)
  "Return a blank string with bars displayed, using style STYLE.
OFF is the character offset within the string to draw the first
bar, NBARS is the desired number of bars to add, and BAR-FROM is
the starting index of the first bar (>=1).  WIDTH is the total
string width to return, right padding with space if needed.

If SWITCH-AFTER is supplied and is an integer, switch from STYLE
to STYLE2 after drawing that many bars.  If it is t, use
STYLE2 for all bars.

Bars are displayed using stipple properties or characters; see
`indent-bars-prefer-character'."
  (concat (make-string off ?\s)
          (string-join
           (cl-loop
            for i from 0 to (1- nbars)
            collect indent-bars-bar)
           (make-string (1- indent-bars-spacing) ?\s))
          (if width
              (make-string (- width
                              (+ off nbars (* (1- nbars) (1- indent-bars-spacing))))
                           ?\s))))

(defun indent-bars--tab-display (p off bar-from max &rest r)
  "Display up to MAX bars on the tab at P, offseting them by OFF.
Bars are spaced by `indent-bars-spacing' and displayed with style
STYLE.  BAR-FROM is the bar number for the first bar.  Other
arguments R are passed to `indent-bars--blank-string'.  Returns
the number of bars actually displayed."
  (let* ((nb (min max (/ (- tab-width off -1) indent-bars-spacing)))
         (str (apply #'indent-bars--blank-string off nb
                     bar-from tab-width r)))
    (put-text-property p (+ p 1) 'indent-bars-display str)
    nb))

(defun indent-bars--draw-line (nbars start end &optional invent)
  "Draw NBARS bars on the line between positions START and END.
Bars are drawn in style STYLE, `indent-bars-style' by default.
START is assumed to be on a line beginning position.  Drawing
starts at a column determined by `indent-bars-starting-column'.
Tabs at the line beginning have appropriate display properties
applied if `indent-tabs-mode' is enabled.

If SWITCH-AFTER is an integer, switch from STYLE to STYLE2
after drawing that many bars.  If it is t, use STYLE2
exclusively.

If INVENT is non-nil and the line's length is insufficient to
display all NBARS bars (whether by replacing tabs or adding
properties to existing non-tab whitespace), bars will be
\"invented\".  That is, the line's final newline, which is (only
in this case) expected to be located at END, will have its
display properties set to fill out the remaining bars, if any are
needed."
  (let* ((tabs (when (and indent-tabs-mode
                          (save-excursion
                            (goto-char start) (looking-at "^\t+")))
                 (- (match-end 0) (match-beginning 0))))
         (vp indent-bars--offset)
         (bar 1) prop tnum bars-drawn)
    (when tabs                                ; deal with initial tabs
      (while (and (<= bar nbars) (< (setq tnum (/ vp tab-width)) tabs))
        (setq bars-drawn
              (indent-bars--tab-display (+ start tnum) (mod vp tab-width)
                                        bar (- nbars bar -1)))
        (cl-incf bar bars-drawn)
        (cl-incf vp (* bars-drawn indent-bars-spacing)))
      (cl-incf start (+ (mod vp tab-width) (/ vp tab-width))))
    (when (<= bar nbars)                ; still bars to show
      (setq prop 'indent-bars-display)
      (let ((pos (if tabs start (+ start indent-bars--offset))))
        (while (and (<= bar nbars) (< pos end))
          (put-text-property
           pos (1+ pos)
           prop indent-bars-bar)
          (cl-incf bar)
          (cl-incf pos indent-bars-spacing))
        ;; STILL bars to show: invent them (if requested)
        (if (and invent (<= bar nbars))
            (put-text-property
             end (1+ end) 'indent-bars-display
             (concat (indent-bars--blank-string
                      (- pos end) (- nbars bar -1) bar nil)
                     "\n")))))
    ))

(defsubst indent-bars--context-bars (end)
  "Maximum number of bars at point and END.
Moves point."
  (max (indent-bars--current-indentation-depth)
       (progn
         (goto-char (1+ end))                ; end is always eol
         (indent-bars--current-indentation-depth))))

(defun indent-bars--display (beg end)
  "Draw indentation bars from BEG..END, based on line contents.
BEG and END should be on the same line.  STYLE, SWITCH-AFTER and
STYLE2 are as in `indent-bars--draw-line'.  If STYLE is not
passed, uses `indent-bars-style' for drawing."
  (let ((n (save-excursion
             (goto-char beg)
             (indent-bars--current-indentation-depth))))
    (when (> n 0) (indent-bars--draw-line n beg end nil))))

(defun indent-bars--display-blank-lines (beg end)
  "Display the appropriate bars over the blank-only lines from BEG..END.
Only called if `indent-bars-display-on-blank-lines' is non-nil.
To be called on complete multi-line blank line regions.

It is ambigious how many bars to draw on each line in a stretch
of blank lines, so this uses the maximum depth of the surrounding
line indentation, above and below.  Drawing using
`indent-bars--draw-line'.  STYLE, SWITCH-AFTER and STYLE2 are as
in `indent-bars--draw-line'.

Note: blank lines at the very beginning or end of the buffer are
not indicated, even if they otherwise would be."
  (let ((pm (point-max)) ctxbars)
    (save-excursion
      (goto-char (1- beg))
      (beginning-of-line 1)
      (when (> (setq ctxbars (indent-bars--context-bars end)) 0)
        (goto-char beg)
        (while (< (point) end) ;note: end extends 1 char beyond blank line range
          (let* ((bp (line-beginning-position))
                 (ep (line-end-position)))
            (unless (= ep pm)
              (indent-bars--draw-line ctxbars bp ep 'invent))
            (beginning-of-line 2)))))
    nil))

;;;; jit-lock support
(defvar indent-bars--orig-fontify-region nil)
(defun indent-bars--extend-region (start end)
  "Extend the region START..END.
If `indent-bars-display-on-blank-lines' is non-nil, this extends
it to include complete contiguous stretches of blank lines and
always starts and ends on the beginning of a line.  Uses and sets
the dynamic variables `jit-lock-start' and `jit-lock-end'."
  (save-excursion
    (let ((chars " \t\n"))
      (goto-char start)
      (forward-line 0)
      (when (and indent-bars-display-on-blank-lines
                 (< (skip-chars-backward chars) 0))
        (unless (bolp) (forward-line 1)))
      (when (< (point) start) (setq start (point)))
      (goto-char end)
      (unless (bolp) (forward-line 1))
      (when (and indent-bars-display-on-blank-lines
                 (> (skip-chars-forward chars) 0))
        (unless (bolp) (forward-line 0)))
      (when (> (point) end) (setq end (point)))))
  (cons start end))

(defvar-local indent-bars--display-function
    'indent-bars--display)
(defvar-local indent-bars--display-blank-lines-function
    'indent-bars--display-blank-lines)
(defun indent-bars--draw-all-bars-between (start end)
  "Search for and draw all bars between START and END.
The beginning of line at START is used to locate real and (if
configured) blank-line bars, which are drawn in the appropriate
style.  This is basically a very tiny, bar-only version of what
`font-lock-fontify-region-keywords' does."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward indent-bars--regexp end t))
      (if (match-beginning 2)
          (funcall indent-bars--display-blank-lines-function
                   (match-beginning 2) (match-end 2))
        (funcall indent-bars--display-function
                 (match-beginning 1) (match-end 1))))))

(defvar-local indent-bars--font-lock-inhibit nil)
(defun indent-bars--fontify (beg end verbose)
  "Add indent-bars from BEG to END after calling font-lock there.
The VERBOSE argument is provided by font-lock.  If
`indent-bars--font-lock-inhibit' is a function, call it with BEG
and END.  If it returns non-nil, skip font-lock."
  ;; Fontify with font-lock, unless inhibited (clears + sets 'face)
  (unless (and indent-bars--font-lock-inhibit
               (funcall indent-bars--font-lock-inhibit beg end))
    (pcase (funcall indent-bars--orig-fontify-region beg end verbose)
      (`(jit-lock-bounds ,beg1 . ,end1) (setq beg beg1 end end1))))
  ;; Always draw bars (which may overwrite 'face in a few places)
  (pcase-let ((`(,beg . ,end) (indent-bars--extend-region beg end)))
    (with-silent-modifications
      (remove-text-properties beg end '(indent-bars-display nil))
      (indent-bars--draw-all-bars-between beg end))
    `(jit-lock-bounds ,beg . ,end)))

;;;; Current depth

(defvar indent-bars-current-face nil
  "Face for `indent-bars's current depth bar")

(defvar indent-bars-highlight-current-depth nil
  "Flag for whether to highlight the current depth bar differently")

(defvar indent-bars-highlight-selection-method 'context
  "Method for selecting bar depth for current indentation highlight.
If nil, the last showing bar on the current line is selected for
highlight.  If the symbol `on-bar', and the start of the text on
the line would have fallen directly on a bar, highlight that bar
depth instead.  If `context', use `on-bar' logic, but only if a
directly adjacent (non-blank) context line is indented deeper, by
more than one indent spacing.  Otherwise select the last bar
showing for highlight (i.e. the same as CONTEXT nil).")

(defvar indent-bars-depth-update-delay 0.075
  "Minimum delay time in seconds between depth highlight updates.
Has effect only if `indent-bars-highlight-current-depth' is
non-nil.  Set to 0 for instant depth updates.")

;;; Current indentation depth highlighting
(defvar-local indent-bars--current-depth 0)
(defvar-local indent-bars--remaps nil
  "An alist of active face-remap cookies for faces.
Keyed by style tag (nil for the main style).  These remaps are
used to change the current depth highlight (aside from stipple changes).")

(defvar-local indent-bars--highlight-timer nil)
(defun indent-bars--update-current-depth-highlight (depth)
  "Update highlight for the current DEPTH.
Works by remapping the appropriate indent-bars[-tag]-N face for
all styles in the `indent-bars--styles' list.  DEPTH should be
greater than zero."
  (setq indent-bars--highlight-timer nil)
  (dolist (s indent-bars--styles)
    (let* ((face indent-bars-face)
           (hl-col indent-bars-face)
           (hl-bg indent-bars-current-face))
      (when (or hl-col hl-bg (ibs/current-stipple-face s))
        (when-let ((c (alist-get (ibs/tag s) indent-bars--remaps))) ; out with the old
          (face-remap-remove-relative c))
        (setf (alist-get (ibs/tag s) indent-bars--remaps)
              (face-remap-add-relative
               face
               `(,@(when hl-col `(:foreground ,hl-col))
                 ,@(when hl-bg `(:background ,hl-bg)))
               (ibs/current-stipple-face s))))
      )))

(defun indent-bars--update-current-depth-highlight-in-buffer (buf depth)
  "Highlight bar at DEPTH in buffer BUF."
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (indent-bars--update-current-depth-highlight depth))))

(defun indent-bars--highlight-current-depth (&optional force)
  "Refresh current indentation depth highlight.
Rate limit set by `indent-bars-depth-update-delay'.  If FORCE is
non-nil, update depth even if it has not changed."
  (unless (or indent-bars--highlight-timer (not indent-bars-mode))
    (let* ((depth (indent-bars--current-indentation-depth
                   indent-bars-highlight-selection-method)))
      (when (and depth (or force (not (= depth indent-bars--current-depth)))
                 (> depth 0))
        (setq indent-bars--current-depth depth)
        (if (zerop indent-bars-depth-update-delay)
            (indent-bars--update-current-depth-highlight depth)
          (setq indent-bars--highlight-timer
                (run-with-idle-timer
                 indent-bars-depth-update-delay nil
                 #'indent-bars--update-current-depth-highlight-in-buffer
                 (current-buffer) depth)))))))

;;;; Setup and mode
(defun indent-bars--guess-spacing ()
  "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
  (cond
   (indent-bars-spacing-override)
   ((and (derived-mode-p 'python-mode) (boundp 'py-indent-offset))
    py-indent-offset)
   ((and (derived-mode-p 'python-mode) (boundp 'python-indent-offset))
    python-indent-offset)
   ((and (derived-mode-p 'ruby-mode) (boundp 'ruby-indent-level))
    ruby-indent-level)
   ((and (derived-mode-p 'scala-mode) (boundp 'scala-indent:step))
    scala-indent:step)
   ((and (derived-mode-p 'scala-mode) (boundp 'scala-mode-indent:step))
    scala-mode-indent:step)
   ((and (or (derived-mode-p 'scss-mode) (derived-mode-p 'css-mode))
         (boundp 'css-indent-offset))
    css-indent-offset)
   ((and (derived-mode-p 'nxml-mode) (boundp 'nxml-child-indent))
    nxml-child-indent)
   ((and (derived-mode-p 'coffee-mode) (boundp 'coffee-tab-width))
    coffee-tab-width)
   ((and (derived-mode-p 'js-mode) (boundp 'js-indent-level))
    js-indent-level)
   ((and (derived-mode-p 'js2-mode) (boundp 'js2-basic-offset))
    js2-basic-offset)
   ((and (derived-mode-p 'sws-mode) (boundp 'sws-tab-width))
    sws-tab-width)
   ((and (derived-mode-p 'web-mode) (boundp 'web-mode-markup-indent-offset))
    web-mode-markup-indent-offset)
   ((and (derived-mode-p 'web-mode) (boundp 'web-mode-html-offset)) ; old var
    web-mode-html-offset)
   ((and (local-variable-p 'c-basic-offset) (numberp c-basic-offset))
    c-basic-offset)
   ((and (local-variable-p 'c-ts-common-indent-offset)
         (symbolp c-ts-common-indent-offset)
         (numberp (symbol-value c-ts-common-indent-offset)))
    (symbol-value c-ts-common-indent-offset))
   ((and (derived-mode-p 'yaml-mode) (boundp 'yaml-indent-offset))
    yaml-indent-offset)
   ((and (derived-mode-p 'elixir-mode) (boundp 'elixir-smie-indent-basic))
    elixir-smie-indent-basic)
   ((and (derived-mode-p 'lisp-data-mode) (boundp 'lisp-body-indent))
    lisp-body-indent)
   ((and (derived-mode-p 'cobol-mode) (boundp 'cobol-tab-width))
    cobol-tab-width)
   ((or (derived-mode-p 'go-ts-mode) (derived-mode-p 'go-mode))
    tab-width)
   ((derived-mode-p 'nix-mode)
    tab-width)
   ((and (derived-mode-p 'nix-ts-mode) (boundp 'nix-ts-mode-indent-offset))
    nix-ts-mode-indent-offset)
   ((and (derived-mode-p 'json-ts-mode) (boundp 'json-ts-mode-indent-offset))
    json-ts-mode-indent-offset)
   ((and (derived-mode-p 'json-mode) (boundp 'js-indent-level))
    js-indent-level)
   ((and (boundp 'standard-indent) standard-indent))
   (t 2)))                                 ; backup

(defun indent-bars--setup-font-lock ()
  "Wrap the `font-lock-fontify-region-function' to provide bars."
  ;; Setup to wrap font-lock-fontify-region
  (unless (eq font-lock-fontify-region-function #'indent-bars--fontify)
    (setq indent-bars--orig-fontify-region font-lock-fontify-region-function)
    (setq-local font-lock-fontify-region-function #'indent-bars--fontify))
  (setq indent-bars--regexp
        (rx-to-string
         `(seq bol (or (and
                        ;; group 1: basic blank indent detection
                        (group
                         ,(if (not indent-tabs-mode)
                              `(>= ,(1+ indent-bars--offset) ?\s)
                            '(+ (any ?\t ?\s))))
                        (not (any ?\t ?\s ?\n)))
                       ;; group 2: multi-line blank regions
                       ,@(if indent-bars-display-on-blank-lines
                             '((group (* (or ?\s ?\t ?\n)) ?\n))))))))

(defun indent-bars-setup ()
  "Setup all face, color, bar size, and indentation info for the current buffer."
  ;; Spacing
  (setq indent-bars-spacing (indent-bars--guess-spacing)
        indent-bars--offset (or indent-bars-starting-column indent-bars-spacing))

  ;; PPSS
  (setq indent-bars--ppss
        (or indent-bars-no-descend-string indent-bars-no-descend-lists))

  ;; Current depth Highlighting
  (when indent-bars-highlight-current-depth
    (add-hook 'post-command-hook
              #'indent-bars--highlight-current-depth nil t)
    (setq indent-bars--current-depth 0)
    (indent-bars--highlight-current-depth))

  ;;Jit/Font-lock
  (cl-pushnew 'indent-bars-display (alist-get 'display char-property-alias-alist))
  (indent-bars--setup-font-lock)
  (font-lock-flush))

(defvar indent-bars--teardown-functions nil)
(defun indent-bars-teardown ()
  "Tears down indent-bars."
  ;; Remove current highlight remaps
  ;; (dolist (s indent-bars--styles)
  ;;   (face-remap-remove-relative
  ;;    (alist-get (ibs/tag s) indent-bars--remaps)))

  (when indent-bars--orig-fontify-region
    (setq font-lock-fontify-region-function indent-bars--orig-fontify-region))
  (remove-text-properties (point-min) (point-max) '(indent-bars-display nil))

  (font-lock-flush)
  (font-lock-ensure)

  (setq indent-bars--current-depth 0)
  (remove-hook 'post-command-hook #'indent-bars--highlight-current-depth t)
  (remove-hook 'window-state-change-functions
               #'indent-bars--window-change t)
  (run-hooks 'indent-bars--teardown-functions))

(defun indent-bars-reset (&rest _r)
  "Reset indent-bars config."
  (interactive)
  (indent-bars-teardown)
  (indent-bars-setup))

(defun indent-bars-setup-and-remove (frame)
  "Setup indent bars for FRAME and remove from `after-make-frame-functions'."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (remove-hook 'after-make-frame-functions #'indent-bars-setup-and-remove t)
      (indent-bars-setup))))

;;;###autoload
(define-minor-mode indent-bars-mode
  "Indicate indentation with configurable bars."
  :global nil
  (if indent-bars-mode
      (if (and (daemonp) (not (frame-parameter nil 'client)))
          (add-hook 'after-make-frame-functions #'indent-bars-setup-and-remove
                    nil t)
        (indent-bars-setup))
    (indent-bars-teardown)))

(provide 'indent-bars)

;;; indent-bars.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ibs/" . "indent-bars-style-"))
;; End:
