;;; helheim-latex-lib.el -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'dash))
(eval-when-compile (require 'hel-macros))
(require 'hel)

(defvar cdlatex-command-alist-comb)
(declare-function texmathp "texmathp")
(declare-function cdlatex--texmathp "cdlatex")
(declare-function org-inside-LaTeX-fragment-p "org")
(declare-function org-element-at-point "org-element")
(declare-function org-element-type "org-element")

;;; Math context

;;;###autoload
(defun +latex-in-math-p ()
  "Return non-nil when point is inside LaTeX math.

In an Org buffer this covers both fragments (\\(..\\), \\[..\\], $..$)
and `latex-environment' elements.

Anywhere else the answer comes from `texmathp', which AUCTeX provides
and which reads the LaTeX source itself."
  (cond ((derived-mode-p 'org-mode)
         (or (org-inside-LaTeX-fragment-p)
             (eq (-> (org-element-at-point) (org-element-type))
                 'latex-environment)))
        ((require 'texmathp nil t)
         (texmathp))))

(defun +cdlatex-active-p ()
  "Return non-nil when CDLaTeX owns the editing keys in this buffer."
  (or (bound-and-true-p org-cdlatex-mode)
      (bound-and-true-p cdlatex-mode)))

;;;###autoload
(defun +latex-inhibit-corfu-auto-p ()
  "Return non-nil inside math where CDLaTeX is active.
Meant for `+corfu-inhibit-auto-functions'."
  (and (+cdlatex-active-p)
       (+latex-in-math-p)))

;;; CDLaTeX snippets

;;;###autoload
(defun +latex-cdlatex-keyword-before-point ()
  "Return the `cdlatex-command-alist-comb' entry `cdlatex-tab' would expand.
Return nil when the word before point is not a CDLaTeX keyword, or when
the keyword is not allowed in the current math context.

This repeats the lookup at the head of `cdlatex-tab', so the answer says
exactly whether TAB is about to expand something."
  (when-let* ((_ (+cdlatex-active-p))
              (word (save-excursion
                      (let ((pos (point)))
                        (backward-word 1)
                        (while (eq (following-char) ?$) (forward-char 1))
                        (buffer-substring-no-properties (point) pos))))
              (entry (assoc word cdlatex-command-alist-comb)))
    (and (if (cdlatex--texmathp) (nth 6 entry) (nth 5 entry))
         entry)))

;;; Locating the TeX object before point

(cl-defun +latex-adjacent-tex-object-start (&optional (pos (point)))
  "Return the start position of the TeX object ending just before POS.
POS defaults to point. Return nil when there is no such object.

Understands balanced (), [] and {} groups, plain words, a macro name in
front of a brace group (so `\\alpha' and `\\mathbf{v}' come back
whole), and chains through `_', `^' and `.', so that `a_{ij}' or `x^2'
count as one object. Adapted from `laas-identify-adjacent-tex-object'."
  (save-excursion
    (goto-char pos)
    (cond ((memq (char-before) '(?\) ?\]))
           (backward-sexp)
           (point))
          ((eq (char-before) ?})
           (backward-sexp)
           (while (eq (char-before) ?})
             (backward-sexp))
           ;; If the braces belong to a macro, swallow the macro too.
           (when (looking-back "\\\\[A-Za-z@*]+" (line-beginning-position))
             (goto-char (match-beginning 0)))
           (when (memq (char-before) '(?_ ?^ ?.))
             (backward-char)
             (-some-> (+latex-adjacent-tex-object-start) (goto-char)))
           (point))
          ((and (char-before)
                (or (<= ?a (char-before) ?z)
                    (<= ?A (char-before) ?Z)
                    (<= ?0 (char-before) ?9)))
           (backward-word)
           (when (eq (char-before) ?\\)
             (backward-char))
           (when (memq (char-before) '(?_ ?^ ?.))
             (backward-char)
             (-some-> (+latex-adjacent-tex-object-start) (goto-char)))
           (point)))))

;;; Fractions

;;;###autoload
(defun +latex-frac ()
  "Insert a fraction, filling the numerator from context.
With an active region, wrap it as the numerator. Otherwise, if a TeX object sits
immediately before point, move that into the numerator instead: `x^2//' gives
\\frac{x^2}{}, and `(a+b)//' gives \\frac{a+b}{}, dropping the now-redundant
outer brackets. If neither applies, insert an empty template. Point always ends
up in the empty field."
  (interactive)
  (if (use-region-p)
      (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "\\frac{%s}{}" s))
        (backward-char 1))
    (if-let* ((object-start (+latex-adjacent-tex-object-start)))
        (let* ((start (save-excursion
                        (if (memq (char-before) '(?\) ?\]))
                            (progn (delete-char -1)
                                   (goto-char object-start)
                                   (delete-char 1))
                          (goto-char object-start))
                        (point)))
               (end (point))
               (content (buffer-substring-no-properties start end)))
          (delete-region start end)
          (insert "\\frac{" content "}{}")
          (backward-char 1))
      (insert "\\frac{}{}")
      (backward-char 3))))

;;; Automatic subscripts

(defvar aas-transient-snippet-key)
(defvar aas-transient-snippet-condition-result)

;;;###autoload
(defun +latex--auto-script-condition ()
  "Condition for the automatic subscript snippets.

aas calls this with point *before* the trigger key. Return one of:

  `one-sub'      start a fresh subscript:   x   + 1 -> x_1
  `extended-sub' widen a bare script:       x_1 + 2 -> x_{12}
  `braced-sub'   append inside braces:      x_{12} + 3 -> x_{123}

or nil to leave the key alone. `braced-sub' is an addition on top of laas,
which stops after two characters.

Deliberately conservative: a new subscript only starts after a *single* letter,
so `\\sin2' and `abc1' are left untouched. Type `_' (CDLaTeX gives you `_{}')
when you want a subscript anywhere else."
  (unless (or (bobp) (= (1- (point)) (point-min)))
    (let ((prev (char-before))
          (prev2 (char-before (1- (point)))))
      (cond ((and (memq prev2 '(?_ ?^))
                  (not (eq prev ?{))
                  (+latex-in-math-p))
             'extended-sub)
            ((and (eq prev ?})
                  (save-excursion
                    (ignore-errors
                      (backward-sexp)
                      (memq (char-before) '(?_ ?^))))
                  (+latex-in-math-p))
             'braced-sub)
            ((and (or (<= ?a prev ?z) (<= ?A prev ?Z))
                  (not (or (<= ?a prev2 ?z) (<= ?A prev2 ?Z)))
                  (+latex-in-math-p))
             'one-sub)))))

;;;###autoload
(defun +latex-insert-script ()
  "Insert or extend a subscript, branching on the condition result.
The inserted text is the last character of `aas-transient-snippet-key',
so the digit triggers and the `ii'/`jj'/`nn'/`kk' triggers can all share
this one function."
  (interactive)
  (let ((s (substring aas-transient-snippet-key -1)))
    (pcase aas-transient-snippet-condition-result
      ('one-sub
       (insert "_" s))
      ('extended-sub ; point is right after the lone script char
       (backward-char)
       (insert "{")
       (forward-char)
       (insert s "}"))
      ('braced-sub ; point is right after the closing brace
       (backward-char)
       (insert s)
       (forward-char)))))

;;; Surround in \left \right sized delimiters

;; CDLaTeX's own `lr(', `lr[', ... entries in `cdlatex-command-alist' stay
;; enabled, but they only fire when a non-word character precedes the
;; trigger: `cdlatex-tab' looks the keyword up with `backward-word', so
;; `\(x lr(' and `\(x+lr(' expand while `\(xlr(' does not -- which misses
;; exactly the common cases, `f(x)' and `2(a+b)'. The two entry points
;; below have no such restriction.

(defconst +latex--delimiter-alist
  '((?\) "\\left( "         . " \\right)")
    (?\] "\\left[ "         . " \\right]")
    (?}  "\\left\\{ "       . " \\right\\}")
    (?>  "\\left\\langle "  . " \\right\\rangle")
    (?|  "\\left| "         . " \\right|")
    (?\( "\\left(\n"        . "\n\\right)")
    (?\[ "\\left[\n"        . "\n\\right]")
    (?{  "\\left\\{\n"      . "\n\\right\\}")
    (?<  "\\left\\langle\n" . "\n\\right\\rangle"))
  "Delimiter characters, and the LaTeX pair each one inserts.")

(defconst +latex--fragment-alist
  '((?\( "\\( "  . " \\)")
    (?\) "\\( "  . " \\)")
    (?\[ "\\[\n" . "\n\\]")
    (?\] "\\[\n" . "\n\\]"))
  "Delimiter characters, and the math fragment each one opens.")

(defun +latex--insert-empty-pair (left right block)
  "Insert an empty LEFT...RIGHT pair and leave point inside it.
With BLOCK non-nil each delimiter gets a line of its own and point lands
on the empty line between them."
  (if block
      (let (beg mid)
        (delete-horizontal-space t)
        (unless (bolp) (insert "\n"))
        (setq beg (point))
        (insert left "\n")
        (setq mid (point-marker))
        (insert "\n" right)
        (delete-horizontal-space)
        (unless (eolp) (insert "\n"))
        (indent-region beg (point))
        ;; MID sits at the head of the empty line; `end-of-line' steps
        ;; over whatever indentation `indent-region' put there.
        (goto-char mid)
        (end-of-line)
        (set-marker mid nil))
    (insert left right)
    (backward-char (length right))))

(defun +latex--surround-range (beg end left right block)
  "Surround the text between BEG and END with LEFT and RIGHT.
With BLOCK non-nil each delimiter gets a line of its own and the whole
block is re-indented."
  (let ((beg (copy-marker beg))
        (end (copy-marker end t)))
    (save-excursion
      (if block
          (progn
            (goto-char end)
            (unless (bolp) (insert "\n"))
            (insert right)
            (delete-horizontal-space)
            (unless (eolp) (insert "\n"))
            (goto-char beg)
            (delete-horizontal-space t)
            (unless (bolp) (insert "\n"))
            (insert left "\n")
            ;; Point sits at the head of the text; LEFT is the line above.
            (indent-region (line-beginning-position 0) end))
        (goto-char end) (insert right)
        (goto-char beg) (insert left)))
    (set-marker beg nil)
    (set-marker end nil)))

(defun +latex--surround-region (left right block)
  "Surround the active region with LEFT and RIGHT.
The selection shrinks past the whitespace at its ends first, so the delimiters
land on the text itself. With BLOCK non-nil each delimiter gets a line of its
own and the whole block is re-indented."
  (let ((beg (save-excursion
               (goto-char (region-beginning))
               (skip-chars-forward " \t\r\n")
               (point)))
        (end (save-excursion
               (goto-char (region-end))
               (skip-chars-backward " \t\r\n")
               (point))))
    (if (< beg end)
        (+latex--surround-range beg end left right block)
      ;; The selection holds nothing but whitespace: there is no text to
      ;; surround, so leave an empty pair for the user to fill.
      (+latex--insert-empty-pair left right block))))

;;;###autoload
(defun +latex-insert-delimiters ()
  "Insert an empty \\left...\\right pair on one line, point between them.
The pair is chosen from the first character of `aas-transient-snippet-key', so
one function serves every delimiter trigger in the table below. That character
is an opening one, which stands for the block layout, so the line break in each
string becomes the space it stands in for."
  (interactive)
  (-let [(left . right) (alist-get (aref aas-transient-snippet-key 0)
                                   +latex--delimiter-alist)]
    (+latex--insert-empty-pair (string-replace "\n" " " left)
                               (string-replace "\n" " " right)
                               nil)))

;;;###autoload (autoload '+latex-surround "helheim-latex-lib" nil t)
(hel-define-command +latex-surround ()
  "Surround text with LaTeX delimiters, chosen by where point is.

Inside math the pair is a sized \\left...\\right one, and the invoking
key picks its layout. An opening character -- (, [, {, < -- puts each
delimiter on a line of its own. A closing character -- ), ], }, >, | --
keeps the pair on one line, one space away from the text.

Outside math the pair opens a math fragment instead: ( and ) give inline
math, \\( ... \\), while [ and ] give display math, \\[ ... \\], on lines
of its own. The layout follows the fragment, so an opening character and
its closing partner do the same thing there. The other characters have
no fragment and raise an error.

With an active region, surround the region. Inside math with no region,
surround the TeX object right before point, so x^2 becomes
\\left( x^2 \\right). With neither, insert an empty pair and leave point
inside it.

Called any other way than from a key binding, the character is read from
the minibuffer."
  :multiple-cursors t
  (interactive)
  (let* ((math (+latex-in-math-p))
         (table (if math +latex--delimiter-alist +latex--fragment-alist))
         ;; `+latex--delimiter-alist' holds every character the command is
         ;; bound to, so it decides whether a key invoked us. TABLE then
         ;; decides what that key means here.
         (char (if (assq last-command-event +latex--delimiter-alist)
                   last-command-event
                 (read-char (if math "Delimiter: |([{< or )]}>" "Math: ) or ]")))))
    (-if-let* (((left . right) (alist-get char table)))
        (let ((block (or (string-search "\n" left)
                         (string-search "\n" right)))
              ;; Outside math the text before point is prose, and the word
              ;; it ends with is no more a formula than the rest of it.
              (object-start (if (and math (not (use-region-p)))
                                (+latex-adjacent-tex-object-start))))
          (when block
            (setq left  (string-trim left)
                  right (string-trim right)))
          (cond ((use-region-p)
                 (+latex--surround-region left right block))
                (object-start
                 (+latex--surround-range object-start (point) left right block))
                (t
                 (+latex--insert-empty-pair left right block))))
      (if math
          (user-error "No \\left...\\right pair for `%c'" char)
        (user-error "Not inside math, and `%c' opens no fragment" char)))))

;;; .
(provide 'helheim-latex '(lib))
;;; helheim-latex-lib.el ends here
