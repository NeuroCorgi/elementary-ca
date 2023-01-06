;;; ca-mode.el --- major mode for cellular automata -*- lexical-binding: t; -*-

(defmacro ca-white-char () ?@)
(defmacro ca-black-char () ?\ )

(defmacro ca-white-string () (char-to-string (ca-white-char)))
(defmacro ca-black-string () (char-to-string (ca-black-char)))

(defvar ca-previous-deltas nil)

(defvar ca-rule 0)
(defvar ca-rule-string "0")

(defun ca-make-white ()
  (interactive "" '(ca-mode))
  (let ((inhibit-read-only t))
    (save-excursion
      (when (= (line-number-at-pos) 1)
        (delete-char 1)
        (insert (ca-white-char))
        (ca-evolve)))))

(defun ca-make-black ()
  (interactive "" '(ca-mode))
  (let ((inhibit-read-only t))
    (save-excursion
      (when (= (line-number-at-pos) 1)
        (delete-char 1)
        (insert (ca-black-char))
        (ca-evolve)))))

(defun ca-scroll-generations (arg)
  (interactive "p" '(ca-mode))
  (scroll-down arg))

(defun ca-set-rule (rule)
  (interactive "nRule: " '(ca-mode))
  (save-excursion
    (if (or (> rule 255) (< rule 0))
        (error "Rule should be in range from 0 to 255: %d" rule)
      (setq ca-rule rule)
      (setq ca-rule-string (int-to-string ca-rule))
      (ca-evolve))))

;;;###autoload
(defun cellular-automaton (&optional rule)
  "Starts elementary cellular automaton.
Initial state can be modified with `ca-make-white` and
`ca-make-black`. The rule can be changed with `ca-set-rule`."
  (interactive)
  (ca-setup-automaton))

(define-derived-mode ca-mode special-mode "Cellular Automaton"
  "Major mode for elementary cellular automata."
  (setq-local case-fold-search t)
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil)
  (setq-local mode-line-buffer-identification '("Cellular automaton: Rule "
                                                ca-rule-string))
  (setq-local fill-column (window-width))
  (buffer-disable-undo))

(defun ca-setup-automaton ()
  (switch-to-buffer (get-buffer-create "*Automaton*") t)

  (setq number-generations (- (window-height) 2))

  (erase-buffer)
  (ca-mode)
    
  (let ((inhibit-read-only t))
    (insert-char (ca-black-char) fill-column)
    (insert-char ?\n)
    ;; (message "%d %d" fill-column (point))
    (ca-evolve)
    (goto-char 1)))

(defun ca--calculate-position (num)
  "Gives a value to the not-black characters"
  (lambda (x) (if (eq x (ca-white-char)) num 0)))

(defun ca--evolve-string (str)
  (let ((left-cells
         (mapcar (ca--calculate-position 4) (concat (ca-black-string) str)))
        (center-cells
         (mapcar (ca--calculate-position 2) str))
        (right-cells
         (cdr (mapcar (ca--calculate-position 1) (concat str (ca-black-string))))))
    (let ((new-generation
            (mapcar (lambda (x) (logand ca-rule (ash 1 x)))
                    (mapcar* #'+ left-cells center-cells right-cells))))
      (mapconcat (lambda (x) (if (= x 0) (ca-black-string) (ca-white-string))) new-generation ""))))

(defun ca-evolve ()
  (goto-char (+ fill-column 2))
  (dotimes (_ number-generations)
      (let ((line (buffer-substring
                   (- (point) (1+ fill-column))
                   (- (point) 1))))
        (let ((inhibit-read-only t)
              (new-generation-string (ca--evolve-string line)))
          (if (char-after (point))
              (delete-char (1+ fill-column)))
          (insert new-generation-string)
          (insert-char ?\n)))))

(provide 'life)

;;; ca-mode.el ends here
