;;;; $DOOMDIR/definitions.el -*- lexical-binding: t; -*-

(defvar alias-table
  '((yes-or-no-p . y-or-n-p)
    (ef  . expand-file-name)
    (rb  . revert-buffer)
    (cc  . concat)
    (bb  . bury-buffer)
    (bm  . buffer-menu)
    (ro  . read-only-mode)
    (ow  . overwrite-mode)
    (rs  . replace-string)
    (rr  . replace-regexp)
    (qr  . query-replace)
    (qrr . query-replace-regexp)
    (af  . auto-fill-mode)
    (ci  . call-interactively)
    (dc  . delete-char)
    (dr  . delete-region)
    (ib  . ispell-buffer)
    (id  . ispell-change-dictionary)
    (ap  . find-file-at-point)
    (tl  . transpose-lines)
    (rf  . rename-file)
    (fa  . find-alternate-file)
    (tr  . table-recognize)
    (tu  . table-unrecognize)
    (tir . table-insert-row)
    (tdr . table-delete-row)
    (dcr . downcase-region)
    (ucr . upcase-region)
    (ccr . capitalize-region)
    (bod . beginning-of-defun)
    (eod . end-of-defun)
    (pi  . package-install)
    (pl  . package-list-packages)
    (pr  . package-refresh-contents)

    (clhs . hyperspec-lookup)))

(defmacro alias (alias fun)
  `(defalias ',alias ',fun))

(defun def-aliases (table)
  "Define the aliases."
  (cl-loop for (key . val) in table do
           (defalias key val)))

(def-aliases alias-table)
(cl-defmacro defcmd (name (&rest args) docstring &rest body)
  `(defun ,name (,@args)
     ,docstring
     (interactive)
     ,@body))

(defcmd swap-windows (direction)
        "Swap windows to direction"
        (let ((win-list (window-list)))
          (when (>= (length win-list) 2)
            (let* ((window-1 (cl-first win-list))
                   (window-2 (cl-ecase direction
                               ((up left) (cl-first (last win-list)))
                               ((down right) (cl-second win-list))))
                   (buffer-1 (window-buffer window-1))
                   (buffer-2 (window-buffer window-2))
                   (start-1 (window-start window-1))
                   (start-2 (window-start window-2))
                   (point-1 (window-point window-1))
                   (point-2 (window-point window-2)))
              (set-window-buffer window-1 buffer-2)
              (set-window-buffer window-2 buffer-1)
              (set-window-start window-1 start-2)
              (set-window-start window-2 start-1)
              (set-window-point window-1 point-1)
              (set-window-point window-2 point-2)
              (other-window (cl-ecase direction
                              ((up) -1)
                              ((down) 1)))))))

(defcmd swap-up ()
        "Swap windows up"
        (swap-windows 'up))

(defcmd swap-down ()
        "Swap windows down"
        (swap-windows 'down))

(defun other-window-1 (&optional arg)
  "Switch to other window, backwards."
  (interactive "p")
  (other-window (- arg)))

(defcmd clear-buffer ()
        "Clear the current buffer according to the major mode."
        (cl-case major-mode
          (slime-repl-mode (ci 'slime-repl-clear-buffer))
          (sly-repl-mode (ci 'sly-repl-clear-buffer))))

(defcmd clear-output ()
        "Clear the output according to the major mode."
        (cl-case major-mode
          (slime-repl-mode (ci 'slime-repl-clear-output))
          (sly-repl-mode (ci 'sly-repl-clear-output))))

(defun switch-theme (theme)
  "Show a completing prompt for changing the theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))


;;; Wrapped word with (~) prior skipping letter, numbers and special-characters

(defun s ()
  "Get the starting position for wrapping text.."
  (if (use-region-p)
      (region-beginning)
    (save-excursion
      (skip-chars-backward "[:word:][:punct:]")
      (point))))

(defun e ()
  "Get the ending position for wrapping text.."
  (if (use-region-p)
      (region-end)
    (save-excursion
      (skip-chars-forward "[:word:][:punct:]")
      (point))))

(defcmd wrapped-with-tilde ()
        "Wrap selected region or word at point with ~ for ORG document."
        (interactive)
        (let ((start (s))
              (end (e)))
          (save-excursion
            (goto-char end)
            (insert "~")
            (goto-char start)
            (insert "~"))
          (goto-char (+ end 2))))
