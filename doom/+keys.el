;;;; $DOOM/keys.el -*- lexical-binding: t; -*-


(defmacro map-workspace-keys (map)
  "Map the workspace switching keys in MAP."
  `(map! :map ,map
         :leader "1" '+workspace/switch-to-0
         :leader "2" '+workspace/switch-to-1
         :leader "3" '+workspace/switch-to-2
         :leader "4" '+workspace/switch-to-3
         :leader "5" '+workspace/switch-to-4
         :leader "6" '+workspace/switch-to-5
         :leader "7" '+workspace/switch-to-6
         :leader "8" '+workspace/switch-to-7
         :leader "9" '+workspace/switch-to-8
         :leader "0" '+workspace/switch-to-final))

(defmacro map-company-keys (map)
  "Map tab to company-complete in MAP."
  `(map! :map ,map
         :i "<tab>" 'company-complete
         :i "TAB"   'company-complete))

(defun bind-workspace-keys ()
  "Bind the workspace keys."
  (map-workspace-keys global-map)
  (map-workspace-keys vterm-mode-map)
  (map-workspace-keys sly-mrepl-mode-map)
  (map-workspace-keys treemacs-mode-map))


(defun bind-toplevel-keys ()
  "Bind the toplevel keys."
  ;; General keybindings
  (map!
   :leader
   :desc "Execute extended command"   "<SPC>" 'execute-extended-command
   :desc "Switch to buffer"           "d b" 'switch-to-buffer
   :desc "Insert digraph"            "d d" 'evil-insert-digraph
   :desc "Reload Doom"                "d ." 'doom/reload
   :desc "Switch to last buffer"     "d `" 'evil-switch-to-windows-last-buffer
   :desc "Replace regexp"            "d h" 'replace-regexp
   :desc "Replace string"            "d H" 'replace-string
   :desc "Org agenda"                "d a" 'org-agenda
   :desc "Org capture"               "d n" 'org-capture
   :desc "Undo"                      "d u" 'undo-fu-only-undo
   :desc "Redo"                      "d r" 'undo-fu-only-redo)

  ;; Workspace management keybindings
  (map!
   :leader
   :desc "Delete workspace"          "d D" '+workspace/delete
   :desc "Switch to other workspace" "d <right>" '+workspace/other
   :desc "Display workspace"         "d y" '+workspace/display
   :desc "Switch to other workspace" "d z" '+workspace/other
   :desc "Rename workspace"          "d ," '+workspace/rename
   :desc "Switch to previous workspace" "d [" '+workspace/switch-left
   :desc "Switch to next workspace"  "d ]"'+workspace/switch-right
   :desc "Swap with previous workspace" "d {" '+workspace/swap-left
   :desc "Swap with next workspace"  "d }" '+workspace/swap-right)

  ;; Window management keybindings
  (map!
   :leader
   :desc "Swap window up"           "d <up>"   'swap-up
   :desc "Swap window down"         "d <down>" 'swap-down
   :desc "Move active window right" "d p"      'evil-window-right
   :desc "Split window"             "d 0"      'evil-window-vsplit)
  ;; Mark
  (map!
   :leader
   :desc "Mark sexp"                 "v"       'mark-sexp
   :desc "Mark defun"                "V"       'mark-defun
   :desc "Backward-up"               "{"       'sp-backward-up-sexp
   :desc "backward-down"             "}"       'sp-backward-down-sexp))

(defun bind-main-keys ()
  "Bind the main leader keys."
  (map!
   :leader
   :desc "Execute extended command"  "<SPC>" 'execute-extended-command
   :desc "Maximize window"           "w m" 'doom/window-maximize-buffer)
  (map!
   :leader
   :desc "Switch to buffer"          "b b" 'consult-buffer
   :desc "Buffer menu"               "b B" 'buffer-menu
   :desc "Find function"             "h C-f" 'find-function
   :desc "Find variable"             "h C-v" 'find-variable
   :desc "Wrap with ~"               "w d" 'wrapped-with-tilde
   :desc "Switch to the SLY"         "w a" 'my-switch-to-sly-mrepl-window
   :desc "Switch to other window"    "w e" 'my-other-window)
  (map!
   :leader
   :desc "Clear SLY MREPL" "c p" 'sly-mrepl-clear-repl))


(defun bind-y-keys ()
  "Bind y-prefix keys."
  (map! :leader
        (:prefix-map ("y" . "smartparens")
         :desc "Forward kill sexp"       "k" 'sp-kill-sexp
         :desc "Backward kill sexp"      "K" 'sp-backward-kill-sexp
         :desc "Forward change sexp"     "c" 'change-sexp
         :desc "Backward change sexp"    "C" 'backward-change-sexp
         :desc "Unwrap sexp"             "u" 'sp-unwrap-sexp
         :desc "Forward slurp sexp"      "s" 'sp-forward-slurp-sexp
         :desc "Backward slurp sexp"     "S" 'sp-backward-slurp-sexp
         :desc "Forward barf sexp"       "b" 'sp-forward-barf-sexp
         :desc "Backward barf sexp"      "B" 'sp-backward-barf-sexp
         :desc "Absorb sexp"             "a" 'sp-absorb-sexp
         :desc "Convolute sexp"          "v" 'sp-convolute-sexp
         :desc "Wrap with ()"            "(" 'wrap-with-parens
         :desc "Wrap with []"            "[" 'wrap-with-brackets
         :desc "Wrap with {}"            "{" 'wrap-with-braces
         :desc "Wrap with ''"            "'" 'wrap-with-single-quotes
         :desc "Wrap with \"\""          "\"" 'wrap-with-double-quotes
         :desc "Wrap with = ="           "=" 'wrap-with-equals
         :desc "Wrap with ~ ~"           "~" 'wrapped-with-tilde)))

(defun bind-z-keys ()
  "Bind z-prefix keys."
  (map!
   :leader
   :prefix ("z" . "zzz")
   :desc "Clear buffer"              "c b" 'clear-buffer
   :desc "Clear output"              "c o" 'clear-output
   :prefix ("e" . "export")
   :desc "Org export as HTML"        "e h" 'org-html-export-to-html
   :desc "Org Pandoc export as HTML" "e H" 'org-pandoc-export-as-html5
   :desc "Org export as PDF"         "e p" 'org-latex-export-to-pdf
   :prefix ("t" . "theme")
   :desc "Switch theme"              "t t" 'switch-theme))

;; toplevel

(bind-workspace-keys)
(bind-company-keys)
(bind-toplevel-keys)
(bind-main-keys)
(bind-y-keys)
(bind-z-keys)
