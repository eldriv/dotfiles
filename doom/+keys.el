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

(defun bind-company-keys ()
  "Bind the company keys."
  (map-company-keys global-map))

(defun bind-toplevel-keys ()
  "Bind the toplevel keys."
  ;; general
  (map! 
   :g "s-b" 'switch-to-buffer
   :g "s-d" 'evil-insert-digraph
   :g "s-." 'doom/reload
   :g "s-`" 'evil-switch-to-windows-last-buffer
   :g "s-h" 'replace-regexp
   :g "s-H" 'replace-string
   :g "s-a" 'org-agenda
   :g "s-n" 'org-capture
   :g "s-p" '+treemacs/toggle
   :g "s-u" 'undo-fu-only-undo
   :g "s-r" 'undo-fu-only-redo)
  ;; workspaces
  (map! 
   :g "s-D" '+workspace/delete
   :g "s-o" '+workspace/other
   :g "s-y" '+workspace/display
   :g "s-z" '+workspace/other
   :g "s-," '+workspace/rename
   :g "s-[" '+workspace/switch-left
   :g "s-]" '+workspace/switch-right
   :g "s-{" '+workspace/swap-left
   :g "s-}" '+workspace/swap-right)
  ;; windows
  (map! 
   :n "s-<up>" 'swap-up
   :n "s-<down>" 'swap-down)
  ;; main
  (map! 
   :n "(" 'sp-down-sexp
   :n ")" 'sp-up-sexp
   :n "{" 'sp-backward-up-sexp
   :n "}" 'sp-backward-down-sexp))

(defun bind-main-keys ()
  "Bind the main leader keys."
  (map! :leader
        :desc "Execute extended command"  "<SPC>" 'execute-extended-command
        :desc "Maximize window"           "w m" 'doom/window-maximize-buffer)
  (map! :leader
        :desc "Switch to buffer"          "b b" 'consult-buffer
        :desc "Buffer menu"               "b B" 'buffer-menu
        :desc "Find function"             "h C-f" 'find-function
        :desc "Find variable"             "h C-v" 'find-
        :desc "Wrap with ~"               "w d" 'wrapped-with-tilde))

(defun bind-z-keys ()
  "Bind z-prefix keys."
  (map! :leader
        (:prefix-map
         ("z" . "zzz")
         (:prefix-map ("c" . "clear")
          :desc "Clear buffer"             "b" 'clear-buffer
          :desc "Clear output"             "o" 'clear-output)
         (:prefix-map ("e" . "export")
          :desc "Org export as HTML"        "h" 'org-html-export-to-html
          :desc "Org Pandoc export as HTML" "H" 'org-pandoc-export-as-html5
          :desc "Org export as PDF"         "p" 'org-latex-export-to-pdf)
         (:prefix-map ("t" . "theme")
          :desc "Switch theme"             "t" 'switch-theme))))

;;; toplevel

(bind-workspace-keys)
(bind-company-keys)
(bind-toplevel-keys)
(bind-main-keys)
(bind-z-keys)
