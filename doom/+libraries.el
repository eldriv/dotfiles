;;;; $DOOM/libraries.el -*- lexical-binding: t; -*-

;; Sly MREPL Mode Hook
(defun my-sly-mrepl-mode-hook ()
  (smartparens-mode 1)
  (define-key sly-mrepl-mode-map (kbd "<tab>") 'company-complete)
  (define-key sly-mrepl-mode-map (kbd "TAB") 'company-complete))

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(after! org
  (setq org-directory "~/org/"
        org-agenda-files '("~/org")
        org-capture-templates
        '(("s" "Snippet" entry
           (file+headline "n.org" "Captured Items")
           "* Note No. %^{}  \n")))

  (setq deft-directory "~/org"
        deft-extensions '("org" "txt" "md")
        deft-recursive nil)
  
  (add-hook 'org-mode-hook 'org-modern-mode)
  
  (setq org-modern-fold-stars
        '(("▶" . "▼") ("▷" . "▽") ("▸" . "▾") ("▹" . "▿"))))

(after! smartparens
  (add-hook 'lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'sly-mrepl-mode-hook 'my-sly-mrepl-mode-hook))

(after! page-break-lines
  (global-page-break-lines-mode 1))

(after! vterm
  (add-to-list 'vterm-eval-cmds '("dired" dired))
  (setq vterm-max-scrollback 1000000)
  (map! :map vterm-mode-map
        :i "<tab>" 'vterm-send-tab
        :i "TAB"   'vterm-send-tab))

(after! vterm-toggle
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(after! popup
  (set-popup-rules!
    '(("^ \\*" :slot -1)
      ("^\\*" :select t)
      ("^\\*Completions" :slot -1 :ttl 0)
      ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
      ("^\\*Help" :slot -1 :size 0.25 :select t)
      ("^\\*sly-mrepl\*" :slot -1 :select t :size 0.5 :side right)
      ("^\\*vterm\*" :slot -1 :select t :size 0.5 :side bottom)
      ("^\\*sly-mrepl\*" :slot -1 :select t :size 0.5 :side bottom)
      ("^\\*doom:" :size 0.35 :select t :modeline t :quit t :ttl t))))


(after! backup-each-save
  (push 'backup-each-save after-save-hook))

