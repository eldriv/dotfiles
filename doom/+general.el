;;; $DOOM/general.el -*- lexical-binding: t; -*-

(defun set-modes (modes)
  "Set multiple modes with key-value pairs."
  (cl-loop for (key . val) in modes
           when (fboundp key) do (funcall key val)))

(defun setup-ui ()
  "Setup UI options."
  (set-modes '((menu-bar-mode . -1)
               (tool-bar-mode . -1)
               (scroll-bar-mode . -1)
               (line-number-mode . -1)
               (horizontal-scroll-bar-mode . -1)
               (column-number-mode . 1)
               (size-indication-mode . 1)))
  (unless window-system
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line)))

(defun nyc-runemacs/full-screen-setup ()
  "Make Emacs full-screen and adjust UI elements accordingly."
  (interactive)
  (if (fboundp 'toggle-frame-fullscreen)
      (toggle-frame-fullscreen)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun setup-theme ()
  "Set theme for Doom"
  (setq doom-theme 'doom-manegarm
        +evil-want-o/O-to-continue-comments nil
        fancy-splash-image "~/img/img.png"
        display-line-numbers-type t
        evil-move-cursor-back t
        auto-save-default t
        confirm-kill-emacs nil
        version-control t))

(defun setup-hooks ()
  "Setup the common hooks."
  (add-hook 'lisp-mode-hook 'turn-on-auto-fill)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
  (add-hook 'after-save-hook 'backup-each-save))

(defun alist-keys (alist)
  (mapcar 'car alist))

(defvar my-custom-digraphs
  '(((?p ?p) . ?\x20B1)                     ; ₱ (Philippine Peso Sign)
    ((?a ?a) . ?\x2227) ((?& ?&) . ?\x2227) ; ∧ (Logical AND)
    ((?o ?o) . ?\x2228) ((?| ?|) . ?\x2228) ; ∨ (Logical OR)
    ((?n ?n) . ?\x00AC) ((?! ?!) . ?\x00AC) ; ¬ (Negation)
    )
  "Association list of my custom digraphs.")

(defun setup-digraphs ()
  "Remove my custom digraphs from the existing digraphs table."
  (let ((keys (alist-keys my-custom-digraphs)))
    (cl-loop for key in keys
             do (setq evil-digraphs-table
                      (assoc-delete-all key evil-digraphs-table)))
    (setq evil-digraphs-table
          (append evil-digraphs-table
                  my-custom-digraphs))))

(defun nyc-setup-pretty-symbols ()
  "Set up pretty symbols globally with common programming symbols."
  (global-prettify-symbols-mode 1)
  (setq prettify-symbols-alist
        '(("lambda" . ?λ))))

(add-hook 'after-init-hook #'nyc-setup-pretty-symbols)

;; Now setup everything
(setup-ui)
(setup-theme)
(setup-hooks)
(require 'evil-digraphs)
(setup-digraphs)
(nyc-runemacs/full-screen-setup)
(nyc-setup-pretty-symbols)
(require 'org-download)
