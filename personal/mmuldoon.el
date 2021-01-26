(require 'server)
(unless (server-running-p)
  (server-start))

(global-set-key (kbd "C-S-SPC") 'easy-mark)
(global-set-key (kbd "S-<f10>") 'menu-bar-open)
(global-set-key (kbd "S-<f12>") 'menu-bar-mode)
(global-set-key (kbd "s-<backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "M-s-<backspace>") 'kill-line)
(global-set-key (kbd "M-<f5>") 'org-capture)
;; (global-set-key (kbd "M-<f5>") 'org-roam-capture)
;; (global-set-key (kbd "<f5>") 'mmuldoon/org-file)
;; (global-set-key (kbd "M-<f7>") 'org-roam-insert)
;; (global-set-key (kbd "M-<f5>") 'org-roam-insert-immediate)
(global-set-key (kbd "<f5>") 'org-roam-find-file)
(add-to-list 'load-path "~/src/org-fc/")
(prelude-require-packages '(indium ng2-mode clj-refactor discover-clj-refactor org-roam org-download flycheck-clj-kondo org-drill groovy-mode))

(setq prelude-format-on-save t)
(add-hook 'js2-mode-hook #'(lambda() (setq js-indent-level 2)))
(add-hook 'json-mode-hook #'flycheck-mode)
(add-hook 'typescript-mode-hook #'(lambda() (setq typescript-indent-level 2)))
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'markdown-mode-hook #'(lambda() (setq markdown-command "multimarkdown")))
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq user-full-name "mickey muldoon")
(setq user-mail-address "muldoon.mickey@gmail.com")

(setq initial-scratch-message nil)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 1))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(setq org-directory "~/org"
      org-ellipsis " ▼ ")

(setq org-drill-scope 'directory)

;; https://github.com/jrblevin/config/blob/master/.emacs.d/init.el#l1301
;; store backup files in one place.  do the same for auto save files.
(defvar mm-backup-directory (expand-file-name "backups/" user-emacs-directory))
(unless (file-exists-p mm-backup-directory)
  (make-directory mm-backup-directory t))
(setq make-backup-files t
      backup-directory-alist `((".*" . ,mm-backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      vc-make-backup-files t
      kept-old-versions 6
      kept-new-versions 9)
(setq auto-save-default t
      auto-save-file-name-transforms `((".*" ,mm-backup-directory t))
      auto-save-list-file-prefix mm-backup-directory)


;;; export org files into their own directories.
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "exported-org-files")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))

;; remove for now since it gets in the way of plantuml embeds.
;; (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

(setq-default org-download-image-dir "./_resources")
(require 'org-drill)

(setq org-roam-directory (file-truename "~/org"))
(setq org-roam-completion-everywhere t)
(setq org-roam-title-sources '((title) alias))
(setq mmuldoon/agenda-subdirs '("/private-notes/" "/todo/" "/mmuldoon-work/"))
(setq org-agenda-files
      (mapcar #'(lambda(subdir)
                  (concat org-directory subdir)) mmuldoon/agenda-subdirs))
(add-hook 'org-mode-hook #'(lambda () (setq org-default-notes-file (concat org-directory "/todo/inbox.org"))))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)
(setq cider-repl-history-file ".cider-repl-history")


(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)
                             (clojure . t)
                             (js . t)
                             (plantuml . t)))

(add-to-list 'org-structure-template-alist
             '("j" .  "src clojure"))

(setq org-plantuml-jar-path (expand-file-name "/usr/local/cellar/plantuml/1.2020.19/libexec/plantuml.jar"))

(defun +org-search ()
  (interactive)
  (org-refile '(4)))

(defun +mm-clean ()
  (interactive)
  (crux-cleanup-buffer-or-region)
  (fixup-whitespace))

(defun +mm-focus-mode ()
  (interactive)
  (nlinum-mode -1)
  (visual-line-mode 1)
  (whitespace-mode -1))


(add-hook 'writeroom-mode-hook '+mm-focus-mode)

;;; require

(eval-and-compile (require 'org)) ; for `org-completing-read'

;;; code:

(defvar ocv/capture-prmt-history nil
  "history of prompt answers for org capture.")
(defvar ocv/capture-prmt-type-history nil
  "history of prompt answers for org capture.")

(defun ocv/prmt (prompt variable &optional default history)
  "prompt for string, save it to variable and insert it.
optional argument default, a string, is the default input value, if any.
optional argument history, a symbol bound to a completion list (of strings)
to offer.  it defaults to `ocv/capture-prmt-history'."
  (set (make-local-variable variable)
       (let* ((history (or history 'ocv/capture-prmt-history))
              (hist-list (symbol-value history)))
         (org-completing-read
          (concat prompt ": ") hist-list nil nil nil history default))))

(defun mmuldoon/org-file ()
  (interactive)
  (counsel-find-file "~/org/"))

;;; stefan monnier <foo at acm.org>. it is the opposite of fill-paragraph

(defun unfill-paragraph (&optional region)
  "takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; this would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(setq org-capture-templates
      `(("t" "todo" entry (file+headline ,(concat org-directory "/todo/inbox.org") "tasks")
         "* todo %?")
        ("j" "journal" entry (file+datetree ,(concat org-directory "/private-notes/journal.org"))
         "* %?")
        ("m" "meeting" entry (file+datetree ,(concat org-directory "/mmuldoon-work/meetings.org"))
         "* topic: %?\n** type: %(ocv/prmt \"type\" 'type nil 'ocv/capture-prmt-type-history)\n** purpose:\n** present:\n- mickey\n** notes:\n")
        ("p" "private meeting" entry (file+datetree ,(concat org-directory "/private-notes/meetings.org")) "* topic: %?\n** type: %(ocv/prmt \"type\" 'type nil 'ocv/capture-prmt-type-history)\n** purpose:\n** present:\n- mickey\n** notes:\n")
        ;; ("n" "blue note" entry
        ;;  (file (lambda ()(concat org-directory "/blue-notes/" (read-string "filename: "))))
        ;;  "* %?\n %u")
        ))

;;; ("b" "blue note" entry (file ,(mmuldoon/new-org-file (concat org-directory "/blue-notes/"))))

;; (require 'company-org-roam)
(push 'company-capf company-backends)

(require 'org-fc)
(require 'org-fc-hydra)
(global-set-key (kbd "C-c F") 'org-fc-hydra/body)

(setq org-fc-directories '("~/org/aero/"))

(setq org-roam-capture-templates
      `(("d" "default" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "${title}"
         :head "#+TITLE: ${title}"
         :unnarrowed t)))

(setq org-roam-capture-immediate-template
      `("d" "default" plain #'org-roam-capture--get-point
        "%?"
        :file-name "${title}"
        :head "#+TITLE: ${title}"
        :unnarrowed t
        :immediate-finish t))


(setq projectile-project-search-path '("~/Documents/2_Areas/OSCAR/Oscar_Code" "~/Documents/2_Areas/Ochoa/" "~/Documents/2_Areas/Ripley/Ripley_Code" "~/Documents/2_Areas/Dex/Dex_Code" "~/Documents/6_Notes"))

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; C-S-x send defun to rebl
;; C-x C-r send last sexp to rebl (Normally bound to "find-file-read-only"... Who actually uses that though?)
(add-hook 'cider-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
            (local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp)))

(setq sentence-end-double-space nil)
(require 'flycheck-clj-kondo)
