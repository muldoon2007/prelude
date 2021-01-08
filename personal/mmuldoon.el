(require 'server)
(unless (server-running-p)
  (server-start))

(global-set-key (kbd "C-S-SPC") 'easy-mark)
(global-set-key (kbd "S-<f10>") 'menu-bar-open)
(global-set-key (kbd "S-<f12>") 'menu-bar-mode)
(global-set-key (kbd "s-<backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "M-s-<backspace>") 'kill-line)
(global-set-key (kbd "M-<f5>") 'org-capture)
;; (global-set-key (kbd "M-<f7>") '+org-search)
;; (global-set-key (kbd "M-<f5>") 'org-roam-capture)
;; (global-set-key (kbd "<f5>") 'mmuldoon/org-file)
;; (global-set-key (kbd "M-<f7>") 'org-roam-insert)
;; (global-set-key (kbd "M-<f5>") 'org-roam-insert-immediate)
(global-set-key (kbd "<f5>") 'org-roam-find-file)
(prelude-require-packages '(indium ng2-mode clj-refactor discover-clj-refactor org-roam org-download company-org-roam flycheck-clj-kondo org-drill))
(setq prelude-format-on-save t)
(add-hook 'js2-mode-hook #'(lambda() (setq js-indent-level 2)))
(add-hook 'json-mode-hook #'flycheck-mode)
(add-hook 'typescript-mode-hook #'(lambda() (setq typescript-indent-level 2)))
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'markdown-mode-hook #'(lambda() (setq markdown-command "multimarkdown")))
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq user-full-name "Mickey Muldoon")
(setq user-mail-address "muldoon.mickey@gmail.com")

(setq initial-scratch-message nil)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 1))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(setq org-directory "~/org"
      org-ellipsis " ▼ ")

(setq org-drill-scope 'directory)

;; https://github.com/jrblevin/config/blob/master/.emacs.d/init.el#L1301
;; Store backup files in one place.  Do the same for auto save files.
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


;;; Export org files into their own directories.
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "exported-org-files")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))

;; Remove for now since it gets in the way of plantuml embeds.
;; (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

(setq-default org-download-image-dir "./_resources")
(require 'org-drill)

(setq org-roam-directory "~/org")
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

(setq org-plantuml-jar-path (expand-file-name "/usr/local/Cellar/plantuml/1.2020.19/libexec/plantuml.jar"))

(defun +org-search ()
  (interactive)
  (org-refile '(4)))

(defun +mm-clean ()
  (interactive)
  (crux-cleanup-buffer-or-region)
  (fixup-whitespace))

;;; Require

(eval-and-compile (require 'org)) ; for `org-completing-read'

;;; Code:

(defvar ocv/capture-prmt-history nil
  "History of prompt answers for Org capture.")
(defvar ocv/capture-prmt-type-history nil
  "History of prompt answers for Org capture.")

(defun ocv/prmt (prompt variable &optional default history)
  "PROMPT for string, save it to VARIABLE and insert it.
Optional argument DEFAULT, a string, is the default input value, if any.
Optional argument HISTORY, a symbol bound to a completion list (of strings)
to offer.  It defaults to `ocv/capture-prmt-history'."
  (set (make-local-variable variable)
       (let* ((history (or history 'ocv/capture-prmt-history))
              (hist-list (symbol-value history)))
         (org-completing-read
          (concat prompt ": ") hist-list nil nil nil history default))))

(defun mmuldoon/org-file ()
  (interactive)
  (counsel-find-file "~/org/"))

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-directory "/todo/inbox.org") "Tasks")
         "* TODO %?")
        ("j" "Journal" entry (file+datetree ,(concat org-directory "/private-notes/journal.org"))
         "* %?")
        ("m" "Meeting" entry (file+datetree ,(concat org-directory "/mmuldoon-work/meetings.org"))
         "* Topic: %?\n** Type: %(ocv/prmt \"Type\" 'type nil 'ocv/capture-prmt-type-history)\n** Purpose:\n** Present:\n- Mickey\n** Notes:\n")
        ("p" "Private meeting" entry (file+datetree ,(concat org-directory "/private-notes/meetings.org")) "* Topic: %?\n** Type: %(ocv/prmt \"Type\" 'type nil 'ocv/capture-prmt-type-history)\n** Purpose:\n** Present:\n- Mickey\n** Notes:\n")
        ;; ("n" "Blue Note" entry
        ;;  (file (lambda ()(concat org-directory "/blue-notes/" (read-string "Filename: "))))
        ;;  "* %?\n %U")
        ))

;;; ("b" "Blue note" entry (file ,(mmuldoon/new-org-file (concat org-directory "/blue-notes/"))))

(require 'company-org-roam)
(push 'company-org-roam company-backends)

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
(setq sentence-end-double-space nil)
(require 'flycheck-clj-kondo)
