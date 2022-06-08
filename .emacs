
;; ---------
;; PACKAGE
;; ---------

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; install use-package and requires it
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-dired all-the-icons rainbow-delimiters company session helm-ag jedi csv-mode magit restclient-helm helm-git-grep helm-ls-git)))

;; ---------
;; Basic
;; ---------

;; La fleche de direction vers le bas ne doit pas etendre le fichier
;; en fin de tampon (seul un retour chariot explicite le fait).
(setq next-line-add-newlines nil)

;; numero de lignes et de colonnes
(column-number-mode t)
(line-number-mode t)

;; nom du fichier a la place de emacs@host
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; la completion respecte la casse
(setq dabbrev-case-replace nil)

;; Remplace les 'yes or no' par des 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; store all backup and autosave files in the tmp dir
(defvar emacs-backup-directory (concat "~/.emacs.d/backup/"))
(setq backup-directory-alist
      `((".*" . ,emacs-backup-directory)))

(defvar emacs-autosave-directory (concat "~/.emacs.d/autosave"))
(setq auto-save-file-name-transforms
      `((".*", emacs-autosave-directory t)))

;; ajout d'un repertoire local de fichiers lisp
(setq my-lisp-directory (expand-file-name "~/.emacs.d/site-lisp/"))
(add-to-list 'load-path my-lisp-directory)

;; disable lock files for concurrent editing : cannot be moved to another folder
(setq create-lockfiles nil)

;; Lorsqu'on saisit un texte alors qu'une zone est selectionnee, cette
;; derniere est ecrasee par le texte saisi.
(delete-selection-mode 1)

(tool-bar-mode -1)
(setq visible-bell 1)

;; ---------
;; UI
;; ---------

;; apparence d'emacs
(setq default-frame-alist
      (append default-frame-alist
              '((foreground-color . "white")
                (background-color . "gray10")
                (cursor-color . "firebrick"))))


;; windows font
;;(set-frame-font "-outline-Courier New-normal-r-normal-normal-11-*-96-96-c-*-iso8859-1")
;;(set-frame-font "-outline-Consolas-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1")
(add-to-list 'default-frame-alist '(font . "-outline-Consolas-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1" ))
(set-face-attribute 'default t :font "-outline-Consolas-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1" )

;; max font lock settings 
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; display current line with a slighly different font 
(require 'hl-line)
(set-face-attribute 'hl-line nil :inherit nil :background "gray20")
(global-hl-line-mode 1)

;; transparency
(defun transparency (alpha-level)
  (interactive "p")
  (message (format "Alpha level passed in: %s" alpha-level))
  (let ((alpha-level (if (< alpha-level 2)
			 (read-number "Opacity percentage: " 85)
		       alpha-level))
	(myalpha (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha alpha-level))
  (message (format "Alpha level is %d" (frame-parameter nil 'alpha))))

;; ---------
;; Parentheses handling
;; ---------

(require 'paren)
(show-paren-mode t)
(setq blink-matching-paren t)
(setq blink-matching-paren-on-screen t)

;; rainbow-delimiters : parentheses highlight
(dolist (package '(rainbow-delimiters))
  (unless (package-installed-p package)
    (package-install package)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(require 'cl-lib)
(require 'color)

(set-face-attribute 'rainbow-delimiters-depth-1-face nil
		    :foreground "orange"
                    :inherit 'rainbow-delimiters-base-face)

(set-face-attribute 'rainbow-delimiters-depth-2-face nil
		    :foreground "slate blue"
                    :inherit 'rainbow-delimiters-base-face)

(set-face-attribute 'rainbow-delimiters-depth-6-face nil
		    :foreground "MediumPurple1"
                    :inherit 'rainbow-delimiters-base-face)

;; stronger colors from rainbow-delimiters from : https://yoo2080.wordpress.com/2013/09/08/living-with-rainbow-delimiters-mode/
;; (cl-loop
;;  for index from 1 to rainbow-delimiters-max-face-count do
;;  (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
;;    (cl-callf color-saturate-name (face-foreground face) 30)))

;; jump to parentheses pair
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-S-p") 'match-paren)


;; ---------
;; History
;; ---------

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist")
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
	isearch-string
        regexp-search-ring
	helm-M-x-input-history
	helm-grep-history
	helm-grep-ag-history
	helm-ag--command-history
	helm-M-x-input-history
	))

;; ---------
;; End of lines 
;; ---------

;; convertion de formats de fichiers
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t) (replace-match "\n")))

(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; ---------
;; CSV
;; ---------

;; install csv-mode and requires it
(dolist (package '(csv-mode))
  (unless (package-installed-p package)
    (package-install package)))

(use-package csv-mode
  :ensure t
  :mode "\\.csv$"
  :init
  (setq csv-separators '("|")))

;; ---------
;; BOOKMARKS
;; ---------

(setq bookmark-default-file
      (expand-file-name "~/.emacs.d/bookmarks/emacs.bmk"))

;; ---------
;; Completion (company-mode)
;; ---------

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

;; -------
;; DIRED
;; -------

(require 'dired)   ;; available with default emacs install 
(require 'dired-x) ;; available with default emacs install 

(setq  dired-sort-menu-saved-config
       (quote ((dired-actual-switches . "-lah")
	       (ls-lisp-ignore-case . t)
	       (ls-lisp-dirs-first . t))))

;; dired icons
(dolist (package '(all-the-icons all-the-icons-dired))
  (unless (package-installed-p package)
    (package-install package)))

(defvar emacs-font-directory (concat "~/.emacs.d/fonts/"))
(if (file-directory-p emacs-font-directory) nil
  (progn
    (make-directory emacs-font-directory)
    (all-the-icons-install-fonts)))
;;; this will download required fonts, but still need to install them manually on windows (double click)

(when (display-graphic-p)
  (require 'all-the-icons)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files)) 
            (buffer-file-name (nth 2 marked-files)))))


(define-key dired-mode-map "="	'dired-ediff-marked-files)

(define-key dired-mode-map "p"	'dired-previous-line)
(define-key dired-mode-map "n"	'dired-next-line)
(define-key dired-mode-map "\C-n"  'dired-next-marked-file)
(define-key dired-mode-map "\C-p"  'dired-prev-marked-file)

(define-key dired-mode-map (kbd "<M-up>") 'dired-up-directory)
;;(define-key dired-mode-map (kbd "<M-down>") 'dired-tree-down)

(defun dired-for-each-marked-file(fn)
  "Do stuff for each marked file, only works in dired window"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((filenames (dired-get-marked-files)))
	(mapcar fn filenames))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))

;; integration with w32
(defun w32-browser (doc)
  "Browse to a particular file/URL using default web browser"
  (w32-shell-execute 1 doc))

(defun dired-w32-open(file)
  (interactive)
  (w32-browser (dired-replace-in-string "/" "\\" file)))

(defun dired-w32-open-file-at-point()
  (interactive)
  (dired-w32-open (dired-get-filename)))

(defun dired-w32-open-each-marked-file()
  (interactive)
  (dired-for-each-marked-file 'dired-w32-open))

(define-key dired-mode-map [f3] 'dired-w32-open-file-at-point)
(define-key dired-mode-map [(shift f3)] 'dired-w32-open-each-marked-file)

(define-key dired-mode-map "F" 'dired-do-find-marked-files)

(defun dired-do-zipfile (zipfile)
  (interactive "szip file name: ")
  (dired-do-shell-command (format "zip -r %s *" zipfile)
			  nil
			  (dired-get-marked-files t)))
(define-key dired-mode-map "Z"	'dired-do-zipfile)

(setq ls-lisp-ignore-case t)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies  'always)
(setq dired-listing-switches "-ahl")

;; autorize "a" shortcut on dired (open destination in current buffer)
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
	  (lambda ()
            (dired-hide-details-mode)))


;; ---------------------
;; TRAMP
;; ---------------------

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq password-cache-expiry nil)

;; ---------------------
;; FIND / GREP
;; ---------------------
(require 'find-dired)

(global-set-key "\C-x\ f" (quote find-name-dired))

(setq find-program "gfind.exe")
;; pattern for find
(setq find-ls-option (quote ("-exec ls -ld {} \;" . "-ld")))

;; pattern for recursive grep
;;   <D> for the base directory
;;   <X> for the find options to restrict directory list
;;   <F> for the find options to limit the files matched
;;   <C> for the place to put -i if the search is case-insensitive
;;   <R> for the regular expression to search for
(global-set-key "\C-x\ g" (quote rgrep))
(setq grep-find-ignored-directories (quote ("CVS" ".git" ".svn" "classes" "build" "target" ".metadata" "pristine")))
(setq grep-find-template "gfind . <X> -type f <F> -exec grep -a <C> -nH -e <R> {} \";\" ")

;; -type f -regex ".*.\(js\|xml\|ts\|java\|sql\)" -and -not -path "*bower*" -and -not -path "*target*" 


;; ----------
;; TABBAR
;; ----------
;;(require 'cl) ;; remove-if est dans cl
;;(when (require 'tabbar nil t)
;;  (setq tabbar-buffer-groups-function
;; 		(lambda (b) (list "All Buffers")))
;;  (setq tabbar-buffer-list-function
;; 		(lambda ()
;; 		  (remove-if
;; 		   (lambda(buffer)
;; 			 (find (aref (buffer-name buffer) 0) " *"))
;; 		   (buffer-list))))
;;  (tabbar-mode)) ;; affiche tous les buffers sauf *

;;(global-set-key [(control tab)] (quote dabbrev-expand))
;;(global-set-key (kbd "<C-next>") (quote tabbar-forward))
;;(global-set-key (kbd "<C-prior>")(quote tabbar-backward))


;; ----------
;; SHORTCUTS
;; ----------
(cua-mode nil)
(global-set-key [(control meta o)] 'find-file)

(global-set-key "\M-l" (quote goto-line))
(global-set-key "\C-x\ a" (quote mark-whole-buffer))

;; screen split
(global-set-key [f4] (quote kill-this-buffer))
(global-set-key [f5] (quote delete-window)) ;; ferme la fenetre f5
(global-set-key [f6] (quote other-window)) ;; rotation du cuseur			  f6
(global-set-key [f7] (quote split-window-vertically)) ;; coupe en vertical	 f7
(global-set-key [f8] (quote split-window-horizontally))	;; coupe en horizontal f8

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)


;; always exit searches at the beginning of the expression found
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

;; macro pour toutes les lignes d'une région
(global-set-key "\C-ce" 'apply-macro-to-region-lines)

;; uniquify & sort-lines
(defun uniquify-region ()
  "remove duplicate adjacent lines in the given region"
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (beginning-of-buffer)
  (while (re-search-forward "\\(.*\n\\)\\1+" nil t)
    (replace-match "\\1" nil nil))
  (widen)
  nil)

;; Rectangles / Column mode
;; Copy rectangle : C-x r r r ==> C-x * 2 while selecting in CUA + r (rectangle) + r (put to register) + r (name of the register)
;; Insert rectangle : C-x r i r ==> C-x * 2 while selecting in CUA + r (rectangle) + i (insert from register) + r (name of the register)

;; Kill ring
;; C-y (or C-v in CUA) + M-y : paste old value from kill-ring
;; browse-kill-ring : visit and edit the kill ring


;; ----------
;; INDENTATION
;; ----------

;; ;; START TABS CONFIG
;; ;; Create a variable for our preferred tab width
;; (setq custom-tab-width 2)
;;  
;; ;; Two callable functions for enabling/disabling tabs in Emacs
;; (defun disable-tabs () (setq indent-tabs-mode nil))
;; (defun enable-tabs  ()
;;   (local-set-key (kbd "TAB") 'tab-to-tab-stop)
;;   (setq indent-tabs-mode t)
;;   (setq tab-width custom-tab-width))
;;  
;; ;; Hooks to Enable Tabs
;; (add-hook 'prog-mode-hook 'enable-tabs)
;; ;; Hooks to Disable Tabs
;; (add-hook 'lisp-mode-hook 'disable-tabs)
;; (add-hook 'emacs-lisp-mode-hook 'disable-tabs)
;;  
;; ;; Language-Specific Tweaks
;; (setq-default js-indent-level custom-tab-width)      ;; Javascript
;;  
;; ;; Making electric-indent behave sanely
;; (setq-default electric-indent-inhibit t)
;;  
;; ;; Make the backspace properly erase the tab instead of
;; ;; removing 1 space at a time.
;; (setq backward-delete-char-untabify-method 'hungry)
;;  
;; ;; WARNING: This will change your life
;; ;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; ;; This will also show trailing characters as they are useful to spot.
;; ;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (setq whitespace-style '(face tabs tab-mark trailing))
;;  
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(whitespace-tab ((t (:foreground "#636363")))))
;; (setq whitespace-display-mappings
;;       '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;; ;;(global-whitespace-mode) ; Enable whitespace mode everywhere
;; ;; END TABS CONFIG


;; ---------------
;; Lisp
;; ---------------

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


;; ---------------
;; Powershell
;; ---------------

;; install powershell 
(dolist (package '(powershell))
  (unless (package-installed-p package)
    (package-install package)))

;; associate to .ps1 files
(use-package powershell
  :mode ("\\.ps1" . powershell-mode)
  :ensure t)

;; ---------
;; XML
;; ---------

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|xul\\|svg\\|rptdesign\\)\\'" . nxml-mode)
	    auto-mode-alist))
(setq nxml-auto-insert-xml-declaration-flag t)
(setq nxml-slash-auto-complete-flag t)


(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)  ;;  replace whitespace 
      (backward-char) (insert "\n") (setq end (1+ end)))
    (while (search-forward-regexp "\>\<" nil t)  ;; break tags not separated
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "XML pretty print done!"))

;; (use-package xml-format
;;   :demand t
;;   :after nxml-mode)

;; ------------------
;; SQL
;; ------------------
;; (setq sql-product (quote oracle))

;; (require 'sqlplus)
;; (add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))

;; (setq sqlplus-save-passwords t)
;; (add-hook 'sqlplus-mode-hook (lambda () (easy-menu-add-item nil nil sqlplus-connections-menu t)))

;; ------------------
;; eshell
;; ------------------
(require 'pcomplete)

;; complete as much as possible, and then wait for the next charater
(setq eshell-cmpl-cycle-completions nil)

(setq eshell-save-history-on-exit t)
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 128)

(setq eshell-buffer-name "eshell")
(setq eshell-directory-name "~\\.emacs.d\\eshell\\")

(defun eshell/clear ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
    ;; simply delete the region
    (delete-region (point-min) (point-max))))
(defalias 'eshell/cls 'eshell/clear)

(add-hook 'eshell-mode-hook
	  '(lambda () (local-set-key (kbd "C-l") '(lambda ()
						    (interactive)
						    (eshell/clear)
						    (eshell-send-input)))))

(defun eshell-ctrl-a (prompt &optional alt-bol-fcn)
  ""
  (interactive)
  (if (and (string-match (concat "^" (regexp-quote prompt)
				 " *$")
			 (buffer-substring-no-properties
			  (line-beginning-position)
			  (point)))
	   (not (bolp)))
      (beginning-of-line)
    (if alt-bol-fcn
	(funcall alt-bol-fcn)
      (beginning-of-line)
      (search-forward-regexp prompt))))

(defun eshell-hook-ctrl-a()
  ""
  (interactive)
  (eshell-ctrl-a (funcall eshell-prompt-function)))

(defalias 'eshell/emacs 'find-file)
(defun eshell/dired ()
  (dired (eshell/pwd)))
(defun eshell/open (file)
  (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name file))) nil)

(defun eshell/less (file)
  "Pager view of FILE."
  (view-file file)  0)
(defalias 'eshell/more 'eshell/less)


;; ------------------
;; HELM-MODE
;; ------------------

;; install helm package which are going to be required
(dolist (package '(helm helm-swoop helm-projectile helm-ls-git helm-git-grep))
  (unless (package-installed-p package)
    (package-install package)))

(require 'helm)

(with-eval-after-load 'helm
  (setq helm-always-two-windows nil)
  (setq helm-display-buffer-default-height 20)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window)))

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ;; make TAB works in terminal, C-i is the same as TAB
(define-key helm-map (kbd "S-<tab>") 'helm-select-action)

;; Buffer list + bookmarks + recent files under Helm
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-bookmarks
                                  helm-source-recentf))
(global-set-key "\C-x\C-b" (quote helm-mini))

;; helm M-x replacement
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-f") nil) ;; Remove the old keybinding
(global-set-key (kbd "C-f h") 'helm-resume)

;; helm search content of buffers
(global-set-key (kbd "C-f s") 'helm-multi-swoop-all)

;; helm search repo with git grep
(global-set-key (kbd "C-f g") 'helm-projectile-grep) ;; use projectile Git grep as default (able to grep whole project sources, not only subdirectories from where helm-git-grep is launched)
(setq projectile-use-git-grep t)
;;(global-set-key (kbd "C-f g") 'helm-grep-do-git-grep)
;;(define-key isearch-mode-map (kbd "C-f g") 'helm-git-grep-from-isearch)

(setq helm-git-grep-candidate-number-limit 100)
(setq helm-input-idle-delay 0.5)
(setq helm-idle-delay 1) ;; 1 second before launching any external command (git grep chokes otherwise)

;; helm ag 
(global-set-key (kbd "C-f a") 'helm-ag) ;; use ag for text and log folders searches

;; helm git ls
(add-to-list 'helm-sources-using-default-as-input 'helm-source-ls-git)
(global-set-key (kbd "C-f l") 'helm-ls-git-ls)

;; helm bookmarks
(global-set-key (kbd "C-f b") 'helm-bookmarks)

;; helm google, because why not 
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(global-set-key (kbd "C-f w") 'helm-google-suggest)

;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; ------------------
;; ORG-MODE
;; ------------------
(setq org-export-with-sub-superscripts nil)
(setq org-src-fontify-natively t)
(setq org-export-html-postamble nil)
(setq org-todo-keywords
      '((sequence "TODO" "WIP" "DONE" )))
(setq org-todo-keyword-faces
      '(("WIP" . "yellow")))

;; ------------------
;; CALENDAR
;; ------------------

;; interpret the date 1/2/1990 as February 1, 1990
(setq european-calendar-style t) ; before using any calendar or diary command

;; years must be written in full
(setq abbreviated-calendar-year nil)
(setq diary-abbreviated-year-flag nil)

;; calendrier en francais
;; (setq calendar-week-start-day 1  ; 0:Sunday, 1:Monday
;;       calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi"
;;     						   "Jeudi" "Vendredi" "Samedi"]
;;       calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai"
;;     							 "Juin" "Juillet" "Aout" "Septembre"
;;     							 "Octobre" "Novembre" "Décembre"]
;;       )


;; mark all visible dates that have diary entries
(setq mark-diary-entries-in-calendar t)
(setq diary-file "~/.emacs.d/diary")

;; marks the current date, by changing its face
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; M-x sunrise-sunset
;; Lille : Longitude: 3.058 - Latitude: 50.632 

;; remove some holidays
(setq general-holidays nil)	        ; get rid of too U.S.-centric holidays
(setq hebrew-holidays nil)			; get rid of religious holidays
(setq islamic-holidays nil)			; get rid of religious holidays
(setq oriental-holidays nil)		; get rid of Oriental holidays
(setq bahai-holidays nil)			; get rid of Baha'i holidays

(setq calendar-week-start-day 1
      calendar-intermonth-text
      '(propertize
	(format "%2d"
		(car
		 (calendar-iso-from-absolute
		  (calendar-absolute-from-gregorian (list month day year)))))
	'font-lock-face 'font-lock-function-name-face))


(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)
            (require 'cl)
            (defun calendar-count-days-generic(d1 d2)
              (let* ((days (- (calendar-absolute-from-gregorian d1)
                              (calendar-absolute-from-gregorian d2)))
                     (days (1+ (if (> days 0) days (- days)))))
                days))
            (defun calendar-count-weekend-days-generic(date1 date2)
              (let* ((tmp-date (if (< date1 date2) date1 date2))
                     (end-date (if (> date1 date2) date1 date2))
                     (weekend-days 0))
                (while (<= tmp-date end-date)
                  (let ((day-of-week (calendar-day-of-week
                                      (calendar-gregorian-from-absolute tmp-date))))
                    (if (or (= day-of-week 0)
                            (= day-of-week 6))
                        (incf weekend-days ))
                    (incf tmp-date)))
                weekend-days))
            (defun calendar-count-workdays-region ()
              "Count the number of days (inclusive) between point and the mark 
  excluding weekends and holidays."
              (interactive)
              (let* (
                     (d1 (calendar-cursor-to-date t))
                     (d2 (car calendar-mark-ring))
                     (date1 (calendar-absolute-from-gregorian d1))
                     (date2 (calendar-absolute-from-gregorian d2))
                     (start-date (if (<  date1 date2) date1 date2))
                     (end-date (if (> date1 date2) date1 date2))
                     (days (- (calendar-count-days-generic d1 d2)
                              (calendar-count-weekend-days-generic start-date end-date))))
                (message "Region has %d workday%s (inclusive)"
                         days (if (> days 1) "s" ""))))
            ))


;; ------------------
;; ASCII
;; ------------------

(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion (let ((i -1))
		    (insert "ASCII characters 0 thru 127.\n\n")
		    (insert " Hex	 Dec  Char|	 Hex  Dec  Char|  Hex  Dec	Char|  Hex	Dec	 Char\n")
		    (while (< i 31)
		      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
				      (setq i (+ 1	i)) i (single-key-description i)
				      (setq i (+ 32 i)) i (single-key-description i)
				      (setq i (+ 32 i)) i (single-key-description i)
				      (setq i (+ 32 i)) i (single-key-description i)))
		      (setq i (- i 96))))))


;; ------------------
;; Python 
;; ------------------
(add-hook 'python-mode-hook
	  (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)
	    (global-eldoc-mode -1)
            ;;(elpy-enable)
	    ))

;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t) ; optional

;; ------------------
;; Javascript / JS / Angular / TypeScript
;; ------------------

;;(global-auto-complete-mode t)

;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))
;;  
;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)
;;  
;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;;  
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;  
;; (setq tide-format-options
;;   '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;;  
;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))


;; ------------------
;; REST CLIENT MODE
;; ------------------

(dolist (package '(restclient restclient-helm))
  (unless (package-installed-p package)
    (package-install package)))

(setq auto-mode-alist
      (cons '("\\.\\(rest\\|restclient\\)\\'" . restclient-mode) auto-mode-alist))


;; ------------------
;; Magit
;; ------------------

;; install magit 
(dolist (package '(magit))
  (unless (package-installed-p package)
    (package-install package)))


;; ------------------
;; SERVER mode
;; ------------------

(require 'server)

(if (eq (server-running-p) :other)
    (server-force-delete))

(unless (server-running-p)
  (server-start)
  (set-frame-parameter nil 'title "Emacs Server"))

;; shortcut to start emacs under windows 
;; d:\emacs-27.1-x86_64\bin\emacsclientw.exe -c -n -a d:\emacs-27.1-x86_64\bin\runemacs.exe

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
