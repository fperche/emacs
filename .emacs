
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

;; Ensure use-package is installed and loaded
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(custom-set-variables
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(powershell consult-ls-git consult-ag embark-consult embark consult marginalia orderless vertico dirvish all-the-icons-dired all-the-icons rainbow-delimiters company session csv-mode magit helm-ag helm-git-grep helm-ls-git)))

;; ---------
;; Basic
;; ---------

;; <down> at end of file should not create new lines on its own 
(setq next-line-add-newlines nil)

;; replace 'yes or no' by 'y or n'
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

;; delete text when writting over selection
(delete-selection-mode 1)

(cua-mode nil) ;; CUA mode activated
(global-set-key [(control meta o)] 'find-file)

(global-set-key "\M-l" (quote goto-line))
(global-set-key "\C-x\ a" (quote mark-whole-buffer))

;; screen split
(global-set-key [f4] (quote kill-this-buffer)) 
(global-set-key [f5] (quote delete-window))
(global-set-key [f6] (quote other-window)) 
(global-set-key [f7] (quote split-window-vertically))
(global-set-key [f8] (quote split-window-horizontally))

;; Rectangles / Column mode
;; Copy rectangle : C-x r r r ==> C-x * 2 while selecting in CUA + r (rectangle) + r (put to register) + r (name of the register)
;; Insert rectangle : C-x r i r ==> C-x * 2 while selecting in CUA + r (rectangle) + i (insert from register) + r (name of the register)

;; Kill ring
;; C-y (or C-v in CUA) + M-y : paste old value from kill-ring

;; ---------
;; UI
;; ---------

;; display file name under frame 
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

(tool-bar-mode -1) ;; no toolbar 
(setq visible-bell 1) ;; no sounding bell

;; display column and line number
(column-number-mode t)
(line-number-mode t)

(when window-system (set-frame-size (selected-frame) 120 48))

;; Default window
(set-foreground-color "white")
(set-background-color "gray10")
(set-cursor-color "firebrick")

;; New Frames
(setq default-frame-alist
      (append default-frame-alist
              '((foreground-color . "white")
                (background-color . "gray10")
                (cursor-color . "firebrick"))))


;; windows font
(when (member "Consolas" (font-family-list)) ;; when consolas font is available on system : use it 
  (add-to-list 'default-frame-alist '(font . "-outline-Consolas-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1" ))
  (set-face-attribute 'default t :font "-outline-Consolas-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1" ))

;; max font lock settings 
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; display current line with a slighly different font 
(require 'hl-line)
(set-face-attribute 'hl-line nil :inherit nil :background "gray20")
(global-hl-line-mode 1)

;; transparency
;; (defun transparency (alpha-level)
;;   (interactive "p")
;;   (message (format "Alpha level passed in: %s" alpha-level))
;;   (let ((alpha-level (if (< alpha-level 2)
;; 			 (read-number "Opacity percentage: " 85)
;; 		       alpha-level))
;; 	(myalpha (frame-parameter nil 'alpha)))
;;     (set-frame-parameter nil 'alpha alpha-level))
;;   (message (format "Alpha level is %d" (frame-parameter nil 'alpha))))


;; ---------
;; Delimiters handling
;; ---------

(require 'paren)
(show-paren-mode t)
(setq blink-matching-paren t)
(setq blink-matching-paren-on-screen t)

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

(require 'savehist)
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
	))

;;; 2022-07-22 : does not achieve expected result of remembering previous values under isearch, need to debug

;; ---------
;; CSV
;; ---------

(when (not (< emacs-major-version 27))
  (progn
    ;; install csv-mode and requires it
    (dolist (package '(csv-mode))
      (unless (package-installed-p package)
	(package-install package)))
    (use-package csv-mode
      :ensure t
      :mode "\\.csv$"
      :init
      (setq csv-separators '("|")))
    ))

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

;; ---------
;; Misc text utility functions
;; ---------

(defun carriage-return-conversion-dos-to-unix()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t) (replace-match "\n")))

(defun carriage-return-conversion-unix-to-dos()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

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

;; macro execution for all lines under region
(global-set-key "\C-ce" 'apply-macro-to-region-lines)

;; -------
;; isearch
;; -------

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(defun isearch-consult-line ()
  "Invoke `consult-line' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (consult-line (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-l") 'isearch-consult-line)

(defun isearch-consult-line-multi ()
  "Invoke `consult-line-multi' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (consult-line-multi (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-f s") 'isearch-consult-line-multi)


;; always exit searches at the beginning of the expression found
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

;; -------
;; DIRED 
;; -------

(require 'dired) ;; available with default emacs install

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

;; (when (display-graphic-p)
;;   (require 'all-the-icons)
;;   (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

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

(defun dired-for-each-marked-file(fn)
  "Do stuff for each marked file, only works in dired window"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((filenames (dired-get-marked-files)))
	(mapcar fn filenames))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))

;; integration with windows
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

;;(setq ls-lisp-ignore-case t)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies  'always)
(setq dired-listing-switches "-ahl")

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

;;(define-key dired-mode-map "q" 'kill-this-buffer) ;; consistent action with "v" view-file-mode
(define-key dired-mode-map "b" 'consult-bookmark) ;; Good idea to set "b" to default bookmarks UI under dired-mode-map
(put 'dired-find-alternate-file 'disabled nil) ;; autorize "a" shortcut on dired (open destination in current buffer)

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; dirvish bookmarks are different from standard bookmarks : do not use
  ;; if under 
  (if (display-graphic-p)
      (dirvish-attributes '(file-size subtree-state))
    (dirvish-attributes '(all-the-icons file-size subtree-state))
    )
  (dirvish-hide-details nil)
  :config
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t) ;; guess destination folder if possible
  (setq delete-by-moving-to-trash nil) ;; delete do not move to trash
  ;; (setq dirvish-reuse-session t)
  ;; Enable mouse drag-and-drop files to other applications (added in Emacs 29, for unix system only)
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  ;; Make sure to use the long name of flags when exists
  ;; eg. use "--almost-all" instead of "-A"
  ;; Otherwise some commands won't work properly
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  ;; (setq dirvish-preview-dispatchers
  ;;  	(cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers)) ;; requires pdftoppm available in path / still not really working on W32 with a lot dependencies at execution time
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (:map dired-mode-map ; Dirvish respects all the keybindings in this map
	("M-<up>"	. dired-up-directory)
	("M-n"		. dirvish-history-go-forward)
	("M-<right>"	. dirvish-history-go-forward)
	("M-p"		. dirvish-history-go-backward)
	("M-<left>"	. dirvish-history-go-backward)
	("M-j" 		. dirvish-history-jump)
	("." . dired-omit-mode)
	("b"   . consult-bookmark)
	("f"   . dirvish-file-info-menu)
	("y"   . dirvish-yank-menu)
	("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
	("?"   . dirvish-dispatch)  ; remapped `dired-summary'
	("TAB" . dirvish-subtree-toggle)
	("M-l" . dirvish-ls-switches-menu)
	("M-m" . dirvish-mark-menu)
	("M-f" . dirvish-toggle-fullscreen) ; 3 panes mode (parent / current / preview)
	("M-s" . dirvish-setup-menu)
	))

;; register a plain text dirvish dispatcher for docx files
(dirvish-define-preview docx (file ext)
  "Preview docx files in plain text
Require: `pandoc' (executable)"
  :require ("pandoc" )
  (cond ((equal ext "docx") `(shell . ("pandoc" "-t" "plain",file)))))
(add-to-list 'dirvish-preview-dispatchers 'docx)

;; register a plain text dirvish dispatcher for pdf files
(dirvish-define-preview pdftotext (file ext)
  "Preview pdf files in plain text
Require: `pdftotext' (executable)"
  :require ("pdftotext" )
  (cond ((equal ext "pdf") `(shell . ("pdftotext",file,"-")))))
(setq dirvish-preview-dispatchers
      (cl-substitute 'pdftotext 'pdf dirvish-preview-dispatchers))

(dirvish-define-preview archive-unzip (file ext)
  "Preview archive files (replaced zipinfo requirement with 'unzip -Z'
Require: `unzip' (executable)
Require: `tar' (executable)"
  :require ("unzip" "tar")
  (cond ((equal ext "zip") `(shell . ("unzip" "-Z" ,file)))
        ((member ext '("tar" "zst")) `(shell . ("tar" "-tvf" ,file))))
  )
(setq dirvish-preview-dispatchers
      (cl-substitute 'archive-unzip 'archive dirvish-preview-dispatchers))

;; ---------------------
;; TRAMP
;; ---------------------

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq password-cache-expiry nil)

;; ---------------------
;; FIND / GREP
;; ---------------------

;; (require 'find-dired)

;; (global-set-key "\C-x\ f" (quote find-name-dired))

;; ;; unix find need to be installed manually with, renamed to gfind and put in path
;; (if (eq system-type 'windows-nt)
;;     (setq find-program "gfind.exe")) 

;; ;; pattern for find
;; (setq find-ls-option (quote ("-exec ls -ld {} \;" . "-ld")))

;; ;; pattern for recursive grep
;; ;;   <D> for the base directory
;; ;;   <X> for the find options to restrict directory list
;; ;;   <F> for the find options to limit the files matched
;; ;;   <C> for the place to put -i if the search is case-insensitive
;; ;;   <R> for the regular expression to search for
;; (global-set-key "\C-x\ g" (quote rgrep))
;; (setq grep-find-ignored-directories (quote ("CVS" ".git" ".svn" "classes" "build" "target" ".metadata" "pristine")))
;; (setq grep-find-template "gfind . <X> -type f <F> -exec grep -a <C> -nH -e <R> {} \";\" ")

;; ;; -type f -regex ".*.\(js\|xml\|ts\|java\|sql\)" -and -not -path "*bower*" -and -not -path "*target*" 

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
;; Emacs Lisp
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
(setq rng-nxml-auto-validate-flag nil) ;; disable automatic validation by default

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

;; ------------------
;; SQL
;; ------------------
;; (setq sql-product (quote oracle))

;; (add-hook 'sql-interactive-mode-hook
;;           '(lambda ()
;;              (company-mode)))

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
;; vertico / orderless / marginalia / consult / embark
;; ------------------

(dolist (package '(vertico orderless marginalia consult consult-ag consult-ls-git embark embark-consult))
  (unless (package-installed-p package)
    (package-install package)))

;; Enable vertico 
(use-package vertico
  :init
  (vertico-mode))

;; use the orderless completion style : (gets fuzzy matching)
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable richer annotations on vertico with marginalia
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; register a file annotator that is not displaying file modes 
(defun marginalia-annotator-local-file-no-modes (cand)
  "Annotate local file without file modes CAND."
  (when-let (attrs (ignore-errors
		     ;; may throw permission denied errors
		     (file-attributes (substitute-in-file-name
				       (marginalia--full-candidate cand))
				      'integer)))
    (if (eq marginalia-align 'right)
        (marginalia--fields
         ;; File owner at the left
         ((marginalia--file-owner attrs) :face 'marginalia-file-owner)
         ((file-size-human-readable (file-attribute-size attrs))
          :face 'marginalia-size :width -7)
         ((marginalia--time (file-attribute-modification-time attrs))
          :face 'marginalia-date :width -12))
      (marginalia--fields
       ((file-size-human-readable (file-attribute-size attrs))
        :face 'marginalia-size :width -7)
       ((marginalia--time (file-attribute-modification-time attrs))
        :face 'marginalia-date :width -12)
       ;; File owner at the right
       ((marginalia--file-owner attrs) :face 'marginalia-file-owner)))))
(add-to-list 'marginalia-annotator-registry
             '(file marginalia-annotator-local-file-no-modes marginalia-annotate-file builtin none))

;; save last annotators selected
(advice-add #'marginalia-cycle :after
            (lambda ()
              (let ((inhibit-message t))
                (customize-save-variable 'marginalia-annotator-registry
                                         marginalia-annotator-registry))))

;; Execute actions on proposed / selected items
(use-package embark
  :ensure t
  :bind
  (("M-E" . embark-act)          ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; integration between embark and consult packages
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(global-set-key (kbd "C-f") nil) ;; Remove the old keybinding - reuse it when trying to "f"ind something

;; use cases to replace helm

;;; helm-M-x : covered OOTB with vertico

;;; helm-show-kill-ring : covered
;;;; (display of multiple lines copied is not great, but has live preview in buffer when hovering)
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)

;;; helm-mini : easy switch to buffer list + bookmarks + recent files : consult-buffer
;;; helm-mini : review open buffers, select some and kill them : manageable with embark after filtering with vertico
(recentf-mode 1) ;; turn on recent file list 
(run-at-time nil (* 5 60) 'recentf-save-list) ;; save every 5 minutes recent file list

;; (setq recentf-max-menu-items 25)
;; (setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-b") 'consult-buffer)

;;; helm-multi-swoop-all : search content of all buffers : ??
(global-set-key (kbd "C-f s") 'consult-line-multi)

;;; helm-projectile-grep : grep whole project sources, not only subdirectories from where git grep is launched : ???
(global-set-key (kbd "C-f g") 'consult-git-grep)
(global-set-key (kbd "C-f G") 'consult-grep)

;;; helm-git-ls : find file name on git repo
(require 'consult-ls-git)
(global-set-key (kbd "C-f l") 'consult-ls-git)

;;; helm-ag : use ag for text and log folders searches + live filtering
(require 'consult-ag)
(global-set-key (kbd "C-f a") 'consult-ag)

;;; keep or flush lines (! ) from buffer
(global-set-key (kbd "C-f k") 'consult-keep-lines)

;; Bookmarks
(global-set-key (kbd "C-f b") 'consult-bookmark)

;; ------------------
;; HELM-MODE
;; ------------------

;; ;; install helm packages which are going to be required
;; (dolist (package '(helm helm-swoop helm-projectile helm-ls-git helm-git-grep helm-ag))
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; (require 'helm)

;; (with-eval-after-load 'helm
;;   (setq helm-always-two-windows nil)
;;   (setq helm-display-buffer-default-height 20)
;;   (setq helm-default-display-buffer-functions '(display-buffer-in-side-window)))

;; ;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ;; make TAB works in terminal, C-i is the same as TAB
;; (define-key helm-map (kbd "S-<tab>") 'helm-select-action)

;; ;; Buffer list + bookmarks + recent files under Helm
;; (setq helm-mini-default-sources '(helm-source-buffers-list
;;                                   helm-source-bookmarks
;;                                   helm-source-recentf))
;; ;;(global-set-key "\C-x\C-b" (quote helm-mini))

;; ;; helm M-x replacement
;; ;; (global-set-key (kbd "M-x") 'helm-M-x)

;; ;; (global-set-key (kbd "C-f h") 'helm-resume)

;; ;; helm search content of buffers
;; ;;(global-set-key (kbd "C-f s") 'helm-multi-swoop-all)

;; ;; helm search repo with git grep
;; ;; (global-set-key (kbd "C-f g") 'helm-projectile-grep) ;; use projectile Git grep as default (able to grep whole project sources, not only subdirectories from where helm-git-grep is launched)
;; (setq projectile-use-git-grep t)
;; ;;(global-set-key (kbd "C-f g") 'helm-grep-do-git-grep)
;; ;;(define-key isearch-mode-map (kbd "C-f g") 'helm-git-grep-from-isearch)

;; (setq helm-git-grep-candidate-number-limit 100)
;; (setq helm-input-idle-delay 0.5)
;; (setq helm-idle-delay 1) ;; 1 second before launching any external command (git grep chokes otherwise)

;; ;; helm ag
;; ;; (global-set-key (kbd "C-f a") 'helm-ag) ;; use ag for text and log folders searches

;; ;; helm git ls
;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-ls-git)
;; ;;(global-set-key (kbd "C-f l") 'helm-ls-git-ls)

;; ;; helm bookmarks
;; ;; (global-set-key (kbd "C-f b") 'helm-bookmarks)

;; ;; helm google, because why not 
;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))
;; (global-set-key (kbd "C-f w") 'helm-google-suggest)

;; ;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;; (setq ad-redefinition-action 'accept)

;; ;; make helm header sections smaller
;; (eval-after-load 'helm
;;   (lambda () 
;;     (set-face-attribute 'helm-source-header nil
;;                         :background "gray30"
;;                         :height 90)))

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

;; calendar days in French
;; (setq calendar-week-start-day 1  ; 0:Sunday, 1:Monday
;;       calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi"
;;     						   "Jeudi" "Vendredi" "Samedi"]
;;       calendar-month-name-array ["Janvier" "FÃ©vrier" "Mars" "Avril" "Mai"
;;     							 "Juin" "Juillet" "Aout" "Septembre"
;;     							 "Octobre" "Novembre" "DÃ©cembre"]
;;       )

;; mark all visible dates that have diary entries
(setq mark-diary-entries-in-calendar t)
(setq diary-file "~/.emacs.d/diary")

;; marks the current date, by changing its face
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

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
;; HIDE SHOW MODE
;; ------------------

(use-package hideshow
  :bind (
	 ("C-<kp-subtract>" . hs-toggle-hiding)
	 ("C-M-<kp-subtract>" . hs-hide-all)
	 ("C-<kp-add>" . hs-show-block)
         ("C-M-<kp-add>" . hs-show-all)
	 )
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  ;; Add `json-mode' and `javascript-mode' to the list
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil))))
  (setq hs-isearch-open t) ;; Automatically open a block if you search for something where it matches
  )

;; ------------------
;; REST CLIENT MODE
;; ------------------

;; (dolist (package '(restclient restclient-helm))
;;   (unless (package-installed-p package)
;;     (package-install package)))
;;  
;; (setq auto-mode-alist
;;       (cons '("\\.\\(rest\\|restclient\\)\\'" . restclient-mode) auto-mode-alist))

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

(if (eq system-type 'windows-nt)
    (progn 
      (require 'server)
      (if (eq (server-running-p) :other)
	  (server-force-delete))
      (unless (server-running-p)
	(server-start)
	(set-frame-parameter nil 'title "Emacs Server"))))

;; shortcut to start emacs under windows
;;; D:\emacs\bin\emacsclientw.exe -c -n -a d:\emacs\bin\runemacs.exe

;; regedit entries to get "Open with Emacs" on empty background to open in dired
;;; Windows Registry Editor Version 5.00
;;; [HKEY_CLASSES_ROOT\Directory\Background\shell\OpenWithEmacs]
;;; @="Open with &Emacs"
;;; [HKEY_CLASSES_ROOT\Directory\Background\shell\OpenWithEmacs\command]
;;; @="D:\\emacs\\bin\\emacsclientw.exe -c -n -a d:\\emacs\\bin\\runemacs.exe \"%V\""

