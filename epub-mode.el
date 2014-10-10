;;; epub-mode.el --- Mode for creating and editing Epubs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Eric Abrahamsen

;; Version: 0

;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: hypermedia

;; Package-Requires: ((esxml "20140320"))

;; URL: https://github.com/girzel/epub-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a mode for creating and editing Epub files:
;; digital books. In fact it is not a single major mode, but rather a
;; routine for zipping and unzipping Epub files, and a collection of
;; minor modes to help with editing the files and directories that
;; make up an Epub archive: dired, html, xml, and css.

;; There are two primary entry points: `epub-create-epub', for making
;; a new Epub from scratch, and `epub-edit-epub', for editing an
;; existing one.

;; Both functions establish a temporary location on your system, where
;; the unzipped Epub is held as a directory tree. When you're done
;; editing the files in the tree, call `epub-zip-epub' in any of the
;; files, and you'll be prompted for a location to save the *.epub
;; file to.

;; The main Epub editing buffer is a Dired buffer visiting the
;; top-level directory, with all subdirectories inserted.

;; See the manual for more details.

;;; Code:

(require 'dired)
;; For writing and modifying XML bits.
(require 'esxml)

(defgroup epub nil
  "Creating and Editing Epub Files"
  :tag "Epub")

(defcustom epub-default-version "2.0"
  "Default Epub version to create.

Generally it's best to leave this at \"2.0\", the Epub 3
specification isn't done yet."
  :group 'epub
  :type '(choice
	  (const :tag "2.0" 2)
	  (const :tag "3.0" 3)))

(defcustom epub-check-exec "epubcheck"
  "Executable used for checking the validity of Epub files."
  :group 'epub
  :type 'string)

(defcustom epub-contents-dirnames
  (list "Text" "Styles" "Images" "Fonts")
  "Default directories to create inside the OEBPS contents
  directory."
  :group 'epub
  :type 'list)

(defcustom epub-dired-ls-switches "-lgohaR"
  "Equivalent value of `dired-listing-switches' for the Epub
  listing."
  :group 'epub
  :type 'string)

(defcustom epub-contents-file-template
  '(package
    ((xmlns . "http://www.idpf.org/2007/opf")
     ;; Unique identifier value comes from "id" attribute of the
     ;; "identifier" metadata element
     (unique-identifier . "bookid")
     (version . "%s")
     (xmlns:dc . "http://purl.org/dc/elements/1.1/"))
    (metadata nil
	      (dc:indentifier
	       ((id . "bookid"))
	       "%s")
	      (meta ((name . "generator")
		     (content . "%s"))))
    (manifest nil)
    (spine ((toc . "ncx"))))
  "Standard structure of the contents.opf file for a new Epub.

This sexp is passed to `esxml-to-xml' for conversion to a string,
and the resulting string passed to `format' to insert the epub
version (usually 2.0), a unique identifier (usually produced by
`epub-make-unique-identifier'), and the version of Epub mode used
to create the Epub.

If you customize it, you'll want to leave three formatting
escapes (%s) where these values will be placed, in the order
stated above."
  :group 'epub
  :type 'list)

(defvar epub-log-buffer "*Epub Log*"
  "Output from zip commands goes here.")

(defvar epub-tmp-dir (make-temp-file "emacs-epub-" t)
  "Temp work directory for expanding Epub files.")

;;;###autoload(put 'epub-work-dir 'safe-local-variable 'stringp)
(defvar epub-work-dir nil
  "Directory name for any given Epub file.

Located within the `epub-tmp-dir' directory. This variable will
be set to a different directory-local value for each Epub being
edited.")

;;;###autoload(put 'epub-target-file 'safe-local-variable 'stringp)
(defvar epub-target-file nil
  "The filename used for the final zipped Epub.

This will be set as a directory-local value for each Epub being
edited.")

(defvar epub-oebps-dirname "OEBPS")
(defvar epub-meta-inf-dirname "META-INF")

(defun epub-mode-identifier-string ()
  "Return a descriptive string about this package.

This goes in the Epub metadata, under the \"generator\"
heading."
  (format "Epub mode, %s, for Emacs"
	  (epub-mode-version)))

(defun epub-mode-version ()
  "Get a sensible version designation for this package.

Either \"version XYZ\", if Epub mode is being run as a package,
or else \"development version\"."
  (if (assoc 'epub-mode package-activated-list)
      (let ((package (assoc 'epub-mode package-alist)))
	(format "version %s"
		(package-version-join
		 (package-desc-version (second package)))))
    "development version"))

;; Templates for the various XML Epub files.

(defun epub-make-mimefile (&optional dir)
  (with-temp-buffer
    (insert "application/epub+zip")
    (write-file (expand-file-name "mimetype" dir))))

(defun epub-make-container-file (&optional dir)
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    (insert (pp-esxml-to-xml
	     '(container
	       ((version . "1.0")
		(xmlns . "urn:oasis:names:tc:opendocument:xmlns:container"))
	       (rootfiles nil
			  (rootfile
			   ((full-path . "OEBPS/content.opf")
			    (media-type . "application/oebps-package+xml")))))))
    (write-file (expand-file-name "container.xml" dir))))

(defun epub-make-toc-file (&optional dir)
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?> <!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">") 
    (insert (pp-esxml-to-xml
	     '(ncx
	       ((xmlns . "http://www.daisy.org/z3986/2005/ncx/")
		(version . "2005-1"))
	       (head nil
		     (meta ((name . "dtb:maxPageNumber") (content . "0")))
		     (meta ((name . "dtb:totalPageCount") (content . "0")))
		     (meta ((name . "dtb:depth") (content . "1"))))
	       (docTitle nil)
	       (navMap nil))))
    (write-file (expand-file-name "toc.ncx" dir))))

(defun epub-make-contents-file (&optional dir)
  "Create an empty content.opf file for a new Epub.

Template is filled in with metadata values for unique-identifier
and version."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>")
    (insert (format
	     (pp-esxml-to-xml epub-contents-file-template)
	     epub-default-version
	     (epub-make-unique-identifier)
	     (epub-mode-identifier-string)))
    (write-file (expand-file-name "content.opf" dir))))

(defun epub-make-unique-identifier ()
  "Make a unique identifier for a new Epub."
  "fake-identifier")

(defun epub-create-epub-template (&optional dir)
  "Create an \"empty\" Epub directory structure."
  (let ((default-directory (or dir epub-work-dir)))
    (condition-case err
	(progn
	  (epub-make-mimefile)
	  (make-directory epub-meta-inf-dirname)
	  (make-directory epub-oebps-dirname)
	  (let ((default-directory
		  (file-name-as-directory
		   (expand-file-name epub-oebps-dirname))))
	    (dolist (d epub-contents-dirnames)
	      (make-directory d))
	    (epub-make-contents-file)
	    (epub-make-toc-file))
	  (let ((default-directory
		  (file-name-as-directory
		   (expand-file-name epub-meta-inf-dirname))))
	    (epub-make-container-file)))
      (error
       (delete-directory epub-work-dir t)
       (signal (car err) (cdr err))))))

(defun epub-make-temp-dir (name)
  "Create a unique, temporary directory for editing a single Epub.

Each Epub is edited in a temporary directory, located within
`epub-tmp-dir'. The name of this directory is saved as the
variable `epub-work-dir', which is directory-local to the files
within this dir."
  (let ((temporary-file-directory epub-tmp-dir))
    (make-temp-file name t)))

(defun epub-check-filename (name)
  "Make sure the user-entered name has the right extension."
  (cond
   ((or (null (file-name-extension name))
	(string= "" (file-name-extension name)))
    (setq name
	  (concat
	   (file-name-sans-extension name)
	   ".epub")))
   ((null (string= "epub" (file-name-extension name)))
    (user-error "Output file should have an .epub extension")))
  name)

(defun epub-create-epub (file)
  "Create a new Epub directory tree."
  (interactive "F")
  (setq file (epub-check-filename file))
  (setq epub-work-dir
	(epub-make-temp-dir
	 (file-name-base file)))
  (setq epub-target-file file)
  (dired epub-work-dir)
  (epub-create-epub-template)
  (epub-dired-mode))

(defun epub-edit-epub (file)
  "Edit an existing Epub file."
  (interactive "f")
  (unless (string= (file-name-extension file) "epub")
    (user-error "Not an epub file"))
  (setq epub-work-dir
	(epub-make-temp-dir
	 (file-name-base file)))
  (setq epub-target-file file)
  (call-process "unzip" nil epub-log-buffer nil
		file "-d" epub-work-dir)
  (dired epub-work-dir)
  (epub-dired-mode))

(defun epub-zip-epub ()
  "Zip the current Epub work directory to an Epub file.

Reads the directory-local value of `epub-target-file' as a
destination."
  (interactive)
  ;; Need to do a sanity check to see if this *is* an Epub directory.
  (let ((target epub-target-file)
	exitcode temp-target)
    (while (file-exists-p target)
      (unless (y-or-n-p "Overwrite existing file?")
	(setq target
	      (expand-file-name
	       (read-file-name "Zip to file: "
			       (file-name-directory target)
			       (file-name-nondirectory target))))))
    (setq target (epub-check-filename target))
    (setq temp-target
	  (expand-file-name
	   (file-name-nondirectory target)
	   epub-tmp-dir))
    ;; Why doesn't it work to let-bind default-directory, instead of
    ;; this?
    (with-current-buffer (car (dired-buffers-for-dir
			       epub-work-dir))
      (setq exitcode
	    (call-process "zip" nil epub-log-buffer nil
			  "-Xr"
			  temp-target
			  "mimetype"
			  epub-meta-inf-dirname
			  epub-oebps-dirname
			  ;; Exclude hidden dotfiles
			  "-x" "\.*")))
    (unless (zerop exitcode)
      (display-buffer epub-log-buffer)
      (error "Unable to create Epub"))
    (message "Created %s" (file-name-nondirectory target))
    (copy-file temp-target target t)
    (delete-file temp-target)))

(define-derived-mode epub-dired-mode dired-mode "Epub"
  "Major mode for editing Epub files as unzipped directories.

Derived from Dired mode"
  :group 'epub
  (dired-readin)
  (add-dir-local-variable
   nil 'epub-work-dir epub-work-dir)
  (add-dir-local-variable
   nil 'epub-target-file epub-target-file)
  (basic-save-buffer)
  (kill-buffer)
  (setq-local dired-actual-switches epub-dired-ls-switches)
  (revert-buffer))

(defvar epub-dired-mode-map (make-sparse-keymap)
  "Keymap in effect in the main Epub editing buffer.")

(define-key epub-dired-mode-map (kbd "C-c C-c") 'epub-zip-epub)

(eval-when '(load)
  (unless (executable-find "zip")
    (error "Epub-mode requires the \"zip\" executable.")))

(provide 'epub-mode)
;;; epub-mode.el ends here
