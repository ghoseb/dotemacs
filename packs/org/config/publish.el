;; Configuration for publishing of org-mode files.

;; Experimenting with docbook exports - not finished
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
(setq org-export-docbook-xslt-proc-command
      "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")

;; Inline images in HTML instead of producting links to the image
(setq org-export-html-inline-images t)

;; Do not use sub or superscripts. I currently don't need this
;; functionality in my documents
(setq org-export-with-sub-superscripts nil)

;; Use org.css from the norang website for export document stylesheets
(setq org-export-html-style-extra
      "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
(setq org-export-html-style-include-default nil)

;; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))

;; I use the following setting to remove the xml header line for HTML
;; exports. This xml line was confusing Open Office when opening the
;; HTML to convert to ODT.
(setq org-export-html-xml-declaration
      (quote (("html" . "")
              ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
              ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

;; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)

(setq org-export-latex-listings t)

;; Org-mode can export tables as TAB or comma delimited formats. Set
;; the default format to CSV.
(setq org-table-export-default-format "orgtbl-to-csv")

;; Prevent the timestamps from being exported in documents.
(setq org-export-with-timestamps nil)

;; The following setting allows #+BIND: variables to be set on export
;; without confirmation. In rare situations where I want to override
;; some org-mode variable for export this allows exporting the document
;; without a prompt.
(setq org-export-allow-BIND t)

;; List of projects
;; norang       - http://www.norang.ca/
;; doc          - http://doc.norang.ca/
;; org-mode-doc - http://doc.norang.ca/org-mode.html and associated files
;; org          - miscellaneous todo lists for publishing
(setq org-publish-project-alist
      ;
      ; http://www.norang.ca/  (norang website)
      ; norang-org are the org-files that generate the content
      ; norang-extra are images and css files that need to be included
      ; norang is the top-level project that gets published
      (quote (("norang-org"
               :base-directory "~/git/www.norang.ca"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
               :recursive t
               :table-of-contents nil
               :base-extension "org"
               :publishing-function org-publish-org-to-html
               :style-include-default nil
               :section-numbers nil
               :table-of-contents nil
               :style "<link rel=\"stylesheet\" href=\"norang.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("norang-extra"
               :base-directory "~/git/www.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("norang"
               :components ("norang-org" "norang-extra"))
              ;
              ; http://doc.norang.ca/  (norang website)
              ; doc-org are the org-files that generate the content
              ; doc-extra are images and css files that need to be included
              ; doc is the top-level project that gets published
              ("doc-org"
               :base-directory "~/git/doc.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :recursive nil
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-publish-org-to-html org-publish-org-to-org)
               :style-include-default nil
               :style "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("doc-extra"
               :base-directory "~/git/doc.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive nil
               :author nil)
              ("doc"
               :components ("doc-org" "doc-extra"))
              ("doc-private-org"
               :base-directory "~/git/doc.norang.ca/private"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
               :recursive nil
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-publish-org-to-html
                                     org-publish-org-to-org)
               :style-include-default nil
               :style "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "Norang Private Documents"
               :sitemap-style "tree"
               :author-info nil
               :creator-info nil)
              ("doc-private-extra"
               :base-directory "~/git/doc.norang.ca/private"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive nil
               :author nil)
              ("doc-private"
               :components ("doc-private-org" "doc-private-extra"))
              ;
              ; Miscellaneous pages for other websites
              ; org are the org-files that generate the content
              ("org-org"
               :base-directory "~/git/org/"
               :publishing-directory "/ssh:www-data@www:~/org"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function org-publish-org-to-html
               :style-include-default nil
               :style "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ;
              ; http://doc.norang.ca/  (norang website)
              ; org-mode-doc-org this document
              ; org-mode-doc-extra are images and css files that need to be included
              ; org-mode-doc is the top-level project that gets published
              ; This uses the same target directory as the 'doc' project
              ("org-mode-doc-org"
               :base-directory "~/git/org-mode-doc/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-publish-org-to-html org-publish-org-to-org)
               :plain-source t
               :htmlized-source t
               :style-include-default nil
               :style "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("org-mode-doc-extra"
               :base-directory "~/git/org-mode-doc/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("org-mode-doc"
               :components ("org-mode-doc-org" "org-mode-doc-extra"))
              ;
              ; http://doc.norang.ca/  (norang website)
              ; org-mode-doc-org this document
              ; org-mode-doc-extra are images and css files that need to be included
              ; org-mode-doc is the top-level project that gets published
              ; This uses the same target directory as the 'doc' project
              ("tmp-org"
               :base-directory "/tmp/publish/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-publish-org-to-html org-publish-org-to-org)
               :style "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
               :plain-source t
               :htmlized-source t
               :style-include-default nil
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "Test Publishing Area"
               :sitemap-style "tree"
               :author-info t
               :creator-info t)
              ("tmp-extra"
               :base-directory "/tmp/publish/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("tmp"
               :components ("tmp-org" "tmp-extra")))))

;; I'm lazy and don't want to remember the name of the project to
;; publish when I modify a file that is part of a project.  So this
;; function saves the file, and publishes the project that includes
;; this file.
;; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done
;; and move on to the next thing
(defun bh/save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))
