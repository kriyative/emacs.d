(require 'org-blog)

(defun my-org-blog-post-title (info)
  (let ((title (org-export-data (plist-get info :title) info)))
    (when (and (stringp title)
               (not (string-equal "index" title)))
      (concat
       (format "<p class=\"title\">%s</p>\n" title)
       "\n<p class=\"subtitle\">\n"
       (format "<span class=\"date\">%s</span>\n"
               (format-time-string
                "%a, %d %b %Y"
                ;; "%a, %d %b %Y %H:%M:00 %z"
                (org-time-string-to-time
                 (org-element-interpret-data
                  (plist-get info :date)))))
       (mapconcat (lambda (tag)
                    (concat "<a class=\"tag\" href=\""
                            tag ".html"
                            "\">" tag "</a>"))
                  (org-blog-get-tags info)
                  "")))))

(defun org-blog-get-tags (info)
  (let ((h (plist-get info :exported-data))
        r)
    (maphash (lambda (k v)
               (when (and (listp k)
                          (eq 'keyword (car k))
                          (equal "TAGS"
                                 (plist-get (cadr k) :key)))
                 (setq r (plist-get (cadr k) :value))))
             h)
    (split-string r "," t split-string-default-separators)))

(defun my-org-blog-head (info)
  "<link rel=\"stylesheet\" href=\"/assets/style.css\" type=\"text/css\"/>")

(defun my-org-blog-banner (info)
  (concat
   "<div class=\"banner\">\n"
   "  <a href=\"/\">; kriyative</a>\n"
   "  <p>; <span class=\"subhead\">adventures in Lisp, Clojure and Emacs</span></p>\n"
   "</div>\n"))

(defun plist-keys (plist)
  (let ((i 0) r)
    (while (< i (length plist))
      (push (nth i plist) r)
      (setq i (+ i 2)))
    r))

(defun my-org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let ((decl (or (and (stringp org-html-xml-declaration)
                          org-html-xml-declaration)
                     (cdr (assoc (plist-get info :html-extension)
                                 org-html-xml-declaration))
                     (cdr (assoc "html" org-html-xml-declaration))

                     "")))
       (when (not (or (eq nil decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
                         (or (and org-html-coding-system
                                  (fboundp 'coding-system-get)
                                  (coding-system-get org-html-coding-system 'mime-charset))
                             "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (when (org-html-xhtml-p info)
	     (format
	      " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
	      (plist-get info :language) (plist-get info :language)))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (if-let ((head-function (plist-get info :head-function)))
       (funcall head-function info)
     (org-html--build-head info))
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (if-let ((banner-function (plist-get info :banner-function)))
       (funcall banner-function info)
     (let ((link-up (org-trim (plist-get info :html-link-up)))
           (link-home (org-trim (plist-get info :html-link-home))))
       (unless (and (string= link-up "") (string= link-home ""))
         (format org-html-home/up-format
                 (or link-up link-home)
                 (or link-home link-up)))))
   ;; Preamble.
   (if-let ((preamble-function (plist-get info :preamble-function)))
       (funcall preamble-function info)
     (org-html--build-pre/postamble 'preamble info))
   ;; Document contents.
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'content org-html-divs))
	   (nth 2 (assq 'content org-html-divs)))
   ;; Document title.
   (if-let ((title-function (plist-get info :title-function)))
       (funcall title-function info)
     (let ((title (plist-get info :title)))
       (format "<h1 class=\"title\">%s</h1>\n"
               (org-export-data (or title "") info))))
   contents
   (when-let ((meta-content-function (plist-get info :meta-content-function)))
     (funcall meta-content-function info))
   (format "</%s>\n"
	   (nth 1 (assq 'content org-html-divs)))
   ;; Postamble.
   (if-let ((postamble-function (plist-get info :postamble-function)))
       (funcall postamble-function info)
     (org-html--build-pre/postamble 'postamble info))
   ;; Closing document.
   "</body>\n</html>"))

(defun org-blog-publish ()
  (interactive)
  (org-publish-project "blog"))

(org-export-define-derived-backend 'org-blog-html
    'html
  :translate-alist '((template . my-org-html-template)))

(defun org-blog-publish-to-html (plist filename pub-dir)
  (org-publish-org-to 'org-blog-html
                      filename
		      (concat "."
                              (or (plist-get plist :html-extension)
                                  org-html-extension "html"))
		      plist pub-dir))
