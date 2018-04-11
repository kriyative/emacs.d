(el-get-bundle bastibe/org-static-blog)

(use-package org-static-blog
  :config
  (setq org-static-blog-publish-url "http://localhost:8000/"
        org-static-blog-publish-title "kriyative"
        org-static-blog-root-directory "~/work/kriyative/kriyative.github.com/"
        org-static-blog-posts-directory org-static-blog-root-directory
        org-static-blog-publish-directory org-static-blog-posts-directory
        org-static-blog-drafts-directory (concat org-static-blog-root-directory "drafts/")
        org-static-blog-page-header (concat
                                     "<link href=\"/assets/style.css\""
                                     " rel=\"stylesheet\""
                                     " type=\"text/css\"></link>")
        org-static-blog-page-preamble (concat
                                       "<div class=\"banner\">"
                                       "<h1><a href=\"/\">kriyative</a></h1>"
                                       "</div>"
                                       "<aside id=\"sidebar\">"
                                       "<section class=\"panel\">"
                                       "<p><img id=\"profile-pic\" "
                                       "src=\"http://www.gravatar.com/avatar/6dcf36ddd0aa73e04e125e3c10f6ff05.png\"/>"
                                       "Hi, I'm Ram Krishnan - welcome to my blog (mostly) "
                                       "on Lisp and Clojure programming</p>"
                                       "</section>"
                                       "<section class=\"panel\">"
                                       "<h1>Links</h1>"
                                       "<ul>"
                                       "<li><a href=\"https://github.com/kriyative\">GitHub</a></li>"
                                       "<li><a href=\"/archive.html\">Archived posts</a></li>"
                                       "<li><a href=\"/tags.html\">Tags</a></li>"
                                       "</ul>"
                                       "</section>"
                                       "</aside>"
                                       )
        org-static-blog-enable-tags t
        org-static-blog-index-length 2))

(defun org-static-blog-post-preamble (post-filename)
  "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat
   "<h1 class=\"post-title\">"
   "<a href=\"" (org-static-blog-get-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
   "</h1>\n"
   "<div class=\"post-date\">"
   (format-time-string "%d %b %Y" (org-static-blog-get-date post-filename))
   "</div>\n"))

(defun org-static-blog-get-post-summary (post-filename)
  "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
  (concat
   "<h1 class=\"post-title\">"
   "<a href=\""
   (concat "/" (org-static-blog-get-url post-filename))
   "\">" (org-static-blog-get-title post-filename) "</a>"
   "</h1>\n"
   "<div class=\"post-date\">"
   (format-time-string "%d %b %Y" (org-static-blog-get-date post-filename))
   "</div>\n"))

(defun org-static-blog-post-postamble (post-filename)
  "Returns the tag list of the post.
This function is called for every post and appended to the post body.
Modify this function if you want to change a posts footline."
  (let ((taglist-content ""))
    (setq taglist-content "<div class=\"blog-post-postamble\">\n")
    (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
      (setq taglist-content
            (concat taglist-content
                    "<ul class=\"taglist\">"
                    "Published under "))
      (dolist (tag (sort (org-static-blog-get-tags post-filename) 'string-lessp))
        (setq taglist-content
              (concat taglist-content "<li><a class=\"tag\" href=\""
                      "tags/" (downcase tag) ".html"
                      "\">" tag "</a></li>\n")))
      (setq taglist-content (concat taglist-content "</ul>")))
    (setq taglist-content (concat taglist-content "</div>\n"))
    taglist-content))

(defun org-static-blog-extract-paragraphs (post-filename num-paras)
  (with-temp-buffer
    (insert-file-contents post-filename)
    (goto-char (point-min))
    (ignore-errors
      (forward-paragraph (1+ num-paras)))
    (buffer-substring-no-properties (point-min) (point))))

(defun org-static-blog-get-body-text (post-filename &optional exclude-title)
  "Get the text body without headers from POST-FILENAME. Preamble
and Postamble are excluded, too."
  (with-temp-buffer
    (let ((final-buffer (current-buffer))
          (org-export-show-temporary-export-buffer nil))
      (with-temp-buffer
        (insert (org-static-blog-extract-paragraphs post-filename 1))
        (org-export-to-buffer 'ascii final-buffer nil nil nil t)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-static-blog-get-rss-item (post-filename)
  "Assemble RSS item from post-filename.
The HTML content is taken from the rendered HTML post."
  (concat
   "<item>\n"
   "  <title>" (org-static-blog-get-title post-filename) "</title>\n"
   "  <description><![CDATA["
   (org-static-blog-get-body-text post-filename t) ; exclude headline!
   "]]></description>\n"
   (let ((categories ""))
     (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
       (dolist (tag (org-static-blog-get-tags post-filename))
         (setq categories (concat categories
                                  "  <category>" tag "</category>\n"))))
     categories)
   "  <link>"
   (concat org-static-blog-publish-url
           (file-name-nondirectory
            (org-static-blog-matching-publish-filename
             post-filename)))
   "</link>\n"
   "  <pubDate>"
   (format-time-string "%a, %d %b %Y %H:%M:%S %z" (org-static-blog-get-date post-filename))
   "</pubDate>\n"
   "</item>\n"))

(defun org-static-blog-assemble-rss-1 (rss-filename post-filenames)
  "Assemble the blog RSS feed.
The RSS-feed is an XML file that contains every blog post in a
machine-readable format."
  (let ((rss-items nil)
        (inhibit-redisplay t))
    (dolist (post-filename post-filenames)
      (let ((rss-date (org-static-blog-get-date post-filename))
            (rss-text (org-static-blog-get-rss-item post-filename)))
        (add-to-list 'rss-items (cons rss-date rss-text))))
    (org-static-blog-with-find-file
     rss-filename
     (erase-buffer)
     (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
             "<rss version=\"2.0\">\n"
             "<channel>\n"
             "<title>" org-static-blog-publish-title "</title>\n"
             "<description>" org-static-blog-publish-title "</description>\n"
             "<link>" org-static-blog-publish-url "</link>\n"
             "<lastBuildDate>" (format-time-string "%a, %d %b %Y %H:%M:%S %z" (current-time)) "</lastBuildDate>\n")
     (dolist (item (sort rss-items (lambda (x y) (time-less-p (car y) (car x)))))
       (insert (cdr item)))
     (insert "</channel>\n"
             "</rss>\n"))))

(defun org-static-blog-assemble-rss ()
  (org-static-blog-assemble-rss-1
   (concat org-static-blog-publish-directory org-static-blog-rss-file)
   (org-static-blog-get-post-filenames)))

(defun org-static-blog-assemble-tags ()
  "Render the tag archive and tag pages."
  (org-static-blog-assemble-tags-archive)
  (save-window-excursion
    (dolist (tag (org-static-blog-get-tag-tree))
      (let ((tag-label (downcase (car tag))))
        (org-static-blog-assemble-multipost-index
         (concat org-static-blog-publish-directory "tags/" tag-label ".html")
         (cdr tag)
         (concat "<h1 class=\"title\">Posts published under \""
                 tag-label
                 "\""
                 " <a class=\"rssbtn\" href=\"/rss/"
                 tag-label
                 ".xml\">rss</a></h1>"))
        (org-static-blog-assemble-rss-1
         (concat org-static-blog-publish-directory "rss/" tag-label ".xml")
         (cdr tag))))))

(defun org-static-blog-assemble-multipost-index-1 (post-filenames)
  (with-temp-buffer
    (dolist (post-filename post-filenames)
      (insert (org-static-blog-get-post-summary post-filename)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-static-blog-assemble-tags-archive ()
  "Assemble the blog tag archive page.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
  (let ((tags-archive-filename (concat org-static-blog-publish-directory org-static-blog-tags-file))
        (tag-tree (org-static-blog-get-tag-tree)))
    (org-static-blog-with-find-file
     tags-archive-filename
     (erase-buffer)
     (insert
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
      "\"https://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
      "<html xmlns=\"https://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n"
      "<head>\n"
      "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />\n"
      "<link rel=\"alternate\"\n"
      "      type=\"appliation/rss+xml\"\n"
      "      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"\n"
      "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
      "<title>" org-static-blog-publish-title "</title>\n"
      org-static-blog-page-header
      "</head>\n"
      "<body>\n"
      "<div id=\"preamble\" class=\"status\">"
      org-static-blog-page-preamble
      "</div>\n"
      "<div id=\"content\">\n"
      "<h1 class=\"title\">Tags</h1>\n")
     (dolist (tag (sort tag-tree (lambda (x y) (string-greaterp (car y) (car x)))))
       (insert "<h1 class=\"tags-title\">" (downcase (car tag)) "</h1>\n")
       (insert
        (org-static-blog-assemble-multipost-index-1
         (sort (cdr tag)
               (lambda (x y)
                 (not
                  (time-less-p (org-static-blog-get-date x)
                               (org-static-blog-get-date y))))))))
     (insert "</body>\n"
             "</html>\n"))))

(defun org-static-blog-assemble-multipost-index (pub-filename post-filenames &optional front-matter)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (org-static-blog-with-find-file
   pub-filename
   (erase-buffer)
   (insert
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
    "\"https://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
    "<html xmlns=\"https://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n"
    "<head>\n"
    "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />\n"
    "<link rel=\"alternate\"\n"
    "      type=\"appliation/rss+xml\"\n"
    "      href=\"" org-static-blog-publish-url org-static-blog-rss-file "\"\n"
    "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
    "<title>" org-static-blog-publish-title "</title>\n"
    org-static-blog-page-header
    "</head>\n"
    "<body>\n"
    "<div id=\"preamble\" class=\"status\">"
    org-static-blog-page-preamble
    "</div>\n"
    "<div id=\"content\">\n")
   (if front-matter
       (insert front-matter))
   (insert
    (org-static-blog-assemble-multipost-index-1
     (sort post-filenames
           (lambda (x y)
             (not
              (time-less-p (org-static-blog-get-date x)
                           (org-static-blog-get-date y)))))))))
