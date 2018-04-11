(ht-update! op/default-template-parameters
            (ht
             ("blog-uri"  "{{doc-root}}/blog/index.html")
             ("wiki-uri"  "{{doc-root}}/wiki/index.html")
             ("tags-uri"  "{{doc-root}}/tags/index.html")
             ("about-uri" "{{doc-root}}/about/index.html")
             ("doc-root"  (or op/doc-root ""))))

(defun op/generate-tag-uri (tag-name)
  "Generate tag uri based on TAG-NAME."
  (concat (or op/doc-root "") "/tags/" (encode-string-to-url tag-name) "/"))

(setq op/category-config-alist
      '(("blog" ;; this is the default configuration
         :show-meta t
         :show-comment t
         :uri-generator op/generate-uri
         :uri-template "{{doc-root}}/blog/%y/%m/%d/%t/"
         :sort-by :date     ;; how to sort the posts
         :category-index t) ;; generate category index or not
        ("index"
         :show-meta nil
         :show-comment nil
         :uri-generator op/generate-uri
         :uri-template "{{doc-root}}/index.html"
         :sort-by :date
         :category-index nil)
        ("about"
         :show-meta nil
         :show-comment nil
         :uri-generator op/generate-uri
         :uri-template "{{doc-root}}/about/index.html"
         :sort-by :date
         :category-index nil)))

(defun op/update-category-index (file-attr-list pub-base-dir)
  "Update index page of different categories. FILE-ATTR-LIST is the list of all
file attribute property lists. PUB-BASE-DIR is the root publication directory."
  (let* ((sort-alist (op/rearrange-category-sorted file-attr-list))
         cat-dir)
    (mapc
     #'(lambda (cat-list)
         (unless (not (plist-get (cdr (or (assoc (car cat-list)
                                                 op/category-config-alist)
                                          (assoc "blog"
                                                 op/category-config-alist)))
                                 :category-index))
           (setq cat-dir (file-name-as-directory
                          (concat (file-name-as-directory pub-base-dir)
                                  (encode-string-to-url (car cat-list)))))
           (unless (file-directory-p cat-dir)
             (mkdir cat-dir t))
           (string-to-file
            (mustache-render
             (op/get-cache-create
              :container-template
              (message "Read container.mustache from file")
              (file-to-string (concat (op/get-template-dir) "container.mustache")))
             (let ((param-table (ht
                                 ("page-title" op/site-main-title)
                                 ("author" (or user-full-name "Unknown Author"))
                                 ("show-meta" nil)
                                 ("show-comment" nil)
                                 ("google-analytics" (and
                                                      (boundp
                                                       'op/personal-google-analytics-id)
                                                      op/personal-google-analytics-id))
                                 ("google-analytics-id" op/personal-google-analytics-id)
                                 ("creator-info" op/html-creator-string)
                                 ("email" (confound-email (or user-mail-address "")))
                                 ("doc-root" (or op/doc-root ""))
                                 ("cat-name" (capitalize (car cat-list))))))
               (ht ("header" (op/render-header param-table))
                   ("nav" (op/render-navigation-bar))
                   ("content" (op/render-content
                               "category-index.mustache"
                               (ht-update
                                param-table
                                (ht 
                                 ("posts" (mapcar
                                           #'(lambda (attr-plist)
                                               (ht-update
                                                param-table
                                                (ht ("date"
                                                     (plist-get
                                                      attr-plist
                                                      (plist-get
                                                       (cdr (or (assoc
                                                                 (plist-get attr-plist :category)
                                                                 op/category-config-alist)
                                                                (assoc
                                                                 "blog"
                                                                 op/category-config-alist)))
                                                       :sort-by)))
                                                    ("post-uri" (plist-get attr-plist :uri))
                                                    ("post-title" (plist-get attr-plist :title)))))
                                           (cdr cat-list)))))))
                   ("footer" (op/render-footer param-table)))))
            (concat cat-dir "index.html") 'html-mode)))
     sort-alist)))
