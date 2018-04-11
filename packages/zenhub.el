(require 'url)
(require 'json)

(defvar *pantheon-issues*
  (github-api-repository-issues "omnypay" "pantheon"))

(defvar *gh-repo-api* (gh-repos-api "github-repos-api"
                                    :sync nil
                                    :cache nil
                                    :num-retries 1))
  
(defvar *repo* (gh-repos-repo-get *gh-repo-api* "pantheon" "omnypay"))
(defvar *repo-id* (slot-value (slot-value *repo* :data) :id))

(defvar *zh-auth-token* "d2691954ed31923fa058dc2890df2e13f31638c1a3c92728995f7c2f9997b92a8e9d6a54ffbf0413")

(defun zh-request (end-point &optional method)
  (let ((url-request-extra-headers `(("X-Authentication-Token" . ,*zh-auth-token*))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "https://api.zenhub.io/p1" end-point))
      (goto-char (point-min))
      (url-http-parse-headers)
      (re-search-forward "^$")
      (let ((headers (list :content-type url-http-content-type
                           :content-length url-http-content-length
                           :transfer-encoding url-http-transfer-encoding
                           :version url-http-response-version
                           :status url-http-response-status
                           :method url-http-method
                           :extra-headers url-http-extra-headers)))
        (re-search-forward "^$")
        (values (if (string-match "json" url-http-content-type)
                    (let ((json-object-type 'plist)
                          (json-false nil))
                      (json-read))
                  (buffer-substring (point) (point-max)))
                headers)))))

(setq *epics* (multiple-value-bind (body _headers)
                  (zh-request (format "/repositories/%s/epics" *repo-id*))
                body))

(defmacro -> (x &rest args)
  "A Common-Lisp implementation of the Clojure `thrush` operator."
  (destructuring-bind (form &rest more)
      args
    (cond
     (more `(-> (-> ,x ,form) ,@more))
     ((and (consp form)
           (or (eq (car form) 'lambda)
               (eq (car form) 'function)))
      `(funcall ,form ,x))
     ((consp form) `(,(car form) ,x ,@(cdr form)))
     (form `(,form ,x))
     (t x))))

(let* ((e (-> *epics*
              (plist-get :epic_issues)
              (elt 0)))
       (repo-id (plist-get e :repo_id))
       (issue (plist-get e :issue_number)))
  (first (zh-request (format "/repositories/%s/epics/%s" repo-id issue))))
(:pipeline (:name "In Progress")
           :issues [(:pipeline (:name "Done") :repo_id 59687450 :is_epic nil :issue_number 1)
                    (:pipeline (:name "In Progress") :repo_id 59687450 :is_epic nil :issue_number 9)]
           :total_epic_estimates (:value 0))

