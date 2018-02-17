(require 'elnode)

(defun my-elnode-hello-world-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return 
   httpcon 
   (format "<html><body><h1>Hello World %s</h1></body></html>"
           (current-time-string))))

(add-to-list 'elnode-hostpath-default-table
             '("/hello/" . my-elnode-hello-world-handler))

