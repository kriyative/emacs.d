(rk-require-packages lsp-java)

(defun rk--java-mode-hook ()
  (setq indent-tabs-mode nil))

(defun rk--lsp-ui-mode-hook ()
  (lsp-ui-sideline-mode -1)
  (lsp-ui-doc-mode -1))

(add-hook 'java-mode-hook #'rk--java-mode-hook)

;; (use-package lsp-java
;;   :init
;;   (let* ((lombok-version "1.18.8")
;;          (lombok-jar-path (expand-file-name
;;                            (concat "~/.m2/repository/org/projectlombok/lombok/"
;;                                    lombok-version
;;                                    "/lombok-"
;;                                    lombok-version
;;                                    ".jar"))))
;;     (setq lsp-java-vmargs `("-noverify"
;;                             "-XX:+UseParallelGC"
;;                             "-XX:GCTimeRatio=4"
;;                             "-XX:AdaptiveSizePolicyWeight=90"
;;                             "-Dsun.zip.disableMemoryMapping=true" 
;;                             "-Xmx8G"
;;                             ;; "-XX:+UseG1GC"
;;                             ;; "-XX:+UseStringDeduplication"
;;                             ,(concat "-javaagent:" lombok-jar-path))))
;;   :config
;;   (add-hook 'java-mode-hook #'rk--java-mode-hook)
;;   ;; (add-hook 'java-mode-hook #'lsp)
;;   ;; (remove-hook 'java-mode-hook #'lsp)
;;   (add-hook 'lsp-ui-mode-hook #'rk--lsp-ui-mode-hook)
  
;;   (define-key java-mode-map (kbd "C-c C-f C-d") 'flymake-show-diagnostics-buffer)
;;   (add-to-list 'display-buffer-alist
;;                '("\\*Flymake diagnostics" display-buffer-below-selected)))

;; (use-package dap-java :ensure nil)
