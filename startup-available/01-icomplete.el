(rk-el-get-bundles oantolin/icomplete-vertical)

(use-package icomplete)

(use-package icomplete-vertical
  :after icomplete
  :config
  (define-key icomplete-minibuffer-map [?\C-f]  'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map [?\C-b]  'icomplete-backward-completions)
  (define-key icomplete-minibuffer-map [?\C-n]  'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map [?\C-p]  'icomplete-backward-completions))
