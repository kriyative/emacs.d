(dolist (prop '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)))
  (push prop default-frame-alist))

(put 'list-timers 'disabled nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq
 ;; don't make the following settings permanent
 ;; gc-cons-threshold most-positive-fixnum
 ;; gc-cons-percentage 0.6
 ;; raise the limit for bindings and unwind-protect contexts
 max-specpdl-size 5000
 package-enable-at-startup nil
 package-quickstart nil
 frame-inhibit-implied-resize t
 inhibit-splash-screen t
 inhibit-startup-message t
 backup-inhibited t
 truncate-partial-width-windows t
 visible-bell 'top-bottom
 transient-mark-mode t
 use-file-dialog nil
 custom-file "~/.emacs.d/custom.el"
 remote-shell-program "ssh"
 url-proxy-services nil
 compilation-scroll-output t
 ;; shell-file-name "bash"
 ;; completion-styles '(basic partial-completion)
 completion-styles '(basic partial-completion substring)
 completion-ignore-case t
 completion-cycle-threshold nil
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 completion-ignored-extensions (nconc completion-ignored-extensions
                                      '(".fasl"
                                        ".dfsl"
                                        ".x86f"
                                        ".err"
                                        ".ufasl"
                                        ".DS_Store"))
 auto-window-vscroll nil
 indent-tabs-mode nil
 native-comp-async-report-warnings-errors 'silent)
