;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reduce file handler checks during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Prevent premature redisplay
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Prevent resize flashing
(setq frame-inhibit-implied-resize t)

;; Disable startup messages
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-scratch-message "")

;; Avoid multiple versions of the same packages being loaded
(setq package-enable-at-startup nil)

;; Set default frame parameters to prevent white flash
(setq default-frame-alist
      '(
        (background-color . "#282c34")  ; doom-one background
        (foreground-color . "#bbc2cf")  ; doom-one foreground
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (alpha-background . 100)
        (fullscreen . maximized)))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Prefer newer files
(setq load-prefer-newer t)

;;; early-init.el ends here
