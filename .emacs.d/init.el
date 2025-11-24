;;; init.el --- Optimized Emacs Configuration -*- lexical-binding: t -*-


;; Basic UI settings (moved from early-init if needed)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(add-to-list 'default-frame-alist '(font . "jetbrainsmono nerd font-20"))


;; Enable desktop save mode for session persistence
(desktop-save-mode 1)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (run-with-timer 0.3 nil
                            (lambda (frame)
                              (set-frame-size frame 80 100))
                            frame)))

;; Directory where desktop files are saved
(setq desktop-dirname "~/.emacs.d/desktop/")
(setq desktop-path (list desktop-dirname))

;; Create directory if it doesn't exist
(unless (file-directory-p desktop-dirname)
  (make-directory desktop-dirname t))

;; Load first 10 buffers immediately, rest lazily
(setq desktop-restore-eager 10)

;; Save desktop periodically
(setq desktop-auto-save-timeout 300)  ; 5 minutes

;; Don't ask about restoring desktop
(setq desktop-restore-in-current-display t)
(setq desktop-load-locked-desktop t)

;;; Straight Package Manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Install Use-Package
(straight-use-package 'use-package)

(use-package el-patch
  :straight t
  :defer t)

(use-package which-key
  :straight t
  :defer 0.2
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5
        which-key-sort-order 'which-key-key-order-alpha))

(use-package ef-themes
  :straight t
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-bio))

;;; Gptel

;; make .authinfo.gpg file in home directory
;; write machine gemini login apikey password YOUR_API_KEY in
;; .authinfo.gpg file

(defun my-get-api-key-secure ()
  "Read an API key securely from auth-source."
    (require 'auth-source)
    (let ((info (car (auth-source-search :host "gemini"))))
    (when info
	(funcall (plist-get info :secret)))))

(use-package gptel
  :straight t
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :init
  (setq gptel-model 'gemini-2.5-pro-exp-03-25
        gptel-backend (gptel-make-gemini "Gemini"
			:key (my-get-api-key-secure)
			:stream t)))


;;; Markdown Mode - defer until opening .md files
(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package rotate
  :straight t
  :defer t)


;;; Clipboard integration with wl-copy (Wayland)
(setq select-enable-clipboard t
      select-enable-primary t)

;; Use wl-copy for clipboard in terminal Emacs
(unless window-system
  (when (executable-find "wl-copy")
    (defun wl-copy-handler (text)
      "Copy TEXT to clipboard using wl-copy."
      (let ((process-connection-type nil))
        (let ((proc (start-process "wl-copy" nil "wl-copy")))
          (process-send-string proc text)
          (process-send-eof proc))))
    
    (setq interprogram-cut-function 'wl-copy-handler)))


;;; Vertico - load early for completion
(use-package vertico
  :straight t
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t))

;;; Orderless - load with vertico
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; Marginalia - load with vertico
(use-package marginalia
  :straight t
  :init
  (marginalia-mode 1))

;;; Consult - defer but preload for common commands
(use-package consult
  :straight t
  :defer 0.1
  :config
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden")
  (setq consult-fd-args "fd --color=never --full-path --hidden --no-ignore --type f --exclude .git"))

;;; Evil-Mode - load early since it's core to workflow
(use-package evil
  :straight (:wait t)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (add-hook 'minibuffer-setup-hook 'evil-insert-state)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Custom functions for home directory search
  (defun my/consult-fd-home ()
    "Run consult-fd from home directory."
    (interactive)
    (let ((default-directory "/home/tudu/"))
      (consult-fd)))
  
  (defun my/consult-ripgrep-home ()
    "Run consult-ripgrep from home directory."
    (interactive)
    (let ((default-directory "/home/tudu/"))
      (consult-ripgrep)))
  
  ;;; KEYBINDINGS
  
  ;; Dired keybindings
  (evil-define-key 'normal 'global (kbd "<leader>e") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader>E") 'dired)
  
  ;; File finding
  (evil-define-key 'normal 'global (kbd "<leader>fh") 'my/consult-fd-home)
  (evil-define-key 'normal 'global (kbd "<leader>fc") 'consult-fd)
  
  ;; Grep/Search
  (evil-define-key 'normal 'global (kbd "<leader>gh") 'my/consult-ripgrep-home)
  (evil-define-key 'normal 'global (kbd "<leader>gc") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>ss") 'consult-line)
  
  ;; Recent files
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-recent-file)
  
  ;; Buffers
  (evil-define-key 'normal 'global (kbd "<leader>bl") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bp") 'previous-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bn") 'next-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bD") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bm") 'ibuffer)

  ;; Git
  (evil-define-key 'normal 'global (kbd "<leader>gf") 'consult-git-grep)
  
  ;; Window navigation
  (evil-define-key 'normal 'global (kbd "H") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "J") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "K") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "L") 'evil-window-right))

;; ibuffer config
(use-package ibuffer
  :ensure nil
  :defer t
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil))

;;; Window split and Window Rotate
;; Install hydra package
(use-package hydra
  :straight t
  :defer t)

;; Define hydra for window management
(with-eval-after-load 'hydra
  (defhydra hydra-window (:color red :hint nil)
    "
^Split^         ^Resize^        ^Rotate^        ^Scroll^        ^Other^
^^^^^^^^-----------------------------------------------------------------
_v_: vertical   _=_: enlarge    _r_: rotate     _j_: down       _d_: delete
_h_: horizontal _-_: shrink     _l_: layout     _k_: up         _x_: delete others
^ ^             _]_: widen      ^ ^             ^ ^             _q_: quit
^ ^             _[_: narrow     ^ ^             ^ ^             ^ ^
"
    ;; Split
    ("v" evil-window-vsplit)
    ("h" evil-window-split)
    
    ;; Resize (these can be repeated)
    ("=" enlarge-window)
    ("-" shrink-window)
    ("]" enlarge-window-horizontally)
    ("[" shrink-window-horizontally)
    
    ;; Rotate (these can be repeated)
    ("r" rotate-window)
    ("l" rotate-layout)
    
    ;; Scroll (can be repeated)
    ("j" evil-scroll-down)
    ("k" evil-scroll-up)
    
    ;; Other
    ("d" evil-window-delete)
    ("x" delete-other-windows :exit t)
    ("q" nil :exit t)))

;; Bind to leader key
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>w") 'hydra-window/body))


;;; Gptel keybindings
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>as") 'gptel-send)
  (evil-define-key 'normal 'global (kbd "<leader>am") 'gptel-menu)
  (evil-define-key 'normal 'global (kbd "<leader>an") 'gptel))

;;; Reload Config
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>rr") 
    (lambda () (interactive) (load-file user-init-file))))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-collection-init)
    (dolist (mode '(dired-mode-map 
                  ibuffer-mode-map 
                  help-mode-map
                  debugger-mode-map))
    (when (boundp mode)
      (evil-define-key 'normal (symbol-value mode) (kbd "SPC") nil))))

(use-package evil-escape
  :straight t
  :defer 0.2
  :after evil
  :config
  (evil-escape-mode 1)
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-delay 0.3))


(use-package evil-leader
  :straight t
  :defer 0.2
  :after evil
  :config
  ;; Set the leader key to Space
  (setq evil-leader/leader "<SPC>")
  (global-evil-leader-mode 1))

;;; ----- Appearance -----

(defun dw/set-terminal-title (title)
  (send-string-to-terminal (format "\e]0;%s\a" title)))

(defun dw/clear-background-color (&optional frame)
  (interactive)
  (or frame (setq frame (selected-frame)))
  "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    (send-string-to-terminal
     (format "\033]11;[90]%s\033\\"
         (face-attribute 'default :background)))
    (set-face-background 'default "unspecified-bg" frame)))

;; Clear background for transparent terminals
(unless (display-graphic-p)
  (add-hook 'after-make-frame-functions 'dw/clear-background-color)
  (add-hook 'window-setup-hook 'dw/clear-background-color))

;; GUI-specific settings
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font "jetbrainsmono nerd font"
                      :weight 'normal
                      :height 220)
  (set-face-attribute 'fixed-pitch nil
                      :font "jetbrainsmono nerd font"
                      :weight 'normal
                      :height 220)
  (set-face-attribute 'variable-pitch nil
                      :font "jetbrainsmono nerd font"
                      :height 220
                      :weight 'normal))

;; Window separator
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ? ))

;;; Nerd-icons - defer until needed
(use-package nerd-icons
  :straight t
  :defer t)

(use-package nerd-icons-dired
  :straight t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Spacious-padding - defer
(use-package spacious-padding
  :straight t
  :defer t)

;;; Custom Mode Line
(setq-default mode-line-format
	      '("%e"
		my-modeline-buffer-name
		"  "
		my-modeline-major-mode
		"  "
		my-modeline-evil-state))

(defface my-modeline-background
  '((t :background "#3355bb" :foreground "white" :inherit bold))
  "Face with a background for use on the mode line.")

(defun my-modeline-buffer-name ()
  "Return buffer-name with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local my-modeline-buffer-name
  '(:eval
    (format "%s"
	    (propertize (my-modeline-buffer-name) 'face 'my-modeline-background)))
  "Mode line construct to display the buffer name")

(put 'my-modeline-buffer-name 'risky-local-variable t)

(defun my-modeline-major-mode-name ()
  "Return capitalized major-mode with icon."
  (let ((icon (if (fboundp 'nerd-icons-icon-for-mode)
                  (nerd-icons-icon-for-mode major-mode)
                ""))
        (mode-name (capitalize (symbol-name major-mode))))
    (format "%s %s" icon mode-name)))

(defvar-local my-modeline-major-mode
    '(:eval
        (format "%s"
                (propertize (my-modeline-major-mode-name) 'face 'bold)))
  "Mode line construct to display the major mode.")

(put 'my-modeline-major-mode 'risky-local-variable t)

(defface my-modeline-evil-normal
  '((t :background "#00aa00" :foreground "white" :inherit bold))
  "Face for Evil normal state.")

(defface my-modeline-evil-insert
  '((t :background "#ff6600" :foreground "white" :inherit bold))
  "Face for Evil insert state.")

(defface my-modeline-evil-visual
  '((t :background "#cc00cc" :foreground "white" :inherit bold))
  "Face for Evil visual state.")

(defface my-modeline-evil-replace
  '((t :background "#ff0000" :foreground "white" :inherit bold))
  "Face for Evil replace state.")

(defun my-modeline-evil-state ()
  "Return the current Evil state as a propertized string."
  (when (bound-and-true-p evil-mode)
    (let ((state (evil-state-property evil-state :tag t)))
      (cond
       ((eq evil-state 'normal)
        (propertize " NORMAL " 'face 'my-modeline-evil-normal))
       ((eq evil-state 'insert)
        (propertize " INSERT " 'face 'my-modeline-evil-insert))
       ((eq evil-state 'visual)
        (propertize " VISUAL " 'face 'my-modeline-evil-visual))
       ((eq evil-state 'replace)
        (propertize " REPLACE " 'face 'my-modeline-evil-replace))
       (t (propertize (format " %s " (upcase (symbol-name evil-state))) 'face 'my-modeline-background))))))

(defvar-local my-modeline-evil-state
  '(:eval (my-modeline-evil-state))
  "Mode line construct to display Evil state.")

(put 'my-modeline-evil-state 'risky-local-variable t)

;;; Reset GC after startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024)
          gc-cons-percentage 0.1)
    ;; Run GC when idle
    (run-with-idle-timer 5 t #'garbage-collect)))

(custom-set-variables
 '(package-selected-packages '(gptel)))
(custom-set-faces)

;;; init.el ends here
