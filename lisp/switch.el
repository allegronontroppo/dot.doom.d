;; mac-ime.el
;;

(when (eq system-type 'darwin)
;; from : https://gist.github.com/penn201500/fd445603ea05faef4c9f5b2e102613ad

;; switch to english input method when switching to normal mode
;; and switch back when entering insert/replace modes
;; need external script support, currently mac-only
(defvar default-im "com.google.inputmethod.Japanese.Roman" "Default ascii-only input method")
(defvar prev-im (substring (shell-command-to-string "/usr/local/bin/im-select") 0 -1)
  "IM that I use when starting Emacs and exiting insert mode")
(defvar ims-cmd "/usr/local/bin/im-select" "im-select command path")
(defvar use-im-remember t "Remember the input method method being used or not.")

(defun toggle-im-remember ()
  "Toggle im-memeber."
       (interactive)
       (setq use-im-remember (not use-im-remember)))

(when (boundp 'mac-input-method-parameters)
      (defun im-use-english ()
        "Switch to english input method on a Mac. im-select is a tool
provided at https://github.com/daipeihust/im-select"
        (interactive)
        (cond ((eq system-type 'darwin)
               (call-process-shell-command (concat ims-cmd " " default-im)))))

      (defun im-remember ()
        "Remember the input method being used in insert mode,
so we can switch to it in other modes."
        (interactive)
        (cond ((eq system-type 'darwin)
               (setq prev-im (if use-im-remember
                                 (substring (shell-command-to-string ims-cmd) 0 -1)
                               nil)))))

      (defun im-remember-eng ()
        "Remember the input method being used in hybrid mode,
so we can switch to it in other modes."
        (interactive)
        (cond ((eq system-type 'darwin)
               (setq prev-im (if use-im-remember
                                 (substring (shell-command-to-string ims-cmd) 0 -1)
                               nil))
               (call-process-shell-command (concat ims-cmd " " default-im))
               )))

     (defun im-use-prev ()
        "Use previous input method.
If previous input method is not defined, use default method"
        (interactive)
        (cond ((eq system-type 'darwin)
               (if prev-im
                   (call-process-shell-command (concat ims-cmd " " prev-im))
                 (call-process-shell-command (concat ims-cmd " " default-im))))))
      )

(when (fboundp 'mac-input-source)
    (defun im-use-english ()
      "Switch to english input method on a Mac. im-select is a tool
provided at https://github.com/daipeihust/im-select"
      (interactive)
      (cond ((eq system-type 'darwin)
             (mac-select-input-source default-im))))

    (defun im-remember ()
      "Remember the input method being used in insert mode,
so we can switch to it in other modes."
      (interactive)
      (cond ((eq system-type 'darwin)
             (setq prev-im (if use-im-remember (mac-input-source) nil)))))

    (defun im-remember-eng ()
      "Remember the input method being used in hybrid mode,
so we can switch to it in other modes."
      (interactive)
      (cond ((eq system-type 'darwin)
             (setq prev-im (if use-im-remember (mac-input-source) nil))
             (mac-select-input-source default-im)
             )))

    (defun im-use-prev ()
      "Use previous input method.
If previous input method is not defined, use default method"
      (interactive)
      (cond ((eq system-type 'darwin)
             (if prev-im
                 (mac-select-input-source prev-im)
               (mac-select-input-source default-im)))))
    )

(when (fboundp 'im-use-english)
  (add-hook 'evil-normal-state-entry-hook  'im-use-english)
  (when (boundp 'evil-hybrid-state-entry-hook)	;; for spacemacs
    (add-hook 'evil-hybrid-state-entry-hook  'im-use-prev)
    (add-hook 'evil-hybrid-state-exit-hook   'im-remember-eng))
  (add-hook 'evil-insert-state-entry-hook  'im-use-prev)
  (add-hook 'evil-insert-state-exit-hook   'im-remember)
  (add-hook 'evil-replace-state-entry-hook 'im-use-prev)
  (add-hook 'evil-replace-state-exit-hook  'im-remember)
  (add-hook 'evil-emacs-state-entry-hook   'im-use-english))

(when (boundp 'mac-input-method-parameters)  ;; for cask emacs
  ;; from https://suzuki.tdiary.net/20160103.html
  (defun suzuki-mac-input-method-decorate (roman-source japanese-source)
    (mac-set-input-method-parameter japanese-source 'title "あ")
    (mac-set-input-method-parameter japanese-source 'cursor-color "red")
    (mac-set-input-method-parameter roman-source 'cursor-color "blue")
    )
  (setq default-input-method "MacOSX")
  (cond
   ;; ことえり
   ((string-match "com.apple.inputmethod.Kotoeri" (mac-get-current-input-source))
    (suzuki-mac-input-method-decorate
     "com.apple.inputmethod.Kotoeri.Roman"
     "com.apple.inputmethod.Kotoeri.Japanese"))
   ;; Google 日本語入力
   ((string-match "com.google.inputmethod.Japanese" (mac-get-current-input-source))
    (suzuki-mac-input-method-decorate
     "com.google.inputmethod.Japanese.Roman"
     "com.google.inputmethod.Japanese.base"))))

(if (fboundp 'mac-input-source)  ;; for EMP(emacs mac port) version
    ;; https://club.jidaikobo.com/knowledge/132.html
     (progn
       (defun my-mac-selected-keyboard-input-source-chage-function ()
         (let ((mac-input-source (mac-input-source)))
           (set-cursor-color
            (if (or (string-match "com.apple.inputmethod.Kotoeri.Roman" mac-input-source)
                    (string-match "com.apple.keylayout.US" mac-input-source)
                    (string-match "com.google.inputmethod.Japanese.Roman" mac-input-source)) ;; google ime
                "white" "Red"))))
       ;; hook for cursor
       (add-hook 'mac-selected-keyboard-input-source-change-hook
                 'my-mac-selected-keyboard-input-source-chage-function)
       ;; timer for cursor
       (setq-default global-cursor-color-timer
                     (run-with-idle-timer
                      0.03
                      t
                      'my-mac-selected-keyboard-input-source-chage-function)))
   ;; if not mac
   (set-cursor-color "white"))

;; cut&paste setting for emacs on MAC
;;    https://apple.stackexchange.com/questions/85222/configure-emacs-to-cut-and-copy-text-to-mac-os-x-clipboard
;;    システムのペーストバッファとのやり取りは別のキーバインドで。
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c C-c") 'pbcopy)
(global-set-key (kbd "C-c C-v") 'pbpaste)
(global-set-key (kbd "C-c x")   'pbcut)
(global-set-key (kbd "C-c C-t") 'toggle-im-remember)
) ;; when darwin

(when (eq system-type 'windows-nt)
  (set-cursor-color "white")
  (defun im-deactivated () (set-cursor-color "white"))
  (defun im-activated ()   (set-cursor-color "red"))
  (add-hook 'w32-ime-on-hook  'im-activated)
  (add-hook 'w32-ime-off-hook 'im-deactivated)
  )

(provide 'mac-ime)
;;;mac-ime.el ends here
