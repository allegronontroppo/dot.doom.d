;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;(setq user-full-name "John Doe"
;      user-mail-address "john@doe.com")
(setq user-full-name "Shigeya Senda"
      user-mail-address "shigeya.s@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #character encode
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #mozc
(when (eq system-type 'gnu/linux)
  (setq default-input-method "japanese-mozc")
  )
;(when (eq system-type 'darwin)
;       ;; no need to set default-input-method on mac-port emacs
;  )
(when (eq system-type 'windows-nt)
  ;; https://nosubject.io/windows10-emacs-27-w32-ime/
  ;; tr-imeのDLLを自動でダウンロードしてくれる
  (tr-ime-advanced-install)
  ;; IM のデフォルトを IME に設定
  (setq default-input-method "W32-IME")
  ;; IME のモードライン表示設定
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))

  ;; IME 初期化
  (w32-ime-initialize)

  ;; IME 制御（yes/no などの入力の時に IME を off にする）
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)

  ;; IME の未確定文字列のフォント設定
  (set-frame-font "Meiryo UI-12" nil t)
  (modify-all-frames-parameters '((ime-font . "Meiryo UI-12")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rust
(defun download-rust-analyzer ()
  (interactive)
  (cond ((eq system-type 'windows-nt)
     (async-shell-command "curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-windows.exe -o ~/.local/bin/rust-analyzer.exe"))
    ((eq system-type 'darwin)
     (async-shell-command "curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-mac -o ~/.local/bin/rust-analyzer"))
    ((eq system-type 'gnu/linux)
     (async-shell-command "curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #company
(setq company-idle-delay 0.2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #font : https://ngkz.github.io/2018/10/doom-emacs-develop-setup/
;(setq doom-font (font-spec :family "HackGen Console" :size 14)
;      doom-variable-pitch-font (font-spec :family "HackGen")
;      doom-unicode-font (font-spec :family "HackGen Console")
;      doom-big-font (font-spec :family "HackGen Console" :size 22))
(setq doom-font (font-spec :family "NasuM" :size 16)
     doom-variable-pitch-font (font-spec :family "NasuM")
      doom-unicode-font (font-spec :family "NasuM")
      doom-big-font (font-spec :family "NasuM" :size 22))

;; コメントを明るく
(setq doom-one-brighter-comments t)

;; 行末の空白をハイライト
;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; Disable trailing whitespace highlighting when in some modes
;; https://qiita.com/tadsan/items/df73c711f921708facdc
(defun my/disable-trailing-mode-hook ()
  "Disable show tail whitespace."
  (setq show-trailing-whitespace nil))

(defvar my/disable-trailing-modes
  '(comint-mode
    eshell-mode
    eww-mode
    term-mode
    twittering-mode))

(mapc
 (lambda (mode)
   (add-hook (intern (concat (symbol-name mode) "-hook"))
             'my/disable-trailing-mode-hook))
 my/disable-trailing-modes)

;; タブ／全角スペース／ハードスペースを記号に変換して表示
;; highlight tab, hard space, and full-width space
(require 'whitespace)
(setq whitespace-style '(
    face
    tabs
    spaces
    space-mark
    tab-mark
))
(setq whitespace-display-mappings '(
    (space-mark ?\u3000 [?␣])       ;full-width space AMBIGUOUS WIDTH!
    (space-mark ?\u00A0 [?\uFF65])   ;hard space
    (tab-mark   ?\t     [?» ?\t])    ;tab
))
(setq whitespace-space-regexp "\\([\u3000]+\\)") ; highlight only full-width space
(global-whitespace-mode t)

;turn off auto-fill
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode -1)))

;soft wrapping
(global-visual-line-mode t)

;; minimal number of screen lines to keep above and below the cursor
(setq-default scroll-margin 3)
;; minimal number of screen columns to keep to the left and to the right of the cursor
(setq-default hscroll-margin 5)

;; xでyankを作らない
;; delete character without yanking
(map! :n "x" #'delete-char)

;; C-dをインサートモードで使えるようにする
;; Make C-d usable in insert mode
(map! :i "C-d" #'delete-char)

;; org-mode
;;
;; log when a certain todo item was finished
(setq org-log-done t)

; Make sure that the weekdays in the time stamps of your Org mode files and in the agenda appear in English.
(setq system-time-locale "C")

;; c-mode おすすめ設定 https://seesaawiki.jp/whiteflare503/d/Emacs%20%a5%a4%a5%f3%a5%c7%a5%f3%a5%c8
(defun my-c-mode-common-conf ()
  ;; (electric-mode + 自動インデント + 欲張り削除) ";"を押すと現在の行を
  ;; 再インデントして自動的に改行をするモードのなる設定。これは強力すぎて扱いづらい。
  ;; (c-toggle-auto-hungry-state 1)

  ;; (electric-mode) ";"や"{"などをを入力した場合現在の行を自動インデントを有功にする
  ;; (c-toggle-electric-state 1)

  ;; (欲張り削除 + electric-mode)バックスペースなどの削除するキーを押すと
  ;; スペースを一気に消す欲張り削除機能とelecetic-modeをを有功にする
  (c-toggle-hungry-state 1)

  ;; この関数は廃れた機能 (obsoleteされた)ものなので、emacsのバージョンが22.1以上なら使わないこと
  ;; (c-toggle-auto-state 1)　obsoleted

  ;; (自動インデント) 改行をしたら次の行を自動でインデントしてくれる
  ;; (c-toggle-auto-newline 1)

  ;; C-m を newline-and-indentに設定する
  ;; しかしこれをするとEnterのキーマップまで影響するので
  ;; 大人しくC-jを使ったnewline-and-indentを使うほうが
  ;; (define-key c-mode-base-map ""\C-m" 'newline-and-indent)

  ;(c-set-style "stroustrup")                  ;; スタイルはストラウストラップ
  ;(c-set-style "cc-mode")                     ;; スタイルはcc-mode indent=4
  (c-set-style "linux")                     ;; スタイルはcc-mode indent=4
  ;(flyspell-prog-mode)                        ;; flyspell-prog-mode(自動ispell機能)
  (show-paren-mode t)                         ;; カッコを強調表示する

  ;; 他のエディタなどがファイルを書き換えたらすぐにそれを反映する
  ;; auto-revert-modeを有効にする
  (auto-revert-mode)
  )

;; C言語系のモード全てに適用されるフック
;; c-mode-common-hookにフックを設定する
(add-hook 'c-mode-common-hook 'my-c-mode-common-conf)


;; cua-mode 矩形編集開始：C-c u
;;    https://d.akinori.org/2012/06/14/emacsの範囲選択uiを大幅拡張してくれるcua-mode/
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; cua-mode の不要なキーバインドは除去
;;(define-key global-map (kbd "C-RET") 'cua-set-rectangle-mark)
(bind-key "C-c u" 'cua-set-rectangle-mark) ;; C-c uのあと、Shift+→とかで範囲選択
;; -- 矩形選択中のキーバインド --
;;  M-p	矩形の幅を固定
;;  M-b	空白文字で埋める。 open-rectangle と同等
;;  M-s	文字列で置き換える。 string-rectangle と同等
;;  M-f	1種類の文字で埋める。 string-rectangle で1文字指定したときと同等
;;  M-i	矩形領域内の数字をインクリメントする
;;  M-n	矩形領域を連番で埋める。フォーマット指定可

;; more colors in emacs terminals : need package eterm-256color
;;(add-hook 'term-mode-hook #'eterm-256color-mode)

;; key bind
;;
(bind-key "M-n" 'evil-normal-state)
(bind-key "g@" 'set-mark-command     evil-normal-state-map)

;; use tab-bar-mode
(tab-bar-mode +1)

;; I love lisp-interaction-mode.
(setq initial-major-mode 'lisp-interaction-mode)

;; load directory
;;     see https://www.emacswiki.org/emacs/LoadingLispFiles
(defun load-directory (dir)
  (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
          (mapc load-it (directory-files dir nil "\\.el$"))))

;; load .el files in private/lisp
(let ((ld (expand-file-name "~/.doom.d/lisp")))
   (when (file-directory-p ld)
       (load-directory ld)
   ))

;; M-x calculator
;;    https://qiita.com/hazudojins/items/f117ba23aa427ac84a9f
(add-hook 'calculator-mode-hook
          '(lambda () (enlarge-window 3)))

;; end of config.el
