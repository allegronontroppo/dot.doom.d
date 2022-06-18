# dot.doom.d

## emacs のインストール

### windows
msys64 をインストールしてパッケージとして emacs をインストールする。
``` sh
$ pacman -S mingw-w64-x86_64-emacs
```
#### スタートアップへの emacs の登録
C:\msys64\mingw64\bin\runemacs.exe へのシンボリックリンクを作成する。

### mac
mac port emacs (EMP)をインストールする。
[https://github.com/railwaycat/homebrew-emacsmacport

## doom emacs のインストール

### 必要なモジュールのインストール
- git , ripgrep, fd

### doom 本体

``` sh
$ git clone https://github.com/hlissner/doom-emacs .emacs.d
$ ~/.emacs.d/bin/doom install
```

### 設定ファイル
.doom.d をこの git で提供する。

``` sh
$ git clone https://github.com/allegronontroppo/dot.doom.d.git
```


## リンク
- [cheet-sheet](https://naghdbishi.ir/Doom-Emacs-Cheat-Sheet/README.html)
- [DoomCasts](https://www.youtube.com/playlist?list=PLhXZp00uXBk4np17N39WvB80zgxlZfVwj)
- [Projectile Document](https://docs.projectile.mx/projectile/projects.html)

