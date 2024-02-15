# Petmacs

before you use Petmacs, make sure your emacs version >= 29.1 and tree-sitter is enabled.

---

## Prerequisites

- Fonts

  - [Monego Ligatures](https://github.com/cseelus/monego)

  - [Maple Mono SC NF](https://gitee.com/subframe7536/Maple/releases)

  if you want use nerd icons in terminal, make sure [nerd fonts](https://github.com/ryanoasis/nerd-fonts) is used in your terminal.

- ripgrep

  put [ripgrep](https://github.com/BurntSushi/ripgrep) binary into your `PATH`

- nodejs

  put [nodejs](https://nodejs.org) binaries into your `PATH`

- rime

  install [rime](https://github.com/rime/librime)

``` bash
# use terra-pinyin for chinese input
sudo apt-get install librime-dev fcitx-rime
```

- vterm build tools

``` bash
sudo apt-get install cmake libtool-bin make
```

if your cmake is too old, download the suitable version from [cmake.org](https://cmake.org/download/)

- google-java-format (optional)

install [google-java-format](https://www.npmjs.com/package/google-java-format) if java is needed.

``` bash
# java should be >= 11
npm install -g google-java-format
# java 8
npm install -g google-java-format@1.7
```

---

## Install

### clone repository

```bash
git clone --depth=1 https://github.com/Peter-Chou/petmacs.git ~/.emacs.d
```

if you are facing some certification verification problems while cloning repository by git,
try use the following command and rerun emacs:

``` bash
export GIT_SSL_NO_VERIFY=1
```

### install LSP servers

install the language servers needed by LSP service

- python

``` sh
npm install -g pyright
```

set WORKON_HOME environment variable that has virtual environment directories.
Please make sure [black](https://github.com/psf/black) is installed in your environment to format buffer properly.

- C/C++

use `clangd` in [LLVM project](https://github.com/llvm/llvm-project). soft link llvm project to `/opt/llvm`.

- Golang

``` bash
go install golang.org/x/tools/gopls@latest
```

### install DAP servers

- python

``` bash
pip install debugpy
```

- C/C++

`lldb-vscode` is in LLVM project

- Golang

``` bash
go install github.com/go-delve/delve/cmd/dlv@latest
```

---

### other tools

``` bash
npm install -g readability-cli eslint_d prettier
```


### install dynamic libs for tree-sitter

run the following command in emacs.

``` emacs-lisp
M-x treesit-auto-install-all
```

## Screenshots

![dashboard](./data/pics/dashboard.png)

<br>

![markdown](./data/pics/markdown.png)

<br>

![elisp](./data/pics/elisp.png)
