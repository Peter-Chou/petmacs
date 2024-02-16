# Petmacs

before you use Petmacs, make sure your emacs version >= 29.1 and tree-sitter is enabled.

---

## 1. Prerequisites

### 1.1 Fonts

- [Monego Ligatures](https://github.com/cseelus/monego)

- [Maple Mono SC NF](https://gitee.com/subframe7536/Maple/releases)

if you want use nerd icons in terminal, make sure [nerd fonts](https://github.com/ryanoasis/nerd-fonts) is used in your terminal.

### 1.2 ripgrep

install [ripgrep](https://github.com/BurntSushi/ripgrep) and put it into your `PATH`

### 1.3 fd

install [fd](https://github.com/sharkdp/fd) and put it into your `PATH`

### 1.4 nodejs

put [nodejs](https://nodejs.org) binaries into your `PATH`

### 1.5 rime

install [rime](https://github.com/rime/librime)

``` bash
# use terra-pinyin for chinese input
sudo apt-get install librime-dev fcitx-rime
# install build tools
sudo apt-get install cmake libtool-bin make
```

if your cmake is too old, download the suitable version from [cmake.org](https://cmake.org/download/)

---

## 2. Install

### 2.1 clone repository

```bash
git clone --depth=1 https://github.com/Peter-Chou/petmacs.git ~/.emacs.d
```

if you are facing some certification verification problems while cloning repository by git,
try use the following command and rerun emacs:

``` bash
export GIT_SSL_NO_VERIFY=1
```

### 2.2 install dynamic libs for tree-sitter

run the following command in emacs.

``` emacs-lisp
M-x treesit-auto-install-all
```

## 3. install LSP servers

install the language servers needed by LSP service

### 3.1 pyright (Python)

``` sh
npm install -g pyright
```

set WORKON_HOME environment variable that has virtual environment directories.

### 3.2 clangd (C/C++)

use `clangd` in [LLVM project](https://github.com/llvm/llvm-project). soft link llvm project to `/opt/llvm`.

### 3.3 gopls (Golang)

``` bash
go install golang.org/x/tools/gopls@latest
```

## 4. install DAP servers

### 4.1 debugpy (Python)

``` bash
pip install debugpy
```

### 4.2 lldb-vscode (C/C++)

`lldb-vscode` is already in [LLVM project](https://github.com/llvm/llvm-project).

### 4.3 delve (Golang)

``` bash
go install github.com/go-delve/delve/cmd/dlv@latest
```


## 5. install formatter

### 5.1 black (Python)

install [black](https://github.com/psf/black) is installed in your environment.

``` bash
pip install black
```

### 5.2 google-java-format (Java)

``` bash
# for java >= 11
npm install -g google-java-format
# for java 8
npm install -g google-java-format@1.7
```

### 5.3 clang-format (C/C++)

`clang-format` is already in [LLVM project](https://github.com/llvm/llvm-project).

---

## Screenshots

![dashboard](./data/pics/dashboard.png)

<br>

![markdown](./data/pics/markdown.png)

<br>

![elisp](./data/pics/elisp.png)
