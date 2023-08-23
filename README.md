# Petmacs

before you use Petmacs, make sure your emacs version >= 29.1 and tree-sitter is enabled.

---

## Prerequisites

- Fonts

  - [Monego Ligatures](https://github.com/cseelus/monego)

  - [Maple Mono SC NF](https://gitee.com/subframe7536/Maple/releases)

  - [all-the-icons](https://github.com/domtronn/all-the-icons.el)

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

### install tools

- eaf [Optional]

``` bash
# in .emacs.d folder
git submodule update --init --recursive

# update submodules to latest version
# git submodule update --remote --merge
```

``` bash
conda create -n eaf python=3.8
conda activate eaf

# install browser image-viewer jupyter markdown-previewer org-previewer pdf-viewer git
python site-lisp/emacs-application-framework/install-eaf.py
```

### install LSP servers

install the language servers needed by LSP service

- python

``` sh
npm install -g pyright
```

set WORKON_HOME environment variable that has virtual environment directories.
Please make sure [yapf](https://github.com/google/yapf) is installed in your environment to format buffer properly.

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

``` emacs-lisp
M-x treesit-auto-install-all
```

## Screenshots

![dashboard](./data/pics/dashboard.png)

<br>

![markdown](./data/pics/markdown.png)

<br>

![elisp](./data/pics/elisp.png)
