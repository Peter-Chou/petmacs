# Petmacs

---

## Prerequisite
Petmacs requires fonts for better display:


- Fonts

  - [Fira Code](https://github.com/tonsky/FiraCode)

- icons

  - [all the icons](https://github.com/domtronn/all-the-icons.el/tree/master/fonts)

- global tools

  - [ripgrep](https://github.com/BurntSushi/ripgrep) in your $PATH

- npm

``` bash
sudo apt install nodejs npm
```

- git modules

``` bash
git submodule update --init --recursive
```

``` bash
conda create -n eaf python=3.8
conda activate eaf
python site-lisp/emacs-application-framework/install-eaf.py
```

- C/C++ packages

lsp use clangd in LLVM project
dap use lldb-vscode in LLVM/lldb/tools/lldb-vscode (https://github.com/llvm-mirror/lldb/tree/master/tools/lldb-vscode)

- nodejs packages

```sh
npm install -g eslint_d prettier markdownlint-cli vmd
```

use mirror if you have timeout error.

---

## Install

```bash
git clone https://github.com/Peter-Chou/petmacs.git ~/.emacs.d
```

---

## Screenshots

![petmacs](./resources/img/screenshot_dashboard.png)

![screenshot](./resources/img/screenshot_elisp.png)
