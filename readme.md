# Petmacs

---

## Prerequisite
Petmacs requires fonts for better display:

- Fonts

  - [Fira Code](https://github.com/tonsky/FiraCode)

  - [Fira Code Symbol](https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip)

  - [all the icons](https://github.com/domtronn/all-the-icons.el/tree/master/fonts)

- global tools

  - [ripgrep](https://github.com/BurntSushi/ripgrep) in your $PATH

- Python related Packages

```sh
pip install pylint yapf isort
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
