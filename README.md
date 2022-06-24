# Petmacs

---

## Prerequisite

- Fonts

  - [Monego Ligatures](https://github.com/cseelus/monego)

- icons

  - [all the icons](https://github.com/domtronn/all-the-icons.el/tree/master/fonts)

- global tools

  - put [ripgrep](https://github.com/BurntSushi/ripgrep) binary into your $PATH

- nodejs

  - put [nodejs](https://nodejs.org) binaries into your $PATH

---

## Install

```bash
git clone --depth=1 https://github.com/Peter-Chou/petmacs.git ~/.emacs.d
```

install git submodules:

``` bash
cd ~/.emacs.d

git submodule update --init --recursive

# udpate submodules to latest version
# git submodule update --remote --merge
```

### install tools

- eaf

``` bash
conda create -n eaf python=3.8
conda activate eaf
# install browser image-viewer jupyter markdown-previewer org-previewer pdf-viewer
python site-lisp/emacs-application-framework/install-eaf.py
```

- nodejs packages

```sh
npm install -g eslint_d prettier markdownlint-cli
```

### install prebuilt language servers

- C/C++

lsp use clangd in [LLVM project](https://github.com/llvm/llvm-project).
lldb-vscode for debug is also in LLVM project

- python

``` sh
npm install -g pyright
```

---

## Screenshots

![dashboard](./resources/pics/dashboard.png)

![markdown](./resources/pics/markdown.png)

![elisp](./resources/pics/elisp.png)
