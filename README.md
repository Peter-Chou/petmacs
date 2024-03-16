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

---

## 3. install LSP servers

install the language servers needed by LSP service

### 3.1 pyright (Python)

``` sh
npm install -g pyright
```

set `WORKON_HOME` environment variable that has virtual environment directories.


### 3.2 eclipse jdtls (Java)

M-x: `lsp-install-server` to install eclipse jdtls

Please make sure  java 17 is installed in `/opt/jdk17` folder

### 3.3 clangd (C/C++)

use `clangd` in [LLVM project](https://github.com/llvm/llvm-project). soft link llvm project to `/opt/llvm`.

or you can build it from source

``` bash
sudo apt-get install build-essential lld

llvm_version=llvmorg-18.1.1
git clone -b $llvm_version --depth=1 https://gitee.com/mirrors/LLVM.git
cd LLVM
cmake -S llvm -B build -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=lld -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra;lldb" -DCMAKE_INSTALL_PREFIX=/opt/llvm
cmake --build build -j $(nproc)
sudo $(which cmake) --install build
```

---

## 4. install DAP servers

### 4.1 debugpy (Python)

``` bash
pip install debugpy
```

### 4.2 java-debug (Java)

java-debug already been carried in `./data/lsp-java-jars/com.microsoft.java.debug.plugin-0.51.1.jar`

### 4.3 gdb (C/C++)

gdb >=14.1

#### build gmp for gdb

``` bash
wget https://ftp.gnu.org/gnu/gmp/gmp-6.3.0.tar.gz
tar -vxf gmp-6.3.0.tar.gz
cd gmp-6.3.0
./configure --prefix=/opt/softwares/gmp-6.3.0
make  -j $(nproc)
sudo make install
```

#### build mpfr for gdb

``` bash
wget https://ftp.gnu.org/gnu/mpfr/mpfr-4.2.1.tar.gz
tar -vxf mpfr-4.2.1.tar.gz
cd mpfr-4.2.1
./configure --prefix=/opt/softwares/mpfr-4.2.1
make  -j $(nproc)
sudo make install
```

#### build gdb

``` bash
sudo apt install python-dev-is-python3

wget https://ftp.gnu.org/gnu/gdb/gdb-14.2.tar.gz
tar -vxf gdb-14.2.tar.gz
cd gdb-14.2
mkdir build && cd build
../configure --prefix=/opt/softwares/gdb-14.2 --with-python=/usr/bin/python3  --with-gmp=/opt/softwares/gmp-6.3.0 --with-mpfr=/opt/softwares/mpfr-4.2.1
#--enable-checking=release --enable-languages=c,c++ --disable-multilib
make  -j $(nproc)
sudo make install
sudo ln -sf /opt/softwares/gdb-14.2 /opt/gdb
```

---

## 5. install formatter

petmacs use [apheleia](https://github.com/radian-software/apheleia) to format the buffer.

### 5.1 ruff (Python)

install [ruff](https://github.com/astral-sh/ruff) in your environment.

``` bash
# for python linter and formatter
pip install ruff
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

### 5.4 cmake-format (cmake)

``` bash
pip install cmakelang
```

### 5.5 pg_format (SQL)

``` bash
sudo apt-get install pgformatter
```

### 5.6 dprint (dockerfile, toml)
``` bash
npm install -g dprint
```

### 5.7 prettier

``` bash
npm install -g prettier
```

formatter for `json`, `javascript`, `typescript`, `css`, `scss`, `html`, `graphql`, `markdown`, `yaml`

---

## 6. Screenshots

![dashboard](./data/pics/dashboard.png)

<br>

![elisp](./data/pics/elisp.png)
