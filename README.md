# hterm
Http terminal based on [butterfly](https://github.com/paradoxxxzero/butterfly) with server written in haskell.

### The stroy
My company's machine is running a very old linux distro(CentOs 4.3) which i can't setup a proper butterfly on, but i managed to compile a working GHC(from 6.8 -> 6.10 -> 6.12 -> 7.4 -> 7.8 -> 7.10, Oh My!), So why not just write a backend server with it since i'm making my way to a haskell noobie?

### Build and use
First get your GHC and cabal
```
git clone https://github.com/winterland1989/hterm.git
cd hterm
cabal sandbox init
```
Due to a glib [bug](http://stackoverflow.com/questions/6634387/c-statically-linked-shared-library), you may have to use a workaround like this:
```
$ cd /usr/lib/gcc/x86_64-linux-gnu/4.4
$ sudo cp crtbeginT.o crtbeginT.orig.o
$ sudo cp crtbeginS.o crtbeginT.o
```
And then
```
cabal build
```
You will get your executable at `dist/build/hterm/hterm`

For the sake of simple deploying, i embed all static file into the final executable(and i use static linking too), so you can just copy the `hterm` file to the machine you wanna deploy, since i shamelessly take all the client code from butterfly(main.js and main.css), i will keep the client code as latest as possible, use hterm like this:
```
$ hterm 8080 zsh
```
Now open your browser and access, you are done.
