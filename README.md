# hterm
Http terminal based on [butterfly](https://github.com/paradoxxxzero/butterfly) with server written in haskell.
All credit of js and css go to [paradoxxxzero](https://github.com/paradoxxxzero/butterfly)

### The stroy
My company's machine is running a very old linux distro(CentOs 4.3) which i can't setup a proper butterfly on, but i managed to compile a working GHC(from 6.8 -> 6.10 -> 6.12 -> 7.4 -> 7.8 -> 7.10, Oh My!), So why not just write a backend server with it since i'm making my way to a haskell noobie?

### Build and use
First get your GHC and cabal
```
git clone https://github.com/winterland1989/hterm.git
cd hterm
cabal sandbox init
cabal install
```
If you use static linking(-staic -optl-static), due to a glib [bug](http://stackoverflow.com/questions/6634387/c-statically-linked-shared-library), you may have to use a workaround like this:
```
$ cd /usr/lib/gcc/x86_64-linux-gnu/4.4
$ sudo cp crtbeginT.o crtbeginT.orig.o
$ sudo cp crtbeginS.o crtbeginT.o
```
You will get your executable at `dist/build/hterm/hterm`

For the sake of simple deploying, i embed all static file into final binary, i will keep the client code as latest as possible, use hterm like this:
```
$ hterm 8080 zsh
```
Now open your browser and access, you are done.

### Key binding
+ KILL:  ctrl + d
+ Copy:  ctrl + c (:))
+ Paste: shift + insert

### Gotcha
After you close your browser/tab, SIGTERM will be sent to your shell process, but there's no guarantee that the shell will be terminated, for example, when you running vim from the shell, a disconnect from hterm may result in two useless running process, so always clean up your shell before leaving, it's also recommended to use ```exit``` to leave your session.
if you need clean up useless shell sessions somehow, find shell processes which ppid equals to hterm's pid and ```kill -9```.
