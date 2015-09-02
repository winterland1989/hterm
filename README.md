hterm
=====

Http terminal based on [butterfly](https://github.com/paradoxxxzero/butterfly) with server written in haskell.
All credit of js and css go to [paradoxxxzero](https://github.com/paradoxxxzero/butterfly), based on that i add some fix.

Build and use
-------------

First get your GHC and cabal
```
git clone https://github.com/winterland1989/hterm.git
cd hterm
cabal sandbox init
cabal install --only-dependencies
cabal build
```
If you use static linking(add `-staic -optl-static` to cabal file), due to a glib [bug](http://stackoverflow.com/questions/6634387/c-statically-linked-shared-library), you may have to use a workaround like this:
```
$ cd /usr/lib/gcc/x86_64-linux-gnu/4.4
$ sudo cp crtbeginT.o crtbeginT.orig.o
$ sudo cp crtbeginS.o crtbeginT.o
```
You will get your executable at `dist/build/hterm/hterm`

For the sake of simple deploying, all static file are embeded into final binary, use hterm binary like this:
```
$ hterm 8080
```
Now open your browser and access, you are done.

Key binding(Linux)
------------------

+ KILL:  ctrl + d
+ Copy:  ctrl + c (:))
+ Paste: shift + insert

Gotcha
------

+ After you close your browser/tab, SIGTERM will be sent to your shell process, but there's no guarantee that the shell will be terminated, for example, when you running vim from the shell, a disconnect from hterm may result in two useless running process, so always clean up your shell before leaving.

+ if you need clean up useless shell sessions somehow, use `killall -9 $SHELL` and re-login.

+ On mac os, safari doesn't allow paste if there's no `input` on focus, and `keyCode` in safari is a mess, so copy and paste doesn't work on safari. Use chrome or firefox to use `command+c` to copy and `command+v` paste.

+ Under some linux distro(Ubuntu for example), `login` need to run as root, so run hterm like this: `sudo hterm 3000`
