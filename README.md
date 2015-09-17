hterm
=====

An HTTP terminal based on [butterfly](https://github.com/paradoxxxzero/butterfly) with a server written in Haskell.
All credit for JS and CSS goes to [paradoxxxzero](https://github.com/paradoxxxzero/butterfly). I added some fixes and a useful save function.

Usage
-----

```
$ hterm 8080
```

Now open your browser, access port 8080, and you are done.

Binaries
--------

Binaries for Mac (built on Yosemite) and Linux (built on Ubuntu x86_64 14.04) are under [binary](https://github.com/winterland1989/hterm/tree/master/binary). All static files (js, css, etc.) are embeded into the binary.


Build from source
-----------------

First [get GHC and cabal](https://www.haskell.org/downloads), then

```
git clone https://github.com/winterland1989/hterm.git
cd hterm
cabal sandbox init
cabal install --only-dependencies
cabal build
```

An easier alternative might be to get [stack](https://github.com/commercialhaskell/stack#how-to-install)
and then
```
git clone https://github.com/winterland1989/hterm.git
cd hterm
stack install
```

### Static linking

By default the project is built with dynamic linking, but you can use static linking by adding `-staic -optl-static` to `-ghc-options` in the cabal file. However, due to a glib [bug](http://stackoverflow.com/questions/6634387/c-statically-linked-shared-library), you may have to use a workaround like this to get it working:

```
$ cd /usr/lib/gcc/x86_64-linux-gnu/4.4
$ sudo cp crtbeginT.o crtbeginT.orig.o
$ sudo cp crtbeginS.o crtbeginT.o
```

You will get your executable at `dist/build/hterm/hterm`.

Key bindings
------------

+ KILL:  Ctrl+D
+ Copy:  Ctrl+C (Command+C on Mac)
+ Paste: Shift+Insert (Command+V on Mac)
+ Save:  Ctrl+S (see below)

Saving `stdout` to client-side file
-----------------------------------

`hterm` uses [FileSaver.js](https://github.com/eligrey/FileSaver.js) to save `stdout` to a local file on the client side. Let's try it:

```
cat theFileYouWantToSave; sleep 10
```

Now you have 10 seconds to press `Ctrl+S` and save it to a local file. After that, your file will be appended with an extra shell prompt line.

You can always access your `stdout` with `window.stdout` in your browser's console. 

Due to a [Safari bug](https://github.com/eligrey/FileSaver.js/issues/12#issuecomment-47247096), the MIME type will always be `text/plain`, so remove the `.txt` extension if necessary.

This function is intended to save a text stream (SQL dump, source code, etc.), but you can use `base64` to encode a binary stream (I've done it a lot):

```
base64 someSmallBinaryFileYesMakeSureReallySmall; sleep 10
```

After saving it to local file, decode it with `base64` at your local shell:

```
base64 -D theFileYouJustSaved > decodedFile
```

Watch out: if the stream is too long it may crash your browser (too many nodes).

Gotchas
-------

+ After you close your browser/tab, SIGTERM will be sent to your shell process, but there's no guarantee that the shell will be terminated. For example, when running `vim` from the shell, a disconnect from `hterm` may result in two useless running processes. **Always clean up your shell before leaving.**

+ If you need to clean up useless shell sessions somehow, use `killall -9 $SHELL` and re-login.

+ On Mac OS, Safari doesn't allow paste if there's no `input` in focus (and `keyCode` in Safari is a mess) so copy and paste doesn't work in Safari. Use Chrome or Firefox to use `Command+C` to copy and `Command+V` paste.

+ Under some Linux distros (Ubuntu, for example), `login` needs to run as root, so run `hterm` like this: `sudo hterm 3000`.
