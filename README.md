hterm
=====

Http terminal based on [butterfly](https://github.com/paradoxxxzero/butterfly) with server written in haskell.
All credit of js and css go to [paradoxxxzero](https://github.com/paradoxxxzero/butterfly), based on that i add some fix and a useful save function.

### Usage

```
$ hterm 8080
```
Now open your browser and access port 8080, you are done.

### Build from source

First get your GHC and cabal, then

```
git clone https://github.com/winterland1989/hterm.git
cd hterm
cabal sandbox init
cabal install --only-dependencies
cabal build
```

If you use static linking(not used by default, add `-staic -optl-static` to cabal file), due to a glib [bug](http://stackoverflow.com/questions/6634387/c-statically-linked-shared-library), you may have to use a workaround like this:
```

$ cd /usr/lib/gcc/x86_64-linux-gnu/4.4
$ sudo cp crtbeginT.o crtbeginT.orig.o
$ sudo cp crtbeginS.o crtbeginT.o
```

You will get your executable at `dist/build/hterm/hterm`

### Key binding

+ KILL:  ctrl + d
+ Copy:  ctrl + c (command + c on Mac)
+ Paste: shift + insert (command + v on Mac)
+ Save: ctrl + s (see below)

### Save stdout to local

Hterm use [FileSaver.js](https://github.com/eligrey/FileSaver.js) to save last `stdout` to local file, let's try it:

```
cat theFileYouWantToSave; sleep 10
```

Now you have 10 seconds to press `ctrl + s` and save it to local. (after that, your file will be appended with a extra shell propmt line).

You can always access your stdout with `window.stdout` in your browser's console. 

Due to a [safari bug](https://github.com/eligrey/FileSaver.js/issues/12#issuecomment-47247096), mime will always be `text/plain`, so remove the `.txt` extension if necessary.

This function is intended to save text stream(sql dump, source code...), but you can use `base64` to encode binary stream(i do it a lot):

```
base64 someSmallBinaryFileYesMakeSureReallySmall; sleep 10
```

After saving it to local, decode it with `base64`(at your local shell):

```
base64 -D theFileYouJustSaved > decodedFile
```

Watch out if the stream is too long, it may crash your browser(too many nodes...).

Gotcha
------

+ After you close your browser/tab, SIGTERM will be sent to your shell process, but there's no guarantee that the shell will be terminated, for example, when you running vim from the shell, a disconnect from hterm may result in two useless running process, so always clean up your shell before leaving.

+ if you need clean up useless shell sessions somehow, use `killall -9 $SHELL` and re-login.

+ On mac os, safari doesn't allow paste if there's no `input` on focus, and `keyCode` in safari is a mess, so copy and paste doesn't work on safari. Use chrome or firefox to use `command+c` to copy and `command+v` paste.

+ Under some linux distro(Ubuntu for example), `login` need to run as root, so run hterm like this: `sudo hterm 3000`
