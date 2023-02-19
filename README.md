The goal of this project is to allow me to learn Haskell by re-implementing
(small parts of) web browser.




This project is based on Dillo web browser (dillo.org,
https://en.wikipedia.org/wiki/Dillo), version 3.0.5.




This project is not using Haskell tool X or Haskell library Y because these
are not the things that I want to focus on right now. My main focus is on
learning the core language and its features (hopefully more and more advanced
features). Examples of X and Y are stack/cabal or a parsing library.




Known problems:
1. A memory may be leaking in FFI code. The boundary between C code and
   Haskell is slowly but constantly shifting, so I'm not paying too much
   attention to lifetime of objects allocated in FFI code.
2. Quality of SSL in this program is unknown (internally it is described as
   "ALPHA"). Don't rely on quality of SSL in this program.
3. Haskell code is slow. Processing a CSS of simple pages can take up to ~20
   seconds.
4. The Haskell code has been tested with only two real-life webpages: lwn.net
   and soylentnews.org
5. The Haskell code to a large degree resembles original C and C++ code.




Currently (January 2023) the project is hosted on GitHub:
https://github.com/acerion/hello.




Compile-time dependencies include:
libssl-dev
libpng-dev
libjpeg-dev
libfltk1.3-dev
gcc
g++
ghc version 8.8.4

Additionally for tests of Haskell code:
libghc-hunit-dev
libghc-quickcheck2-dev
libghc-quickcheck-instances-dev




Compile, install and run with these commands:
export hello_rootdir=hello_rootdir
./configure --prefix=$HOME/$hello_rootdir
make
make install prefix=$HOME/$hello_rootdir
LD_LIBRARY_PATH=$HOME/$hello_rootdir/lib ~/$hello_rootdir/bin/hello




![](code_stats.png "Recent-ish output of code_stats.py")

