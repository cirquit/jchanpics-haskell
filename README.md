# jchanpics-haskell
4chan content downloader via JSON created in Haskell

#### Compiling

 `ghc JChanPics.hs -o <your program name> -O2`

 or just

 `make` in the `/` directory

#### Usage:

 `./<your program name> -board <4chan board> -to <filepath to save content to>`

#### Example:

 `./jpchan -board g -to ../lib/gcontent`

#### TODO:

 * interface to select threads / pages / boards
 * concurrency