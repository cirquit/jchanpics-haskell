# jchanpics-haskell
4chan content downloader via JSON created in Haskell

#### Compiling

 `ghc JChanPics.hs -o <your program name> -O2`

 or just

 `make` in the `/` directory

#### Usage:

 * `./<your program name> -board <4chan board> -to <filepath to save content to> -range <page from in int> <page to in int>`

 * The pages are inclusive, so `-range 1 2` is going to download page 1 AND 2

 * `./<your program name> -board <4chan board> -to <filepath to save content to> -thread <threadid as int>`

#### Example:

 `./jpchan -board g -to ../lib/gcontent -range 1 2`

 `./jpchan -board g -to ../lib/gcontent -thread 39894014`


#### TODO:

 * interface to select threads / pages / boards
 * concurrency