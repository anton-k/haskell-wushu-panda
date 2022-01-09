# Password obfuscator

![Safe pic](https://github.com/anton-k/haskell-wushu-panda/blob/main/img/password-obfuscator.jpg)

Many web sites do want to know our credentials. Hopefully they use
obfuscation techniques to prevent stealing of the paswords and reusal of them
on other platforms. Hopefully they do it but unfortunately some don't do that
and even don't do that on purpose. 

As martial pandas we know lots of top secrets and we'd like to keep them safe.
So let's create a tool to obfuscate the passeords so that we can easily
generate passwords for various platforms. 

Our requirements: it should be easy to 
* generate strong passwords 
* update passwords if it's reused or expired
* we don't know the passwrds and can not look them up (auto copy paste)

## Password obfuscation theory

The main idea is to generate strong passwords out of master password and the input data.
We never show the master password and keep it secret and use only derived passwords on sites.
It's convenient to use the web site name as an input. This way we can generate unique 
and strong passwords. There are several cryptographic algorithms to use for the task:

* PBKDF1
* PBKDF2
* Argon2
* bcrypt
* scrypt
* Catena, Lyra2, Makwa, or yescrypt

We can pick up any of them. We can study the hackage to choose the algorithm we need.
Hopefully somebody already implemented that and indeed we have several options.
Let's use library [argon2](https://hackage.haskell.org/package/argon2).
Study the docs for the library.
We init the project and add `argon2` to dependencies.

```
-- file hpass.cabal
  build-depends:
      base >=4.7 && <5
    , argon2
    , bytestring
    , text-short
```

Also we add useful `text-short` and `bytestring` packages that `argon2` relies on.

## Design of the application

We will create a CLI that asks for master password and auxilliary input
to produce derived password. The password is going to by pasted to our buffer.
So that we can Copy paste it in the browser window that asks for it.

## Include github dependency to stack

With argon2 we face the problems. As we can not use it on stack directly.
Stack keeps it's own versions of some subset of the hackage. The rest of the libraries
we need to specify in the `extra-deps` field of the `stack.yaml` file.
For our task we need to add to `stack.yaml`:

```yaml
extra-deps:
   - git: https://github.com/haskell-hvr/argon2
     commit: cafbc6e5d998ba182d92879c3a6e7b3f5ca93fc1

 allow-newer: true
```

We also allow newer as there is dependency mismatch on regions of the base library in the argon2
and our project.

### Obfuscation in the REPL

Let's see what `argon2` can do for us. Let's enter `stack ghci` and try out main functions:

```haskell
> import Crypto.Argon2
> import qualified Data.Text.Short as T
>
> -- activate GHC language extension in the ghci session (
> -- it let us use string litterals ffor many similar things):
> :set -XOverloadedStrings
> T.unpack <$> hashEncoded defaultHashOptions "wushu-panda-pass" "the-top-most-secret-rev-22-01-09"
Right "$argon2i$v=19$m=4096,t=3,p=1$dGhlLXRvcC1tb3N0LXNlY3JldC1yZXYtMjItMDEtMDk$LrQPoN3+KMjj9WY9wDDyXmjhLOVRmeNRlEpM+G91z/Y"
```

So we can see the result of the obfuscation. 
Unfortunately library also prepends all parameters of the algorithm which makes it vulnerable.
It separates arguments with dollar signs `$`. Actual output that we need
is the last one. Let's split string by dollars. For that we will use the library `split`.
we need to add as dependency and restart ghci session:

```haskell
> commands as before
> Right res = T.unpack <$> hashEncoded defaultHashOptions "wushu-panda-pass" "the-top-most-secret-rev-22-01-09"
> import Data.List.Split (splitOn)
> mapM_ putStrLn $ splitOn "$" res

argon2i
v=19
m=4096,t=3,p=1
dGhlLXRvcC1tb3N0LXNlY3JldC1yZXYtMjItMDEtMDk
LrQPoN3+KMjj9WY9wDDyXmjhLOVRmeNRlEpM+G91z/Y
*Main Crypto.Hpass.App Crypto.Hpass.Args Paths_hpass Crypto.Argon2 T Data.List.Split>
```

We can see that we need to drop first 5 elements from that list.

```haskell
> import qualified Data.List as L
> putStrLn $ L.intercalate "$" $ drop 5 $ splitOn "$" res
LrQPoN3+KMjj9WY9wDDyXmjhLOVRmeNRlEpM+G91z/Y
```

Let's give that function a name:

```haskell
stripParams x = L.intercalate "$" $ drop 5 $ splitOn "$" x
```

Let's save that definition to our source code. We will need it later.

## Application

Let's build the application bit by bit. We use the same structure as in prev project.
We create two files for application and to parse the arguments:

```haskell
Crypto.Hpass.App
Crypto.Hpass.Args
```

Let's start with the code for application. First let's dfine types
and make stubs for the functions:

```haskell
module Crypto.Hpass.App(
  Hpass(..),
  runHpass,
) where

import Data.Bifunctor
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Text.Short (ShortText, toByteString, unpack)
import Crypto.Argon2 (hashEncoded, HashOptions)

data Hpass = Hpass
  { hpass'masterPassword  :: ShortText
  , hpass'secret          :: ShortText
  , hpass'options         :: HashOptions
  }
  deriving (Show)

deriveKey :: Hpass -> Either String ShortText
deriveKey Hpass{..} = bimap show id $
  hashEncoded hpass'options (toByteString hpass'masterPassword) (toByteString hpass'secret)

stripParams :: String -> String
stripParams str = L.intercalate "$" $ drop 5 $ splitOn "$" str

runHpass :: Hpass -> IO ()
runHpass cfg = undefined
```

also we have saved useful functions from trying out `argon2` in the `ghci` session.
What our `runHpass` is going to do? We take in the options, derive the key 
and save that key to clipboard to copy paste that in the browser window.

### Copy paste the result

We almost done the only missing part is saving to clipboard for that we are going
to use [Hclip](https://hackage.haskell.org/package/Hclip) library.
It is also not present on Stackage. So to use it we need to add it to the list
of extra-deps in the `stack.yaml` filer:

```yaml
extra-deps:
  - Hclip-3.0.0.4
  - git: https://github.com/haskell-hvr/argon2
    commit: cafbc6e5d998ba182d92879c3a6e7b3f5ca93fc1
```

Also we add depenedency to the `hpass.cabal` file:

```
build-depends:
    base >=4.7 && <5
  , argon2
  ... more deps ...
  , Hclip
```

Ok let's try it out in the ghci. the only function we need is `setClipboard`:

```haskell
> stack ghci
> import System.Hclip (setClipboard)
>
> setClipboard "haskell-copy"
```

try to paste it in your browser or editor.

### Complete the application

With that function all parts of the application come into place:

```haskell
runHpass :: Hpass -> IO ()
runHpass cfg =
  case deriveKey cfg of
    Right key -> setClipboard $ stripParams $ unpack key
    Left err  -> putStrLn err
```

So we have a working application. We can test it in the ghci
and move on to implementation of the CLI. 

### Parse the arguments

Let's make a stub of the function to parse arguments:

```haskell
module Crypto.Hpass.Args(
  readHpass,
) where

readHpass :: IO Hpass
readHpass = undefined
```

We querry the user for password and site name (which becomes also a secret).
This way our passwords depend on the URL of the site.
We can use it like this:

```
> hpass --password wushu-panda-pass --site reddit.com
```

We can copy paste the solution from the previous task and adapt it to our needs.
we need to read strings in both cases. We can use the function `getCmd` and
change the description and field names. Also we can borrow the parser definition 
from pomodoro example. 

Parser for arguments can look like this:

```haskell
import Options.Applicative
import Crypto.Argon2 (defaultHashOptions)
import qualified Data.Text.Short as T
import Crypto.Hpass.App (Hpass(..))

hpassArgs :: Parser Hpass
hpassArgs = (\psw secret -> Hpass (T.pack psw) (T.pack $ fromSecret secret) defaultHashOptions)
      <$> getPass
      <*> getSite
  where
    getPass = strOption
          ( long "password"
         <> short 'p'
         <> metavar "STRING"
         <> help "Master password")

    getSite = strOption
          ( long "site"
         <> short 's'
         <> metavar "STRING"
         <> help "Web site")

fromSecret :: String -> String
fromSecret site = take 100 $ cycle site
```

It's an easy exercise for the reader.
After that we have the working application. That's cool! only we have some minor issues.

### Hide the master password input

It's not a great idea to type master password as plain string.
It is visible to everybody behind our shoulder and also this approach
makes it easy to hunt for our password by reading terminal history.
Just type `history` and you are ready to grab our top secrret password.
This is bad. To fix that we are going to modify argument reader.
Instead of reading it from arguments we will prompt user for it.
We will read only site name and file that keeps the hashing algorithm options.

Let's modify the example. Instead of reading the `Hpass` config directly
we define preliminary config:

```haskell
data Args = Args
  { args'secret  :: String
  , args'options :: Maybe FilePath
  }
```

we can define a parser for arguments:

```haskell
preArgs :: Parser Args
preArgs = Args
  <$> (fromSecret <$> getSite)
  <*> getOptions
  where
    getSite = argument str
          ( metavar "SITE"
         <> help "Web site")

    getOptions = optional $ strOption
          ( long "hash-config"
         <> short 'h'
         <> metavar "STRING"
         <> help "Path to hash config file")

fromSecret :: String -> String
fromSecret site = take 100 $ cycle site
```

Notice how we use new functions:
* `argument` to pass positional arguments
* `optional` to parse `Maybe String`. Failure is packed as `Nothing`.

```haskell
fromArgs :: Args -> IO Hpass
fromArgs Args{..} = do
  hpass'options <- maybe (pure defaultHashOptions) (fmap read . readFile) args'options
  let hpass'secret = T.pack args'secret
  hpass'masterPassword <- T.pack <$> getPassword
  pure Hpass{..}
  where
    getPassword = getLine
```

This function reads the options from file if it's specified 
otherwise it uses default options. It just copies secret as is.
As the last step it queries passord. 

We solved one problem. It's no longer possible to look up master password in the 
terminal history. But password is still visible. We can make a solution with
special function that does not echoes input (solution is taken from StackOverflow):

```haskell
import System.IO
import Control.Exception

-- | Read the password without echoing the input
getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
```

Great! Now we have all pieces in place. And we are ready to use our application.
You can look at complete code in the `hpass/src/Crypto/Hpass/Args.hs`.

Also we implement the `app/Main.hs` function as in previous task:

```haskell
module Main where

import Crypto.Hpass.App
import Crypto.Hpass.Args

main :: IO ()
main = runHpass =<< readHpass
```

Let's try it out:

```
> hpass reddit.com
Password: <invisible wushu panda secret pass>
```

That's it! We did it. It took us 80 lines of code to complete the task.
Ofcourse we could do it that fast because we have standed on the shoulders of giants
who implemented for us

* argon2
* Hclip
* split
* other useful haskell libraries

We can build things much faster if we can reuse solutions.

## Tips and tricks

As we don't know the password it's easy to misspel stuff on the init. 
If the site wants to retype the password don't copy-paste it twice. Instead
it's safer to go through our routine twice and generate the password with the same inputs.

## Exercise

* Some platforms require passwords to be no more than so many symbols.
    Add option to generate passwords of certain size.

* Often it's useful to store options in human readable form. 
    Add YAML representation of the options and use this format instead of Haskell show/read.
    Read from the yaml file. Use library [yaml](https://hackage.haskell.org/package/yaml).

* Make default file for the hpass options. For now we need to supply the string to the file.
  Instead of that if file is missing try to read YAML config from default file in 
  current directory (say ".hpassrc"). If file is missing either only then use default argon2 options. 
  Use library [`directory`](https://hackage.haskell.org/package/directory) to solve that.

* Make manager of the options. For example instead of editting the `/home/user/.hpassrc` file
  we can set option with:

  ```
  hpass --set NAME --value VALUE
  ```
  
  or 

  ```
  hpass set NAME VALUE
  ```
  
  Look at [commands](https://hackage.haskell.org/package/optparse-applicative-0.16.1.0#commands)
  in the library `optparse-applicative` to implement latter case.

