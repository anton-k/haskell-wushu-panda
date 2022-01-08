# pomodoro timer

In this chapter we will build a pomodoro timer and learn handful of tricks.
We are going to learn how to:

* init project with stack and common lines to work with the haskell project like a pro
* create command line utilities
* use libraries and how to show nice progress bars

The pomodoro technique is a famous productivity trick. 
It forces you to work in small portions of time. Say for only for 20-40 minutes in one go.
The work time is sliced on several pomodoro-runs. 
Between them we can rest and step away from the working place to exercise
or have time to think different or just to have good rest for 5-10 minutes.
This avoids fatigue and keeps us focused on single task when we work.

that will make you a better haskeller. 
We are going to create command line utility that works like this:

```
> pomodoro --minutes 20 --cmd "echo \"done\""
```

On the run it will show the progress bar as time passes as percentage 
and when we rich 100 % it will run the command. 
In the example we can print out the **done** message
also I'd like to play some song with it to signal that we are done. 
For example on linux with pulse audio we can run:

```
paplay bingo.wav
```

This way we get notified that our work/rest period is over.

## Init the project

For this series we are going to use **stack** tool to build and run haskell applications. 
Let's create an empty project:

```
> stack new pomodoro
```

I also like to work with cabal file and don't like hpack by default so after
that I usually remove pack.yaml. 

to check that everything works we can use:

```
> stack build --file-watch
```

It will watch for file updates. Also we can use

```
> stack install
```

To install the executable and

```
> stack ghci 
```

to try out the functions in ghci.

## Design of the app

We need two parts: 
- function to run a timer
- function to read command line arguments

Let's create a simple directory structure that reflects our design:

```
src
  +-System
    +-Pomodoro
      +-App.hs
      +-Args.hs
```

Also we are going to specify that in the cabal file by replacing Lib
with

```
library
  exposed-modules:
      System.Pomodoro.App
      System.Pomodoro.Args
```

In the `app/Main.hs` we will use 

```haskell
main = print "Hello pomodoro"
```

as placeholder for the app for now.

## Run a timer

Let's start with our pomodoro application. What is it good for?
It can wait for some amount of time (usually in minutes) and after
time passes it will run given command.

It would be nice to be able to show the porgress so that we know how much time we have.
Let's start with the type:

### Start with types and template

We define the main type for our app and the function to run it

```haskell
module System.Pomodoro.App
    ( Pomodoro(..)
    , runPomodoro
    ) where

import Data.Time

-- | Pomodoro timer
data Pomodoro = Pomodoro
  { pomodoro'totalTime  :: NominalDiffTime   -- ^ how many seconds to work until the end
  , pomodoro'onExit     :: String            -- ^ command to run on exit
  , pomodoro'greet      :: Maybe String      -- ^ greeting line
  }

runPomodoro :: Pomodoro -> IO ()
runPomodoro = undefined
```

We can specify for how many seconds we want to wait also we specify command to run
on exit and the greeting message for the user. Also we need to add the time library to build-depends
of the pomodoro.cabal file to be able to work with time and seconds:

```
  build-depends:
        base >=4.7 && <5
      , process
      , terminal-progress-bar
      , time
```

Also we add another useful libraries:

- `terminal-progress-bar` - to show the status in console
- `process` - to run commands in console

### A progress bar

We can read the docs for the `terminal-progress-bar` library. Here we can
see that there are just two functions to use:

```haskell
-- create porgress bar with 20 slots to 100%
pb <- newProgressBar defStyle 10 (Progress 0 20 ())

-- increment the progress bar by 1
incProgress pb 1 
```

We can adapt this example to our needs. The main idea is that we will split
the time by so many seconds and on each second we will wait for one second
and increment the progress bar. We initialise the progress bar with total number of seconds.

For that we have simple helper function:

```haskell
import Control.Concurrent (threadDelay)
import System.ProgressBar

wait :: IO ()
wait = threadDelay 1000000 -- 1 second

step :: ProgressBar () -> IO ()
step bar = do
  wait
  incProgress bar 1
```

So now we can wait let's define the `runPomodoro` function:

```haskell
import Control.Monad (void, replicateM_)
import System.Process (system)

runPomodoro :: Pomodoro -> IO ()
runPomodoro Pomodoro{..} = do
  mapM_ putStrLn pomodoro'greet
  let seconds = floor pomodoro'totalTime
  pb <- newProgressBar defStyle 10 (Progress 0 seconds ())
  replicateM_ seconds (step pb)
  void $ system pomodoro'onExit
```

We use haskell extension `RecordWildCards`. We need to add it to cabal file. In the library
section after `default-language` define a new field:

```
  default-language: Haskell2010
  default-extensions:
    RecordWildCards
```

It just brings named fields of the data type into the scope on the syntax `Type{..}`.

So in that function we optionally print the greeting:

```
  mapM_ putStrLn pomodoro'greet
```

After that we count how many seconds we have and define the progress bar:

```
  let seconds = floor pomodoro'totalTime
  pb <- newProgressBar defStyle 10 (Progress 0 seconds ())
```

we wait for so many seconds. The function `replicateM_` executes the procedure
so many times. In our case each time wwe wait for a second and increment the timer.

```
  replicateM_ seconds (step pb)
```

As the last step we just run the final command:

```
  void $ system pomodoro'onExit
```

We ignore the result with function `void`.
That's it we are done with pomodoro app. 

### Testing in the repl

We can try out our app in the repl:

```
stack ghci
```

And we can build pomodoro and run it:

```haskell
> spec = Pomodoro 30 "echo \"done\"" (Just "Hi Pomodoro")
> runPomodoro spec
Hi Pomodoro
[===========================================================================] 100%
done
```

Also we can stop it with `Ctrl+C`.

Nice! For now we can track our time in ghci ad we can schedule work and rest periods.
One cool hint in linux there are dozens of command line tools to play a wav/mp3 files.
We can schedule a bingo song to celebrate our end of work moment.
I like to use `paplay` or `cplay`. Let's take a short well-deserved brake.


## Parse command line arguments

For now we have function that runs a pomidoro timer.
We would like to use it as CLI. For that we need to know how to parse the arguments.
Let's learn about that. We can use a cool library [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative).
It has cool documentation. So visit the link and study the beginning of it. 
It's great exercise in itself. 

We need to use the basic features. We will parse minutes as `Int` and command
line to run on exit as `String`. Greetings will be hardcoded and based on minutes.

The library `optparse-applicative` makes it easy build the CLIs once you understand
the basic principle behind it. It relies on the notion of `Applicative` functor.
The main idea of applicative functor is to apply `map` or `fmap` with the function
of several arguments. So if we have a Maybe transformation:

```haskell
> fmap (* 2) (Just 10)
Just 20
```

With applicative instance we can use the same concept for functions with
several arguments:

```haskell
> (+) <$> (Just 1) <*> (Just 2)
Just 3
```
The fancy combo of `<$>` and arbitrary number (match to the number of arguments in the function)
of `<*>`'s. For example we can sum up four maybes:

```haskell
> (\a b c d -> a + b + c + d) <$> (Just 1) <*> (Just 2) <*> (Just 100) <*> Nothing
Nothing
```

The optparse does the same trick with command line arguments. 
Every `Just` becomes single option parser and we can aggregate them with applicative methods.
Also we can specify the type to parse and help documentation for arguments.
I keep forgetting it all the time, so just use hackage docs to do it again.

Back to our pomodoro task. We need two fields to read: 

minutes as integers:

```haskell
import Options.Applicative
import System.Pomodoro.App (Pomodoro(..))


readPomodoro :: IO Pomodoro
readPomodoro = undefined
  where
    getMinutes =
      option auto
          ( long "minutes"
         <> short 'm'
         <> help "How many minutes to wait"
         <> showDefault
         <> value 25
         <> metavar "INT")
```

It expects integer argument for a long flag `--minutes` or short alternative `-m`.
The default value is 25 minutes. The `help` and `metavar` provide documentation messages.

Command lines as strings 
(all those functions are inside the `where`-clause of the function `readPomodoro`):

```haskell
    getCmd = strOption
          ( long "cmd"
         <> short 'c'
         <> metavar "SHELL-COMMAND"
         <> help "Command to execute on exit"
         <> showDefault
         <> value "echo \"done\"")
```
I think it's self explanatory. We defined nice defaults and help docs messages. 
Only we need to remember that `option auto` produces `Int`
in this context and strOption produces `String`.

We can combine those functions with applicative to get the spec:

```haskell
    pomodoroArgs = (\mins cmd -> Pomodoro (fromMinutes mins) cmd (Just $ toGreeting mins))
      <$> getMinutes
      <*> getCmd

    -- converts to seconds
    fromMinutes n = fromInteger $ 60 * n

    -- makes nice greeting message
    toGreeting n = unwords ["Pomodoro timer:", show n, (if n == 1 then id else ( <> "s")) "minute"]
```

Last thing to complete the parser:

```haskell
readPomodoro = execParser opts
  where
    opts = info (pomodoroArgs <**> helper)
      ( fullDesc
     <> progDesc "Pomodoro timer"
     <> header "pomodoro timer executes command after so many minutes" )
```

Notice how we use our main function `pomodoroArgs`. The rest is a common boilerplate
that is easy to look up in the hackage docs. I don't know still what raccoon operator
`<**>` means. But surprisingly we are done with the code.

The last thing is to define the `app/Main.hs`:

```haskell
module Main where

import System.Pomodoro.App
import System.Pomodoro.Args

main :: IO ()
main = runPomodoro =<< readPomodoro
```

as simple as that our app is ready to use. Let's install it to global space of the PC:

```
stack install
pomodoro --help
```

Let's try it out:

```
pomodoro -m 2
```


## Conclusion

We have built a flexible and robust pomodoro timer. Enjoy it!
I personally like to use it with an alias (put in `~/.bashrc`)

```
alias pom='pomodoro -c "paplay ~/music/pomodoro-song.wav" -m '
```

And after that we can invoke it with shortcut:

```
> pom 40
```

## Exercises: 

* make time parse not only minutes but human readable times (hours/minutes/seconds). 
   See the library `time`. It has useful functions to parse time.

* make our progress bar to show the remaining time in nice human readable format.
  Have a look at the parameter of the `ProgressBar`. It's responsible for custom displays.

* make timer show the local time of expected end time in local time. We can show it like this:

    ```
    Pomodoro timer : work time
      Started      : time
      Finish       : time
    [---progress-bar-------------------] 1 % | time remains
    ```

* add an option for showing the task name in the greeting so that 
    we know which pomodoro we are eating right now. 
    Write it so that module `App.hs` stays unchanged.

