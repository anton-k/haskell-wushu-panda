# Exam engine

For this project we will write generic tool to pass an examine
or take a psychological test. The user answers series of single-choice questions
at the end user sees the scores or final result. It can be the final mark
or sort of category that user belongs to. 

So let's spawn a new stack project and dive into the task.

```
stack new exam-engine
```

It's going to be the CLI application again. User will go over a list of
single choice questions and get results at the end. We create the same structure:

```
src/Exam/
  App.hs
  Args.hs
  Types.hs
```

Only for this project we have more complicated domain and it leads to more
complicated types to describe it. For that we need one more file called `Types.hs`.
So let's dive into domain.

##  Exam spec

Examine is a series of questions. Each question has several answers.
For the answer we get certain score. As we go over the questions we accumulate
final score and at the end of the exam we show it on the screen or save results to the file.
Also we want to show nice greeting at the beginning to inform about examine topic
and maybe show some instructions.

Let's define the types for domain. 
Examine has name, greeting message and the list of questions:

```haskell
import Data.Text (Text)

data Exam = Exam
  { exam'name      :: Text
  , exam'questions :: [Question]
  , exam'greeting  :: Text
  }
  deriving (Show)
```

The question has title and list of answers. Each answer has dedicated score:

```haskell
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Desc a = Desc
  { desc'title :: Text
  , desc'data  :: a
  }
  deriving (Show, Functor)

newtype Question = Question (Desc [Desc Score])
  deriving (Show)

newtype Score = Score (Map Bin Int)
  deriving (Show)

score :: Bin -> Int -> Score
score b n = Score $ M.singleton b n

newtype Bin = Bin { bin'name :: Text }
  deriving (Show, Eq, Ord)
```

We have defined a type `Desc` for named values. 
In the question definition outermost `Desc` holds question message as title
and in the alternatives we have named scores. Each title in the list has alternative
in the title and score as a data. User receives the score for answering choosing 
the given alternative. Score is a `Map` from `Bin`s to integers. The `Bin` is a category
of answers. For example for a typical examine we can have the categories: `Right` and `Wrong`.

As we complete the test we can look at the results:

```haskell
data ExamResult = ExamResult
  { examResult'name    :: Text
  , examResult'exam    :: Text
  , examResult'scores  :: Score
  }
```

Also when we have exam we should now who is passing the exam, we need same the user name.
For that we have the type called `Spec`:

```haskell
data Spec = Spec
  { spec'name   :: Text   -- ^ user name
  , spec'exam   :: Exam   -- ^ examine
  }
```

This CLI is a bit different from previous applications. 
For pomodoro timer we just start the timer and it runs on itself.
But for this app we need to interact with it. We need to type answers and 
receive results. All those actions are going to have side effects.

We can make generic interface for those interACTions.
We need to do only 3 things in our app: 
* ask answer for the question
* show exam greeting
* show results

```haskell
data Act = Act
  { act'ask  :: Question -> IO Int    -- ^ ask a question
  , act'init :: Text -> IO ()         -- ^ show greeting
  , act'exit :: ExamResult -> IO ()   -- ^ show results
  }
```

## Running the exam

Let's define `runExam` function:

```haskell
-- src/Exam/App.hs

runExam :: Act -> Spec -> IO ()
runExam Act{..} (Spec person Exam{..}) = undefined
 act'init $ exam'greeting
 res <- getExamineResult exam'questions
 act'exit (ExamResult person exam'name res)
 where
    getExamineResult = undefined
```

We need to first show the greeting, then start to query for the answers
and after that calculate result and show it to user. 

As we go over the questions we accumulate the Score's for individual questions
and then pack them to `ExamResult`. This looks as `Writer` pattern (monad transformer). 

If you are not familiar with monad transformers and the package `mtl` check out
those articles:

* [Writer monad](http://learnyouahaskell.com/for-a-few-monads-more#writer)
* [fpcomplete tutorial](https://www.fpcomplete.com/haskell/tutorial/monad-transformers/)
* [Real world Haskell tutorial](http://book.realworldhaskell.org/read/monad-transformers.html)

We are going to use `WriterT` wwith `IO` monad as argument because as we go over
questions we need to interact with user (query for answers):

```haskell
import Control.Monad.Writer.Strict

type Run a = WriterT Score IO a
```

As user answers to the question we need to pick up the right alternative
by integer identifier and update the score:

```haskell
import Safe (atMay)

-- | React on question
step :: Question -> Int -> Run ()
step qn ans = mapM_ tell $ toAnswer qn ans

-- | Calculate user score for question answer
toAnswer :: Question -> Int -> Maybe Score
toAnswer (Question (Desc _ opts)) n =
  fmap desc'data $ opts `atMay` n
```

With the power of `Writer`-monad reaction to question becomes a single-liner.
We just calculate the score and save it to the `Writer`.

Notice the usage `atMay` from the `safe` library. It's safe variant of list function `!!`.
The library `safe` is very useful and I highly recommend to study it and use it in your projects.

To be able to use Writer we also need to define `Monoid` instance for `Score`:

```haskell
instance Semigroup Score where
  (<>) (Score a) (Score b) = Score $ M.unionWith (+) a b

instance Monoid Score where
  mempty = Score mempty
```

With those functions in place we can complete the function `runExam`:

```haskell
runExam :: Act -> Spec -> IO ()
runExam Act{..} (Spec person Exam{..}) = do
 act'init $ exam'greeting
 res <- execWriterT $ mapM_ query exam'questions
 act'exit (ExamResult person exam'name res)
 where
  query qn = do
    n <- liftIO $ act'ask qn
    step qn n
```

## Interaction with the user

To interact with user we need to define the functions for `Act` type:

```haskell
data Act = Act
  { act'ask  :: Question -> IO Int    -- ^ ask a question
  , act'init :: Text -> IO ()         -- ^ show greeting
  , act'exit :: ExamResult -> IO ()   -- ^ show results
  }
```

Let's define it in the module `Exam.Args`:

```haskell
defaultAct :: Act
defaultAct = Act{..}
  where
    act'init = undefined
    act'ask  = undefined
    act'exit = undefined
```

Let's start with the simple functions for init:

```haskell
import qualified Data.Text as T

defaultAct :: Act
defaultAct = Act{..}
  where
    act'init str = do
      line
      next
      T.putStrLn str
      next

next :: IO ()
next = putStrLn ""

line :: IO ()
line = putStrLn $ replicate 30 '-'
```

The exit is also easy to implement:

```haskell
import qualified Data.List as L
import qualified Data.Map.Strict as M

...
    act'exit (ExamResult _ _ (Score m)) = do
      line
      next
      T.putStrLn "Results:"
      next
      mapM_ (\(Bin title, score) -> T.putStrLn $ mconcat ["  ", title, ": ", text score]) $ 
        L.sortOn (negate . snd)  $ M.toList m
      next

text :: Show a => a -> Text
text = T.pack . show
```

We sort bins by score from highest to lowest.
The function to query results is a bit more complicated. We are going to 
check that the answer is integer and lies within the range of alternatives for the given question:

```haskell
    act'ask (Question (Desc title qs)) = do
      line
      next
      T.putStrLn title
      next
      mapM_ T.putStrLn $ zipWith showQuestion [1..] qs
      next
      res <- getAnswer (length qs)
      next
      pure res

    getAnswer maxSize = do
      prompt maxSize
      mn <- readMaybe @Int <$> getLine
      case mn of
        Just n | n > 0 && n <= maxSize -> pure $ pred n
        _                              -> getAnswer maxSize

    showQuestion n (Desc title _) = mconcat ["  ", text n, ". ", title]

    prompt maxSize = do
      T.putStr $ mconcat ["Type answer from 1 to ", text maxSize, ": "]
      hFlush stdout
```

We have almost all functions in place. Let's check how our functions work in the ghci.
For that we create some sort of math arithmetics examine:

```haskell
exam1 :: Exam
exam1 =
  Exam
    { exam'name = "Math exam (first grade)"
    , exam'questions =
      [ qn 2 2
      , qn 3 (-5)
      , qn 100 12
      ]
    , exam'greeting = "Welcome to math exam:"
    }
  where
    qn a b =
      Question
        (Desc (mconcat [text a, " + ", text b, " equals:"])
          [ Desc (text a) (score (Bin "Wrong") 1)
          , Desc (text b) (score (Bin "Wrong") 1)
          , Desc (text (a + b)) (score (Bin "Right") 1)
          ]
        )
```

The right answer is always 3. We could add the permutation of answers to make it real.
But for testing purposes it's good enough.

```
> stack ghci
> runExam defaultAct (Spec "Foo" exam1)
------------------------------

Welcome to math exam:

------------------------------

2 + 2 equals:

  1. 2
  2. 2
  3. 4

Type answer from 1 to 3: 1

------------------------------

3 + -5 equals:

  1. 3
  2. -5
  3. -2

Type answer from 1 to 3: 2

------------------------------

100 + 12 equals:

  1. 100
  2. 12
  3. 112

Type answer from 1 to 3: 3

------------------------------

Results:

  Wrong: 2
  Right: 1

```

So it works as expected. Now we need to define how to read exam spec from CLI app.

## Reading Exam spec

At previous projects we used command line flags and arguments to pass parameters
to the application. But for this task it is difficult to type the whole `Exam` specification
as command line argument. We are going to save `Exam` in YAML format and then read it from file.
In command line arguments we point to the file that contains exam spec also we specify the name 
of the user and the file to write the result:

```haskell
import Options.Applicative

data Args = Args
  { args'input  :: FilePath
  , args'output :: FilePath
  , args'name   :: String
  }

readArgs :: IO Args
readArgs = execParser opts
  where
    opts = info (examArgs <**> helper)
      ( fullDesc
     <> progDesc "Exam engine"
     <> header "Exam engine executes examine and saves results" )

    examArgs = Args <$> getInput <*> getOutput <*> getName

    getInput = strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "Input for exam config")

    getOutput = strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "Save results of the exam")

    getName = strOption
          ( long "name"
         <> short 'n'
         <> metavar "NAME"
         <> help "Person name")
```

This is all standard stuff implemented just like we did it in the previous projects.
After that we need to create `Spec` and `Act` to be able to run exam:

```haskell
import Data.Yaml (decodeFileEither)
import Data.Yaml.Pretty (encodePretty, defConfig)
import qualified Data.ByteString as B

readExam :: IO (Act, Spec)
readExam = fromArgs =<< readArgs

fromArgs :: Args -> IO (Act, Spec)
fromArgs Args{..} = do
  eExam <- decodeFileEither args'input
  case eExam of
    Right exam -> do
      let act = saveTo args'output defaultAct
      pure (act, Spec (T.pack args'name) exam)
    Left err -> error $ show err

saveTo :: FilePath -> Act -> Act
saveTo file x = x
  { act'exit = \res -> act'exit x res >> B.writeFile file (encodePretty defConfig res)
  }
```

We use functions `decodeFileEither` and `encodePretty` from the library `yaml` 
to read and save YAML-files.
The function `saveTo` looks interesting. It augments the `exit` method with ability
to save exam results to file. In this way we reuse the `defaultAct` and add new feature to it.

For it to work we need to make instances of ToJSON/FromJSON for our `Exam` and `ExamResult` types.
There are some nuances to it that are good to explain.

## Deriving instances for JSON types

To use `yaml` library we need to define `FromJSON`/`ToJSON` instances for our types.
They come from `aeson` library. The Haskell library to work with `JSON`.

To derive them we can use TemplateHaskell function `deriveJSON`:

```haskell
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Desc a = Desc
  { desc'title :: Text
  , desc'data  :: a
  }
  deriving (Show)
$(deriveJSON defaultOptions ''Desc)
```

Another example:

```haskell
data ExamResult = ExamResult
  { examResult'name    :: Text
  , examResult'exam    :: Text
  , examResult'scores  :: Score
  }
$(deriveJSON defaultOptions ''ExamResult)
```

There are some caveats. When we use template haskell (TH) it splits the file 
into parts by occurence of the TH-expressions. And at the execution of TH-expression
we have access only to definitions that are defined in the previous parts. 
It means that order of type definitions matter. In ordinary Haskell file we can 
place type definitions in any order as long as they are consistent. 
But for TH-derivings we need to be sure that for a given type all subparts
which it contains are already defined in previous sections.

Also we are going to use [`DerivingStrategies`](https://typeclasses.com/ghc/deriving-strategies) extension.
It allows us to specify in which way we need to derive the type class instance.

`stock` is for standard classes. `anyclass` for extension `DeriveAnyClass`
when type has `Generic` instance (from `GHC.Generics`). `newtype` is for 
extension [`GeneralizedNewtypeDeriving`](https://typeclasses.com/ghc/generalized-newtype-deriving) newtypes
when we inherit it from the underying newtype datatype. 

So in the code it looks like this:

```haskell
newtype Score = Score (Map Bin Int)
  deriving newtype (Show, ToJSON, FromJSON)

data Exam = Exam
  { exam'name      :: Text
  , exam'questions :: [Question]
  , exam'greeting  :: Text
  }
  deriving stock (Show)
$(deriveJSON dropPrefixOptions ''Exam)
```

Note that when type is used as key in the `Map` we also need to derive instance for
classes `FromJSONKey`/`ToJSONKey`:

```haskell
newtype Bin = Bin
  { bin'name :: Text
  }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
```

After that our application should work!

### Making JSON instances look nicer

If you run the example you can notice that all fields have prefixes just like in the 
Haskell definitions:

```yaml
examResult'name: "Mary"
examResult'exam: "Math exam"
```

It can look much nicer without the prefixes. We would like to use it like this:

```yaml
name: "Mary"
exam: "Math exam"
```

Luckily we can adjust this with `deriveJSON` options. We used `defaultOptions`.
But it can be adapted to our needs with ease.
Let's define module `Exam.Utils.Aeson`:

```haskell
module Exam.Utils.Aeson(
  dropPrefixOptions
) where

import Data.Aeson.TH

dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropTick }
  where
    dropTick name =
      case dropWhile (/= '\'') name of
        []     -> name
        _:rest -> rest
```

It drops in field names everything before the tick character.
We use `defaultOptions` and redefine the function `fieldLabelModifier`.

And it works:

```haskell
exam: Math exam (1 grade)
name: john
scores:
  Right: 1
  Wrong: 2
```

We did to use YAML and all those tricks are also useful to work with JSON.
It's common approach to define config files in human-friendly YAML format.
It can be server or game config. For this example we used it to create examine specs.
This example was slightly more involved then previous ones. Stil it's just about 200 lines of code
to implement it. In the directory `examples` you can find some example tests to try out the application.

## Exercises

* Add logging of the answers to the `ExamResult`. So that we can know all the answers to questions (integer ids of the choices).

* Show the numbers for the questions before the title of each question.

* Extend system to handle multiple choices

* Can we limit our exam by time? Often we have only limited amount of time to pass the exam.
    Add timer and time stamps of when exam has started and when it was finished in the `ExamResult`.
    And force exit on end of time.    

* Can we devise some method of summarizing the results?
    Like some bins are antogonist of each other and we can estimate 
      a single result based on proportion of them. 
      Ad also we can pick up one of the bins if it's more prominent then the other.
      Implement result estimation system and make it flexible and configurable.
      
* Creative task. We can go one step further and turn our exam engine
  to the text game engine. Only in the text game we traverse not the list of questions
  but the graph of states. The gave is described as graph and on each move 
  our choice can lead to the different state. Also it will be interesting to add
  health parameters to the player and transition will depend not only on the 
  choice vut also on the current health-parameter and it's value.

