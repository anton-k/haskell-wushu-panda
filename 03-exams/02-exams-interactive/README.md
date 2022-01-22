# Interactive exam engine

Let's make or exam example more user friendly and modern looking.
Instead of dumping question by question to terminal we can
make nice interactive terminal application that redraws itself. 
Enter the [brick](https://github.com/jtdaugherty/brick/).

Let's define our exam engine interface so that 
when user answers the question we clear out the screen and draw the next question.

### Brick library for TUIs

TUIs are terminal user application or interactive apps that run stright in the terminal.
Great examples of useful TUIs are `htop` utility or `vim` editor.

Being Haskell developers we are super lucky to have library [`brick`](https://github.com/jtdaugherty/brick/).
As it makes building TUIs very easy. It features concise and easy to grasp model for doing this.

In the `brick` application is sort of finite state machine. It has state and we can update state
based on incoming events. Events can be terminal driven (like user input) or external code
can send events to the brock app over channels. When app receives an event it can update the state
or decide to exit. To render the state on the screen we define drawing functions that is triggered
every time state changes. This approach gives us clear separation of concerns. 
We define drawing functions as means to view the app and we define reactions to the events that
just update the state and don't mess up with drawing utilities. 

To setup the drawing and event handling functions `brick` has the following main type:

```haskell
data App s e n =
    App { appDraw         :: s -> [Widget n]
        , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
        , appHandleEvent  :: s -> BrickEvent n e -> EventM n (Next s)
        , appStartEvent   :: s -> EventM n s
        , appAttrMap      :: s -> AttrMap
        }
```

We are interested in the functions: `appDraw`, `appHandleEvent` and `appAttrMap`.
Other functions can be set to defaul values:

```haskell
import Brick
import qualified Graphics.Vty as Vty

type ExamState = () -- we deal with that

initApp :: Exam -> App ExamState () ()
initApp exam@Exam{..} = App{..}
  where
    appDraw = undefined

    appHandleEvent = undefined

    appAttrMap = const $ attrMap Vty.defAttr [("selected", fg Vty.blue)]

    --------------------------------------------------
    -- sensible defaults 

    appStartEvent = pure
    appChooseCursor = neverShowCursor
```

The `appAttrMap` defines properties of the objects. You can think of it as what CSS does for HTML.
We define our style shortcuts in it. Here we defined that selected item should be rendered in `blue` color.

To complete the code for application we need to render exam on screen and handle the events.

## Types 

Let's define the state for our App. User is going to see the questions and for each question
use is going to choose the right answer and submit it to the app. After user goes over all questions
user is going to see nice screen with summary of scores. As we go we accumulate the total score.
We assume that user navigates over alternatives with Up/Down arrows and submits an answer with `Enter` button.

AS we pass exam we navigate over questions and inside the question we navigate over alternatives.
Also we would like to show hints to user if user does something wrong. 
Let's define that in Haskell:

```haskell
data ExamState = ExamState
  { examState'questionId :: Int           -- ^ current question
  , examState'score      :: Score         -- ^ score so far
  , examState'cursor     :: Maybe Int     -- ^ answer alternative choice
  , examState'log        :: Text          -- ^ hints and help message for the user

defaultLog :: Text
defaultLog = "Use arrows to choose answer, Enter to submit an answer"
```

The `cursor` is empty when question is presented to user for the first time. In this state
pressing `Enter`-button should fail. This will prevent users from accidental double pressing `Enter`
that can lead to unwanted submissions (if we had default answer choice set up).

The `log` field serves as message box to give help messages to the user.

## Events

So now we know our main type let's define reactions to events.
we are going to have four simple actions:

* Press up arrow to choose previous alternative

* Press down arrow to choose next alternative

* Press `Enter` to submit the answer

* Press `q` to quit early

We progress to the next question when answer is submitted. When we reach the final 
question and answer it we show the result score and after that arrows should no longer work
and `Enter` will lead to exit of the app.

Let's define that in Haskell:

```haskell
initApp :: Exam -> App ExamState () ()
initApp exam@Exam{..} = App{..}
  where
    --------------------------------------------------
    appHandleEvent st evt = case evt of
      VtyEvent (Vty.EvKey Vty.KUp [])         -> continue $ updateCursor (-1) st
      VtyEvent (Vty.EvKey Vty.KDown [])       -> continue $ updateCursor 1 st
      VtyEvent (Vty.EvKey Vty.KEnter [])      -> if isOver st then halt st else continue $ setAnswer st
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> halt st
      _                                       -> continue st

    isOver st = examState'questionId st >= totalSize
    totalSize = length exam'questions

    updateCursor = undefined
    setAnswer = undefined
```

Note that all functions are defined under `where`-clause of `initApp` and we have exam definition available to us.

The function `updateCursor` is to be defined. It will  update the current cursor value by given amount of steps.
The function `setAnswer` accumulates the total score with given reply and updates state to show the next question.
We use `brick` functions `continue` to progrress to the new state and `halt` to exit.
The function `isOver` checks weather we have reached the final state. 
Here we use underlying library `vty` to specify terminal events.

Let's define update of the cursor:

```haskell
  updateCursor n st = case lookupQuestion exam (examState'questionId st) of
    Just (Question qn) -> 
      let total = length $ desc'data qn
      in  st { examState'cursor = Just $ maybe 0 ((`mod` total) . (+ n) ) $ examState'cursor st }
    Nothing -> st

  lookupQuestion :: Int -> Maybe Question
  lookupQuestion qnId = exam'questions `atMay` qnId
```

We `atMay` from the `safe` library. Here we just increment the current value of the counter by given amount
and use `mod` to make sure that value is always within available range of answers.

Let's define the `setAnswer` function:

```haskell
  setAnswer st = case examState'cursor st of
    Just choiceId ->
      let nextQuestionId = examState'questionId st + 1
      in
        st
          { examState'questionId = nextQuestionId
          , examState'score      = examState'score st <> lookupScore (examState'questionId st) choiceId
          , examState'cursor     = Nothing
          , examState'log        = if nextQuestionId < totalSize then defaultLog else "Press Enter to exit"
          }
    Nothing -> st { examState'log = "No answer to the question. Use Up/Down arrow to choose the answer" }

  lookupScore = undefined
```

The thing that happen are very simple. We increment the `questionId` by one as we progress to the next question.
We accumulate scores with monoid instance as it was in the previous `exam-engine` implementation.
Also we set the `cursor` to `Nothing` to prevent user from clicking on `Enter` twice and submiting
wrong answers. Also we set up the hint to default message and if it's over we set it to the final hint.
In this hint user knows how to properly exit the app. 

Also we do one interesting thing cursor is set to `Nothing` and enter was pressed we remind the user that
no answer was chosen and provide info on how to make a choice.

The function `lookupScore` is used to find the right score for a given `questionId` and `choiceId`:

```haskell
  lookupScore :: Int -> Int -> Score
  lookupScore qnId choiceId = fromMaybe mempty $ do
    qn <- lookupQuestion qnId
    toAnswer qn choiceId

  toAnswer :: Question -> Int -> Maybe Score
  toAnswer (Question (Desc _ opts)) n =
    fmap desc'data $ opts `atMay` n
```

Nothing special. Standard `Map` interaction. So it's all for the events. Our app became interactional.
To be able to use it we need to be able to render the state on the screen. Let's do it.

## Drawing the state

The `brick` features nice DSL to create formatted output which is called widget. 
We have cool means to compose widgets vertically (`vBox`)
and horizontally (`hBox`). We can create widgets out of primitives like `Text` (`txt`) or `String` (`str`).
Also we can pad widgets with `padAll`, `padTop`, `padLeft` `padRight` etc.

We show two types of screens:

* Current question

* Final score if questions are done

```haskell
import Text.Wrap (wrapText)
...

  appDraw st@(ExamState qnId score mCursor log)
    | isOver st = frame $ drawResult score
    | otherwise = frame $ drawQuestion qnId mCursor
    where
      drawLog = vBox [ line, txt $ wrap log]
      frame w = [padAll 2 $
        vBox [txt exam'name, headLine, padTop (Pad 2) w, padTop (Pad 2) drawLog ]]

  wrap = wrapText defaultWrapSettings 50
  headLine = txt (T.replicate 70 "=")
```

Here we place the app into nice frame to make both screens look uniform. 
also in the frame (below the main view) we draw the hints for the user.
One unusual function is `wrapText` we use it to be sure that text spans for certain
amount of space.

Let's draw the result:

```haskell
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
...

  drawResult score = vBox
    [ txt "Your score:"
    , emptyWidget
    , padLeft (Pad 4) $ vBox $ fmap (txt . binToText) $ sortScore score
    ]

  binToText (Bin title, score) = mconcat ["  ", title, ": ", showText score]
  sortScore (Score m) = L.sortOn (negate . snd)  $ M.toList m

showText :: Show a => a -> Text
showText = T.pack . show
```

Notice the power of combinators for drawing in pair with standard functional
programming tools. In just a line of code we could render all the scores, group them
vertically and pad to the left:

```haskell
    , padLeft (Pad 4) $ vBox $ fmap (txt . binToText) $ sortScore score
```

Neat! Let's move on to draw the question view:

```haskell
  drawQuestion qnId mCursor = case lookupQuestion qnId of
    Just (Question qn) -> vBox
      [ preNum (qnId + 1) (desc'title qn)
      , line
      , padLeft (Pad 4) $ vBox $ zipWith (drawAnswer mCursor) [0..] (desc'data qn)
      ]
    Nothing            -> emptyWidget

  preNum n x = txt (mconcat [showText n, ". ", wrap x])  -- ^ writes formats like "5. message"
  line = txt (T.replicate 70 "-")

  drawAnswer = undefined
```

So we draw a question title alongside with question number. We increment it by one
to make it more human readable. Humans normally count from 1 not from 0 as we programmers usualy do.
Also we draw answers. We zip with list to provide indices for alternatives.
Let's draw the answer:

```haskell
  drawAnswer mCursor n ans
    | mCursor == Just n = withAttr "selected" $ hBox [txt "* ", w]
    | otherwise         = hBox [txt "  ", w]
    where
      w = preNum (n + 1) (desc'title ans)
```

It follows the samme pattern as question message only we also prepend `*`-sign for selected questions.
Also we use function `withAttr` to set up the styling attribute. We define those names in the 
`appAttrMap`. This atribute will render text in blue color. Whicj is nice visual hint to the user.

That's it we are done! We can test the application. 
The rest of the code almost unchanged comparing to the previous implementation. 
One tiny change is that we need to keep the output file in the spec to be able to save the file.
To complete the definition let's define the final function:

```haskell
import qualified Data.ByteString as B
import Data.Yaml.Pretty (encodePretty, defConfig)

runExam :: Spec -> IO ()
runExam (Spec name outFile exam) = do
  let app = initApp exam
      initialState = ExamState 0 mempty Nothing defaultLog
  score <- examState'score <$> defaultMain app initialState
  B.writeFile outFile $ encodePretty defConfig (ExamResult name (exam'name exam) score)
```

We run the brick application with `defaultMain` function. We pass our app definition and the initial state.
After app exits we get the final score. At the last line we save results to the file. 
That's it! The update of `Args.hs` module to save output file to spec is leaved as an easy exercise to the reader.

What's interesting is that if compare the code size for both implementations and 
cool lookng TUI is even slightly shorter (207 vs 213). What a great library `brick` is.
Adding so much value to no more cost of implementation. Of course it means learning more. But
underying concept is very easy and should be familiar. 

I hope that it will open a perspective to write new cool TUIs for you.
Terminal aplications can be fast and nice to use at the same time.

## Exercises

* Add ability to set answer by pressing numbers. `1` for first answer, `2` - for second and so on.

* Add feature to get back to the previous answer and change it. For example user can press `Backspace`
   and offer different answer to the question. Hint: watch out for score updates.

* Experiment with design and layout. Add more colors!

* Add welcome screen. As examine starts we show the greetings message to the user.
   User presses enter and begins the exam.

* Add time constraints. Welcome screen shows how many minutes user has to complete exam.
   The timer starts when user hits the enter for the first time on welcome screen.
   We should be able to show the passing time in seconds. 

   Hint: to implement this task use `BChan`'s. It's the feature to send events 
   programmatically to the brick app from outside of the app. Learn from the official
   brick guide about it.

* Implement your own TUI from scratch.  It can be anything. A random sugestions: 15 puzzle, tetris, solitaire, server config editor,
   your own text quest game.

  For inspiration I highly recommend to checkout this nice tutorial:

  - [Brick Tutorial by Samuel Tay](https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md)

  It teaches to make real-time games in terminal. Looks nice an nerdy.

