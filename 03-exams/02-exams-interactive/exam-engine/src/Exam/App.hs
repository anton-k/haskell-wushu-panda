module Exam.App(
  runExam
) where

import Brick
import Exam.Types

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Safe
import qualified Graphics.Vty as Vty
import Text.Wrap
import Data.Yaml.Pretty (encodePretty, defConfig)

-- | Which question is active
data ExamState = ExamState
  { examState'questionId :: Int           -- ^ current question
  , examState'score      :: Score         -- ^ score so far
  , examState'cursor     :: Maybe Int     -- ^ answer alternative choice
  , examState'log        :: Text          -- ^ hints and help message for the user
  }

defaultLog :: Text
defaultLog = "Use arrows to choose answer, Enter to submit an answer"

runExam :: Spec -> IO ()
runExam (Spec name outFile exam) = do
  let app = initApp exam
      initialState = ExamState 0 mempty Nothing defaultLog
  score <- examState'score <$> defaultMain app initialState
  B.writeFile outFile $ encodePretty defConfig (ExamResult name (exam'name exam) score)

initApp :: Exam -> App ExamState () ()
initApp exam@Exam{..} = App{..}
  where
    --------------------------------------------------
    -- draw
    appDraw st@(ExamState qnId score mCursor log)
      | isOver st = frame $ drawResult score
      | otherwise = frame $ drawQuestion qnId mCursor
      where
        drawLog = vBox [ line, txt $ wrap log]
        frame w = [padAll 2 $
          vBox [txt exam'name, headLine, padTop (Pad 2) w, padTop (Pad 2) drawLog ]]

    wrap = wrapText defaultWrapSettings 50

    drawResult score = vBox
      [ txt "Your score:"
      , emptyWidget
      , padLeft (Pad 4) $ vBox $ fmap (txt . binToText) $ sortScore score
      ]

    binToText (Bin title, score) = mconcat ["  ", title, ": ", showText score]
    sortScore (Score m) = L.sortOn (negate . snd)  $ M.toList m

    drawQuestion qnId mCursor = case lookupQuestion qnId of
      Just (Question qn) -> vBox
        [ preNum (qnId + 1) (desc'title qn)
        , line
        , padLeft (Pad 4) $ vBox $ zipWith (drawAnswer mCursor) [0..] (desc'data qn)
        ]
      Nothing            -> emptyWidget


    line = txt (T.replicate 70 "-")
    headLine = txt (T.replicate 70 "=")

    drawAnswer mCursor n ans
      | mCursor == Just n = withAttr "selected" $ hBox [txt "* ", w]
      | otherwise         = hBox [txt "  ", w]
      where
        w = preNum (n + 1) (desc'title ans)

    preNum n x = txt (mconcat [showText n, ". ", wrap x])

    isOver st = examState'questionId st >= totalSize
    totalSize = length exam'questions

    --------------------------------------------------
    -- handle events

    appHandleEvent st evt = case evt of
      VtyEvent (Vty.EvKey Vty.KUp [])         -> continue $ updateCursor (-1) st
      VtyEvent (Vty.EvKey Vty.KDown [])       -> continue $ updateCursor 1 st
      VtyEvent (Vty.EvKey Vty.KEnter [])      -> if isOver st then halt st else continue $ setAnswer st
      VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> halt st
      _                                       -> continue st

    updateCursor n st = case lookupQuestion (examState'questionId st) of
      Just (Question qn) ->
        let total = length $ desc'data qn
        in  st { examState'cursor = Just $ maybe 0 ((`mod` total) . (+ n) ) $ examState'cursor st }
      Nothing -> st

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

    lookupScore :: Int -> Int -> Score
    lookupScore qnId choiceId = fromMaybe mempty $ do
      qn <- lookupQuestion qnId
      toAnswer qn choiceId

    lookupQuestion :: Int -> Maybe Question
    lookupQuestion qnId = exam'questions `atMay` qnId

    toAnswer :: Question -> Int -> Maybe Score
    toAnswer (Question (Desc _ opts)) n =
      fmap desc'data $ opts `atMay` n

    --------------------------------------------------
    -- start event

    appStartEvent = pure
    appChooseCursor = neverShowCursor
    appAttrMap = const $ attrMap Vty.defAttr [("selected", fg Vty.blue)]

showText :: Show a => a -> Text
showText = T.pack . show

