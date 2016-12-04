import Reflex.Dom
import UI
import JobQueueUI

main :: IO ()
main = mainWidgetWithHead UI.head (el "div" $ do
                                      getUserList
                                      blank
                                  )
