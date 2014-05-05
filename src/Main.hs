-- HackerNews command line client

import Network.Curl
import Text.HTML.TagSoup

data HackerNewsItem = HackerNewsItem { title :: String
                                     , link :: String
                                     , comments :: String
                                     , description :: String
                                     } deriving (Show, Eq)

processWebRequest :: String -> IO (Either CurlCode (String, CurlCode))
processWebRequest  url = do
  (status, page) <- curlGetString url []
  case status of
    CurlOK -> return $ Right (page, status)
    _ -> return $ Left status

hnRss = "https://news.ycombinator.com/rss"

parseWebPage :: String -> [Tag String]
parseWebPage = parseTags

printHnTags = do
  webRes <- processWebRequest hnRss
  case webRes of
    Left status -> putStrLn $ "Failed with code: " ++ show status
    Right (page, _) -> do
      let tags = parseWebPage page
      print tags

exampleRss = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
             \<rss version=\"2.0\">\
             \<channel>\
             \<title>Hacker News</title>\
             \<link>https://news.ycombinator.com/</link>\
             \<description>Links for the intellectually curious, ranked by readers.</description>\
             \<item>\
             \<title>How Steve Wozniak Wrote BASIC for the Original Apple From Scratch</title>\
             \<link>http://gizmodo.com/how-steve-wozniak-wrote-basic-for-the-original-apple-fr-1570573636/all</link>\
             \<comments>https://news.ycombinator.com/item?id=7687174</comments>\
             \<description><![CDATA[<a href=\"https://news.ycombinator.com/item?id=7687174\">Comments</a>]]></description>\
             \</item>\
             \</channel>\
             \</rss>"

getNewsItems page = map getItem $ sections (~== "<item>") $ parseWebPage page
  where
    getItem tags = HackerNewsItem { title = tagText $ tags !! 2
                                  , link = tagText $ tags !! 5
                                  , comments = tagText $ tags !! 8
                                  , description = tagText $ tags !! 11 }
    tagText (TagText s) = s

printHNLinks = do
  Right (page, _) <- processWebRequest hnRss
  let items = getNewsItems page
  mapM_ (uncurry prettyPrintHNItem) $ zip items [1..]

prettyPrintHNItem hnItem num = do
  putStrLn $ "[" ++ show num ++ "] " ++ title hnItem
  putStrLn $ "Link: " ++ link hnItem
  putStrLn $ "Comments: " ++ comments hnItem
  putStrLn ""

processCommands = do
  putStr "> "
  cmd <- getLine
  putStrLn ""
  case cmd of
    "d" -> display
    "display" -> display
    ('s':' ':num) -> showInBrowser num
    ('s':'h':'o':'w':' ':num) -> showInBrowser num
    "e" -> exit
    "exit" -> exit
    _ -> uknown cmd
  where
    display = do
      printHNLinks
      processCommands

    uknown cmd = do
      putStrLn $ "Unknown command: " ++ cmd
      processCommands

    showInBrowser num = do
      putStrLn $ "Opening item: " ++ num ++ " in the system browser"
      processCommands

    exit = putStrLn "Bye..."

main = do
  putStrLn "Welcome to HN Top"
  printHNLinks
