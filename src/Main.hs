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

getNewsItems page = map getItem $ sections (~== "<item>") $ parseWebPage page
  where
    getItem tags = HackerNewsItem { title = tagText $ tags !! 2
                                  , link = tagText $ tags !! 5
                                  , comments = tagText $ tags !! 8
                                  , description = tagText $ tags !! 11 }
    tagText (TagText s) = s

getLatestNewsItems = do
  Right (page, _) <- processWebRequest hnRss
  return $ getNewsItems page

prettyPrintHNItem hnItem num = do
  putStrLn $ "[" ++ show num ++ "] " ++ title hnItem
  putStrLn $ "Link: " ++ link hnItem
  putStrLn $ "Comments: " ++ comments hnItem
  putStrLn ""

printHNLinks items = mapM_ (uncurry prettyPrintHNItem) $ zip items [1..]

processCommands news = do
  putStr "> "
  cmd <- getLine
  putStrLn ""
  case cmd of
    "d" -> display news
    "display" -> display news

    "r" -> refresh
    "refresh" -> refresh

    ('s':' ':num) -> showInBrowser news num
    ('s':'h':'o':'w':' ':num) -> showInBrowser news num

    "e" -> exit
    "exit" -> exit

    _ -> uknown news cmd

  where
    display news = 
      case news of
        Nothing -> do
          putStrLn "Loading Hacker News..."
          items <- getLatestNewsItems
          printHNLinks items
          processCommands (Just items)
        Just items -> do 
          putStrLn "Displaying Hacker News..."
          printHNLinks items
          processCommands news

    refresh = do
      putStrLn "Refreshing Hacker News..."
      items <- getLatestNewsItems
      processCommands (Just items) 

    uknown news cmd = do
      putStrLn $ "Unknown command: " ++ cmd
      processCommands news

    showInBrowser news num = do
      case news of
        Nothing -> putStrLn $ "Please refresh news by pressing 'display' or 'refresh' first"
        Just items -> do
          let itemNum = read num :: Int
          if itemNum <= 0 || itemNum > (length items)
            then do
              putStrLn $ "Please enter a positive number less than or equal to " ++ num
              undefined
            else do
              putStrLn $ "Opening item number " ++ num ++ " in system browser"
              undefined
      processCommands news

    exit = putStrLn "Bye..."

main = do
  putStrLn "Welcome to HN Top"
  processCommands Nothing
