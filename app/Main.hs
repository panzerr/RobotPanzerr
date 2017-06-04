{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Text
import Pipes
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Network.Discord
import System.IO
import System.Random
import TextCommands
import qualified TokenSecret as TOK -- only contains the secret token of the bot

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

update :: Text ->  TMVar StdGen -> IO(String)
update txt sta = do
  rep <- atomically $ do
    val <- takeTMVar sta
    let (response,nextGen) = control val txt
    putTMVar sta nextGen
    return response
  return rep


nub (a,_) = a

main :: IO ()
main = do
  randGen <- getStdGen
  state <- newTMVarIO randGen --creates a random generator and pushes it in state
  runBot (Bot TOK.tokenSecret) $ do
    with ReadyEvent $ \(Init v u _ _ _) ->
      liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

    with MessageCreateEvent $ \msg@Message{..} -> do
      when ("#" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
        resp <- liftIO $ do
          aux <- update  messageContent state
          return aux
        liftIO . hFlush $ stdout
        reply msg (pack resp)
