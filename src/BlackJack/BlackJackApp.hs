{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}  
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

--https://www.blackjack.org/blackjack-rules/

module BlackJack.BlackJackApp where

import           Protolude
import           BlackJack.BlackJack
import           Haskus.Utils.Variant
import           Control.Monad.Loops (untilJust)
import           Data.Function (const, id)
import           Control.Monad.Except
import           Data.Either.Combinators (mapLeft)

promptUser :: MonadIO m => Text -> (Text -> Maybe a) -> m a
promptUser promptTxt f =
  liftIO $ untilJust $ fmap f (putStrLn promptTxt >> getLine)  

pressEnter :: MonadIO m => Text -> m ()  
pressEnter txt = promptUser txt (const $ Just ())  

type GameM = ExceptT (V AllGameResults) IO 

startGame :: GameM PlayerTurn
startGame = do
  liftIO $ pressEnter "Press enter to start the shuffle"
  cardPack <- createCardPack
  liftEither $ mapLeft toVariant $ initGame cardPack 

playerGame :: PlayerTurn -> GameM DealerFirstTurn
playerGame playerTurn = do 
  shouldHit <- promptUser ("Your cards are:" <> show (getPlayerCards playerTurn) <>"; Enter h to hit or s to stand:")  (\case
                      "h" -> Just True 
                      "s" -> Just False 
                      _ -> Nothing)
  if (shouldHit) 
    then liftEither (mapLeft toVariant (playerHit playerTurn)) >>= playerGame
    else return $ playerStand playerTurn                  

dealerFirstGame :: DealerFirstTurn -> GameM DealerNextTurn 
dealerFirstGame dealerFirstTurn = do
  pressEnter ("Dealer card is:" <> show (getDealerCards dealerFirstTurn) <> ";Press enter to get dealer started")
  liftEither $ mapLeft liftVariant $ popVariantHead (dealerFirstStep  dealerFirstTurn) 

dealerNextGame :: DealerNextTurn -> GameM Void
dealerNextGame dealerNextTurn = do
  pressEnter ("Dealer cards so far :" <> show (getDealerCards dealerNextTurn) <> ";Press enter for next dealer step")
  liftEither (mapLeft liftVariant $ popVariantHead (dealerNextStep  dealerNextTurn)) >>= dealerNextGame  

game :: GameM Void  
game = 
  startGame 
  >>= playerGame
  >>= dealerFirstGame
  >>= dealerNextGame  

putStrLnT :: Text -> IO ()
putStrLnT = putStrLn

runGame :: IO () 
runGame = do 
  var <-  (either id absurd) <$> runExceptT game
  let (playerCards, dealerCards, res) = getGameSummary var
  putStrLnT ("Game result:" <> res)
  putStrLnT ("Player cards:" <> show playerCards)
  putStrLnT ("Dealer cards:" <> show dealerCards)

