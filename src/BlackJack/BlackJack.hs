{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}  
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

--https://www.blackjack.org/blackjack-rules/

module BlackJack.BlackJack 
    ( createCardPack
    , initGame
    , playerHit
    , playerStand
    , dealerFirstStep
    , dealerNextStep
    , getPlayerCards
    , getDealerCards
    , getGameResult
    , getGameResultText
    , GameResult (..)
    , AllGameResults
    , PlayerTurn
    , GameOver 
    , DealerFirstTurn
    , DealerNextTurn
    , CardPack
    , DealerStepOutcome
    , GameState
    , GameStatus (..)
    , GameSummary (..)
    ) where

import           Protolude
import           BlackJack.Card
import           Haskus.Utils.Variant

data GameResult 
    = BlackJack
    | PlayerWon
    | DealerWon
    | Push
    | PlayerBusted
    | DealerBusted
    deriving (Show,Enum,Bounded,Typeable)

data GameStatus 
    = PlayerTurnStatus
    | DealerFirstTurnStatus   
    | DealerNextTurnStatus
    | GameOverStatus GameResult

data GameState (gameStatus::GameStatus) (playerStatus::HandStatus) (dealerStatus::HandStatus) = GameState         
    { playerHand :: PlayerHand playerStatus
    , dealerHand :: PlayerHand dealerStatus
    , pack :: CardPack
    } deriving Show

type family PlayerHandForRes (gameRes :: GameResult) :: HandStatus where
  PlayerHandForRes BlackJack = Good
  PlayerHandForRes PlayerWon = Good
  PlayerHandForRes DealerWon = Good
  PlayerHandForRes Push = Good
  PlayerHandForRes PlayerBusted = Busted
  PlayerHandForRes DealerBusted = Good
type family DealerHandForRes (gameRes :: GameResult) :: HandStatus where
  PlayerHandForRes BlackJack = OneCard
  DealerHandForRes PlayerWon = Good
  DealerHandForRes DealerWon = Good
  DealerHandForRes Push = Good
  DealerHandForRes PlayerBusted = OneCard
  DealerHandForRes DealerBusted = Busted
  
type PlayerTurn = GameState 'PlayerTurnStatus 'Good 'OneCard     
type DealerFirstTurn = GameState 'DealerFirstTurnStatus 'Good 'OneCard     
type DealerNextTurn = GameState 'DealerNextTurnStatus 'Good 'Good
type GameOver (res :: GameResult) = GameState (GameOverStatus res) (PlayerHandForRes res) (DealerHandForRes res)     

type AllGameResults = 
  '[ GameOver 'BlackJack
   , GameOver 'PlayerWon
   , GameOver 'DealerWon
   , GameOver 'Push
   , GameOver 'PlayerBusted
   , GameOver 'DealerBusted
   ]

initGame :: CardPack -> Either (GameOver BlackJack) PlayerTurn 
initGame cardPack = 
    let ((card1,card2,card3), newPack) = runState (liftM3 (,,) getCard getCard getCard) cardPack 
    in 
      if gotBlackJack card1 card2 card3 
        then Left $ initGameState card1 card2 card3 newPack 
        else Right $ initGameState card1 card2 card3 newPack

initGameState card1 card2 card3 newPack = GameState (initWithTwoCards card1 card2) (initWithOneCard card3) newPack          

playerHit :: PlayerTurn -> Either (GameOver PlayerBusted) PlayerTurn
playerHit (GameState {..}) = 
    let (card, newPack) = runState getCard pack 
    in case addCardToHand card playerHand of
        Left bustedHand -> Left $ GameState bustedHand dealerHand newPack
        Right goodHand -> Right $ GameState goodHand dealerHand newPack  

playerStand :: PlayerTurn -> DealerFirstTurn
playerStand (GameState {..}) = GameState playerHand dealerHand pack

type DealerStepOutcome =
 '[ DealerNextTurn 
  , GameOver PlayerWon
  , GameOver DealerWon
  , GameOver Push
  , GameOver DealerBusted
  ]

dealerFirstStep :: DealerFirstTurn -> V DealerStepOutcome 
dealerFirstStep  = dealerStep
  
dealerNextStep :: DealerNextTurn -> V DealerStepOutcome 
dealerNextStep  = dealerStep
  
gameState :: forall gameStatus. 
              (PlayerHand 'Good, PlayerHand 'Good, CardPack)
              -> GameState gameStatus 'Good 'Good        
gameState (ph,dh,pack) = GameState  ph dh pack

gameOver :: forall gameRes. 
              (PlayerHand 'Good, PlayerHand 'Good, CardPack)
              -> GameState (GameOverStatus gameRes) 'Good 'Good        
gameOver = gameState @(GameOverStatus gameRes)

dealerStep :: (CanAddCard dealerStatus  ~ 'True) => GameState gameStatus 'Good dealerStatus -> V DealerStepOutcome 
dealerStep (GameState {..}) =     
  let (card, newPack) = runState getCard pack
  in case addCardToHand card dealerHand of
    Left bustedHand -> toVariantAt @4 $ GameState playerHand bustedHand newPack
    Right goodHand -> 
      let 
        dealerScore = handBestScore goodHand
        hp = (playerHand, goodHand, newPack)
      in 
        if dealerScore < 17
          then toVariant $ gameState @'DealerNextTurnStatus hp-- toVariantAt @0 $ gameState hp
          else 
            let playerScore = handBestScore playerHand
            in case compare playerScore dealerScore of
              GT -> toVariant $ gameOver @'PlayerWon hp 
              LT -> toVariant $ gameOver @'DealerWon hp 
              EQ -> toVariant $ gameOver @'Push hp

-- getPlayerCards :: GameState gameStatus playerStatus dealerStatus -> [Card]
getPlayerCards = getHandCards . playerHand
getDealerCards = getHandCards . dealerHand

getGameResult :: (Typeable res) => GameOver (res :: GameResult) -> Proxy (res :: GameResult)
getGameResult _ = Proxy

getGameResultText :: (Typeable res) => GameOver (res :: GameResult) -> Text
getGameResultText = show . typeRep . getGameResult

class GameSummary xs where
  getGameSummary :: V xs  -> (HandCards,HandCards,Text)

instance GameSummary '[] where
  getGameSummary var = undefined

instance (x ~ GameOver res, Typeable res, GameSummary xs) => GameSummary  (x ': xs) where
  getGameSummary var = case popVariantHead var of 
    Right gameOver -> (getPlayerCards gameOver, getDealerCards gameOver, getGameResultText gameOver)
    Left v_xs -> getGameSummary v_xs
