{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module BlackJack.Card 
  ( HandStatus (..)
  , PlayerHand
  , CardPack
  , CanAddCard
  , HandCards
  , createCardPack
  , handBestScore
  , initWithOneCard
  , initWithTwoCards
  , getCard
  , addCardToHand
  , gotBlackJack
  , getHandCards
  ) where

import           System.Random.Shuffle
import           GHC.TypeLits
import           Protolude hiding (TypeError)
import           Prelude (Show (..))

-- ** Card data type and utilties functions

-- !Card data type 
data Card = Card !Int deriving Eq

-- |Card pack 
type CardPack = [Card] --52

-- |Create randomly shuffled card pack 
-- notice using MonadIO (instead of IO to save liftIO)
createCardPack :: (MonadIO m) => m CardPack
createCardPack = do
  cardNumList <- liftIO $ shuffleM [0..51]
  return (Card <$> cardNumList) 

-- |Show instance for Card type show both value and suit of the card  
instance Show Card where
  show (Card iVal) = cardVal (mod iVal 13) <> " " <>  suit (div iVal 13)   
      where
      suit 0 = "Spades"
      suit 1 = "Stars"
      suit 2 = "Diamonds"
      suit 3 = "Hearts"
      suit _ = "Unk"
      cardVal 0 = "Ace"
      cardVal 1 = "King"
      cardVal 11 = "Queen"
      cardVal 12 = "Jack"
      cardVal i = Prelude.show (i::Int)

cardMinMaxValue :: Card -> (Int, Int)         
cardMinMaxValue (Card iVal) =
    case  mod iVal 13 of 
        0 -> (1,11)
        1 -> (10,10)
        11 -> (10,10) 
        12 -> (10,10) 
        i -> (i,i)

newtype CardVal = CardVal {unCardVal :: (Int,Int)}

instance Semigroup CardVal where 
    (<>) (CardVal (i1,i2)) (CardVal (i3,i4)) = CardVal (i1+i3, i2+i4)
instance Monoid CardVal where
    mempty = CardVal (0,0)

cardsValMinMax :: [Card] -> (Int,Int)
cardsValMinMax cards = unCardVal $ fold ((CardVal . cardMinMaxValue) <$> cards)

isBusted :: [Card] -> Bool
isBusted cards = fst (cardsValMinMax cards) >21

---------------------------
-- ** Hand (cards of player or dealer) data and functions

-- |Hand state (Player or Dealer)
data HandStatus
  -- |Dealer initial Hand when only one card is exposed 
  = OneCard 
  -- |Two or more cards (not busted) 
  | Good 
  -- |Busted (sum of points exceeded 21)
  | Busted 

-- |Player hand parameterized witn promoted hand status as phantom type parameter 
newtype PlayerHand (status :: HandStatus) = PlayerHand 
  { handCards :: [Card] 
  } deriving Show

-- >>> :kind PlayerHand
-- PlayerHand :: HandStatus -> *


-- |smart constructors (so hand cannot be constructed using just PlayHand data constructor)
-- |with single card 
initWithOneCard :: Card -> PlayerHand 'OneCard
initWithOneCard card = PlayerHand [card]

-- |smart constructors with two cards
initWithTwoCards :: Card -> Card -> PlayerHand 'Good
initWithTwoCards card1 card2 = PlayerHand [card1, card2]

-- |type family (type level function) to prohibit adding card to busted hand  
type family CanAddCard (handStatus :: HandStatus) :: Bool where
  CanAddCard 'OneCard = 'True 
  CanAddCard 'Good =    'True 
  CanAddCard 'Busted =  TypeError ('Text "Can not add card to busted hand") 
 
-- |disallow adding card to Busted Hand
-- No need to check if Hand is busted - compile time guarantee!  
addCardToHand :: (CanAddCard handStatus ~ 'True) => Card -> PlayerHand handStatus -> 
    Either (PlayerHand Busted) (PlayerHand Good) 
addCardToHand card (PlayerHand cards) = 
  let newCards = card : cards
  in 
    if isBusted newCards 
      then Left $ PlayerHand newCards
      else Right $ PlayerHand newCards

-- |getting hand score only applies to 'Good' hand
handBestScore ::  PlayerHand 'Good -> Int
handBestScore (PlayerHand cards) = 
  let (minScore, maxScore) = cardsValMinMax cards
  in 
    if maxScore > 21 
      then minScore
      else maxScore

-- ***more utilties funtiions

-- |get next card from deck
-- using state monad
getCard :: State CardPack Card
getCard  = do
  pack <- get 
  case pack of
    [] -> return $ Card 51 -- will never happen
    firstCard : cards -> put cards >> return firstCard

-- |Check if BlackJack occured     
-- card1 and card2 are Player cards, card3 is dealer card. If player got 21 
-- and Dealer has neither ace nor 10 then Player got BlackJack
gotBlackJack :: Card -> Card -> Card -> Bool 
gotBlackJack card1 card2 card3 = 
  let (_, val1) = cardMinMaxValue card1
      (_, val2) = cardMinMaxValue card2
      (_, val3) = cardMinMaxValue card3
  in case (val1+val2,val3) of
    (21,11) -> False    
    (21,10) -> False    
    (21,_)  -> True    
    _ -> False


-- |HandCards (of player or dealer)     
newtype HandCards = HandCards [Card] deriving Show

-- |return HandCards (polymorhic by hand status)
getHandCards :: PlayerHand handStatus -> HandCards
getHandCards  = HandCards . handCards   
