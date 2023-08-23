# war_project
## purpose
The purpose of this code is to have the small "war" game programatically and see the end results
## About the War Card Game
The War card game is very simple:  
you take a deck of cards, shuffle it and divide it to two equal decks.   
Each player draws out a card from the top of the deck and place in front of them.   
If player 1 draws a 5 and player 2 draws a 7 then Player 2 wins. 
If player 1 draws a 5 and player 2 draws a 3 then player 1 wins. 
The player with the card with the larger number put their card on top of the oponent's card and adds the two cards to the buttom of their deck. 
For the purpose of the code this is called a "tug". (see "tug" function in the code). 
The order is: Joker >  Ace > King > Queen > Jake > 10 > 9 > ..... > 2

If both players draws the same card (let's say, Ace and Ace) then we have a "war" (see "war" function in the code):
Both players put down 3 cards (face down) and then we are comparing the 4th card on top. This is a "winner takes all" situation;
The highest 5th card is the winner of all the cards. If both cards are identical again then we have another war situation (which is why the war function is recursive). 
If there are no more cards for that player then the last card is the rulling one. If the last one is identical to the opnenets' last one then the openents should
put down another 4 cards and by those the winner is decided. 

## The Setup
For the purpose of setting up and seeing what affects the end results I've generated the following setup code:
1. Repeating with 1 to 2 deck of cards
2. Repeating with 0 to 3 jokers
3. The maximum number of rounds (possible tugs) is limited at different sizes, since there's a good chance of reaching an infinite game)
4. There's the concept of "imbalance" which means either a card which is being removed from the end of the deck for player 1
4a. setup 1: simple removal
4b. setup 2: The removed card are added to the 2nd player deck
5. each condition is run for 200 times.

 ## The results
To reproduce the results you can either re run the code and then choose to save the results. It could take a while (I was running the code on MacBook Pro)
