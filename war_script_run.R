war <- function(player1, player2, i) #default being 1, so default is 5
{
	vec1.len <- if (i+4 < length(player1)) i+4 else length (player1)
	vec2.len <- if (i+4 < length(player2)) i+4 else length (player2)

	vec1 <- head (player1, vec1.len)		
	vec2 <- head (player2, vec2.len)

	if (vec1 [vec1.len] == vec2 [vec2.len])
		return (war (player1, player2, i+4))
	else
	{
		player1 <- tail (player1, length(player1)-vec1.len)
		player2 <- tail (player2, length(player2)-vec2.len)

		if (vec1 [vec1.len] > vec2 [vec2.len])
		{ #could be a problem if player 1 or player 2 is an empty string
			player1 <- c (player1, vec1, vec2)
		}
		# player2 wins 
		else 
		{
			player2 <- c (player2, vec2, vec1)
		}
		ret <- c(player1, player2, length(player1), length(player2))
		return (replace(ret, which(is.na(ret)),0))
	}
}

tug <- function (player1, player2) 
#any round of two palyers
{	#index i default to 1
	
	a<-player1[1]
	b<-player2[1]

	if (a != b) 
	{
	  
		if (length(player1)==1)
		  player1 <- vector(mode="numeric", length=0)  
		else
	    player1<-player1[2:length(player1)] 
		
		if (length(player2)==1)
      player2 <- vector(mode="numeric", length=0)  
		else 
		  player2<-player2[2:length(player2)] 
		
		if (a>b) 
			player1<-c(player1, a, b)
		else 
			player2<-c(player2, b, a)
	}
	else #war situation
	{
		res <- war (player1, player2, 1)
		a<-res[length(res)-1]
		b<-res[length(res)]
		player1<-head(res,a)
		player2<-tail(head(res, length(res)-2),b)

		quantile(player1)
		quantile(player2)
		

		#insert save data
	}
	ret <- c(player1, player2, length(player1), length(player2))
	return (replace(ret, which(is.na(ret)),0))
	
}

# code in use:
play <- function (player1, player2, max_number_of_rounds = 80000)
{
	a <-length(player1)
	b <-length(player2)
	cx <-0
	
	
	res <- c(player1, player2, a, b)

	# the variable max_number_of_rounds is there to avoid an infinite loop
	while (a > 0 && b > 0 && cx < max_number_of_rounds)
	{
	  #player1, player2, re defined afther the tug
	  player1<-head(res,a)
	  player2<-tail(head(res, length(res)-2),b)
	  
	  res<-tug(player1, player2)
	  
	  #a, b, re defined after the run
	  a<-res[length(res)-1]
	  b<-res[length(res)]
	  cx <-  cx+1
	}

	# returning a data frame
	# giving clear labels to the dataframe
	ret	<- data.frame ( "Player1 end position" = a,
				"Player2 end position" = b,
				"Length of game" = cx, 
				"Infinite Game" =  if (cx < max_number_of_rounds) {"No"} else {"Yes"}
			)
	return (ret)
	
}

# define decks
setup <- function (joker_count = 1L, 
					deck_count = 1L, 
					remove_balance = 0L,
					max_number_of_rounds = 10000L) {
	
	deck_stats <- NULL
	game_results <- NULL

	v <- rep (1L:13L, times = 4 * deck_count)
	z <- rep (14L, each = joker_count)

	for (seed_id in 1:200){

		set.seed (seed_id)
		x <- sample (c(v,z))

		#deal
		# remove balance: remove a random card at the end of the deck so we'll have a completely unbalanced two decks
		player1 <- head (x,(length(x)/2) - remove_balance)
		player2 <- tail (x,(length(x)/2))

		
		deck_stats_player_1 <- data.frame ("Min Player 1" = min(player1),
									"Max Player 1" = max(player1) ,
									"Mean Player 1" = mean (player1), 
									"Median Player 1" = median (player1), 
									"Size Player 1" = length (player1), 
									"Seed" = seed_id
								)

		deck_stats_player_2 <- data.frame ("Min Player 2" = min(player2),
										"Max Player 2" = max(player1) ,
										"Mean Player 2" = mean (player2), 
										"Median Player 2"= median (player2), 
										"Size  Player 2" = length (player2)
									)

		# deck_stats <- rbind (deck_stats, deck_stats_player_1)
		# deck_stats <- rbind (deck_stats, deck_stats_player_2)
		deck_stats <- rbind (deck_stats, cbind (deck_stats_player_1, deck_stats_player_2))

		#combining the round results with the previous rounds and the seed_id
		game_results <- rbind(game_results, cbind (play (player1, player2, max_number_of_rounds), seed_id))
	}

	joined_stats <- merge (game_results, deck_stats, by.x = "seed_id", by.y = "Seed")
	return (joined_stats)
}


setup2 <- function (joker_count = 1L, 
					deck_count = 1L, 
					remove_balance = 0L,
					max_number_of_rounds = 10000L) {
	
	deck_stats <- NULL
	game_results <- NULL

	v <- rep (1L:13L, times = 4 * deck_count)
	z <- rep (14L, each = joker_count)

	for (seed_id in 1:200){

		set.seed (seed_id)
		x <- sample (c(v,z))

		#deal
		# remove balance: remove a random card at the end of the deck so we'll have a completely unbalanced two decks
		player1 <- head (x,(length(x)/2) - remove_balance)
		player2 <- tail (x,(length(x)/2) + remove_balance)

		
		deck_stats_player_1 <- data.frame ("Min Player 1" = min(player1),
									"Max Player 1" = max(player1) ,
									"Mean Player 1" = mean (player1), 
									"Median Player 1" = median (player1), 
									"Size Player 1" = length (player1), 
									"Seed" = seed_id
								)

		deck_stats_player_2 <- data.frame ("Min Player 2" = min(player2),
										"Max Player 2" = max(player1) ,
										"Mean Player 2" = mean (player2), 
										"Median Player 2"= median (player2), 
										"Size  Player 2" = length (player2)
									)

		# deck_stats <- rbind (deck_stats, deck_stats_player_1)
		# deck_stats <- rbind (deck_stats, deck_stats_player_2)
		deck_stats <- rbind (deck_stats, cbind (deck_stats_player_1, deck_stats_player_2))

		#combining the round results with the previous rounds and the seed_id
		game_results <- rbind(game_results, cbind (play (player1, player2, max_number_of_rounds), seed_id))
	}

	joined_stats <- merge (game_results, deck_stats, by.x = "seed_id", by.y = "Seed")
	return (joined_stats)
}


# game_results

#	The  result of the loop is two loops:
# deck_stats  # beginning conditions of the deck each player has

df_game <- NULL
# df_game <- setup (1, 1, 10000) # 1 deck of cards with 1 joke

for (joker_count in 0L:3L) {
	for (deck_count in 1L:2L) {
		for (imbalance in (c(0, 1, 4, 5))) { #imblance is starting with uneven number of cards on both sides
												#0, 1, 3, 5 didn't produce reasonable results
			df_game <- rbind (df_game, cbind (setup (joker_count, deck_count, imbalance ,30000L), joker_count, deck_count, imbalance)) 
		}
	}
}


for (joker_count in 0L:3L) {
		for (imbalance in (c(5, 10))) { #imblance is starting with uneven number of cards on both sides
												#0, 1, 3, 5 didn't produce reasonable results
			deck_count <- 1
			df_game <- rbind (df_game, cbind (setup (joker_count, deck_count, imbalance ,30000L), joker_count, deck_count, imbalance, total_cards = deck_count)) 
		}
}




aggregate (.~Infinite.Game, df_game, FUN = max)

summary (df_game$Length.of.game)


library(dplyr)

infinite_game <- factor (c("Yes", "No"))

# Number of games that finished with an infinite loop
df_game %>%
	group_by (Is_odd = (Size.Player.1 + Size..Player.2)%%2, imbalance,Infinite.Game) %>%
	tally()

df_game %>%
	group_by (total_cards = Size.Player.1 + Size..Player.2, Is_odd = (Size.Player.1 + Size..Player.2)%%2, Infinite.Game) %>%
	summarize(max_length_of_game = max (Length.of.game)) %>% 
	print (n = 40)


df_game_csv %>%
	filter (Infinite.Game == "No") %>%
	group_by (Size.Player.1, Is_odd_deck = (Size.Player.1 + Size..Player.2)%%2) %>%
	summarize (avg_length_of_game = mean (Length.of.game)
				, max_length_of_game = max (Length.of.game)) %>%
	print (n = 50)

df_game_csv %>%
	group_by (total_cards = Size.Player.1 + Size..Player.2, Infinite.Game) %>%
	summary(Length.of.game) 



df_game_csv %>%
		filter (Infinite.Game == "No") %>%
		group_by (Size.Player.1, Max.Player.1 ,winner = factor(ifelse (Player1.end.position > 0,  {"Player1"} ,{"Player2"})) %>%
		count()
		)

count (df_game_csv, Infinite.Game)


## GRAPH THIS!
df_summary = count (df_game_csv, is_odd = (Size.Player.1 + Size..Player.2)%%2,  
					total_cards = Size.Player.1 + Size..Player.2,
					size_player_1 = Size.Player.1,
					max_player_1 = df_game_csv$Max.Player.1,
					imbalance,
					winner = factor(ifelse (Infinite.Game ==  "Yes", "Infinite", ifelse (Player1.end.position > 0,  {"Player1"} ,{"Player2"}))))

dim (df_summary)
names (df_summary)
names (df_game_csv)

df_summary

p <- ggplot (df_summary, aes (x = as.factor(is_odd), y = n, fill = winner )) + geom_bar(stat = "identity", width = .5, position = "dodge" ) + facet_wrap(~imbalance   , nrow =  2 ) + labs (x = "is odd", title = "Games by Imbalance" )


	
ggplot (df_summary, aes (x = as.factor(is_odd), y = n, fill = winner )) + geom_bar(stat = "identity", width = .5, position = "dodge" ) + facet_wrap(~max_player_1, nrow =  2 ) + labs (x = "0 - Even deck, 1 - odd deck", y = "number of gaems", title = "winner by Max Card (player 1)")


library (reshape2)
dfp1 <- melt (df_summary)
dfp1
ggplot (dfp1, aes (x = winner, y = value, fill = variable), xlab = "Winner")+ geom_bar (stat = "identity", width = .5 ,position = "dodge")






dim (df_game)
names (df_game)


#START PLOT



plot (
	table (unlist (df_game[, 4]))
	, type = "p"	, col = "red"
	, xlab = "length of game ", ylab = "number of games"
	, main = "Num of Cards per lenth of game  (Blue = even, Red = Odd)"
)
points (
	table (unlist (df_game[, 4]))
	, type = "p" , col = "blue"	
)


# END PLOT (WITH TWO CORR POINTS)



# by 
plot (
	table (unlist (df_game[, 4] ))
	, type = "p"	, col = "red"
	, xlab = "length of game ", ylab = "number of games"
	, main = "Num of Cards per lenth of game  (Blue = even, Red = Odd)"
)
points (
	table (unlist (df_game[, 4]))
	, type = "p" , col = "blue"	
)


#puting it all together in one graph
#this really does look good!

(p <- ggplot (data = df_game) 
	+ geom_count (mapping = aes (x = Length.of.game, 
					y = Size.Player.1+ Size..Player.2
					, color = factor ((Size.Player.1+ Size..Player.2) %%2L)))
	+ facet_wrap (~ Size.Player.1, nrow = 2)
	+ labs (x = "Number of Rounds to complete game", y = "Number of cards in deck", color = "Is Even Deck", caption = "Number of games") 
)

ggsave ("GameLenByPlayer1.png", plot = last_plot())


(q <- ggplot (data = df_game) 
	+ geom_count (mapping = aes (x = Length.of.game, 
					y = imbalance
					, color = factor ((Size.Player.1+ Size..Player.2) %%2L)))
	+ facet_wrap (~ joker_count, nrow = 2)
	+ labs (x = "Number of Rounds to complete game", y = "Imbalance", color = "0 - even deck, 1 = odd deck", caption = "Number of rounds by joker count") 
)

ggsave ("GameLenByJokerCount.png", plot = last_plot())


(t <- ggplot ( data = df_game %>%
		filter (Infinite.Game == "No") %>%
		group_by (Size.Player.1, joker_count, Is_odd_deck = (Size.Player.1 + Size..Player.2)%%2) %>%
		summarize (avg_length_of_game = mean (Length.of.game))
	)
		+ geom_point (mapping = aes (x = avg_length_of_game, y = Size.Player.1, color = factor (Is_odd_deck)))
		+ facet_wrap (~joker_count, nrow = 2)
		+ labs (x = "Average Game Length (finite games only)", y = "Size Player 1", color = "Is Total Deck Even", caption = "Avg Length of Finite Games by Joker count")
)

ggsave ("AvgGameLength.png", plot = last_plot())

(o <- ggplot (data = df_game) 
	+ geom_count (mapping = aes (x = Length.of.game, 
					y = Size.Player.1+ Size..Player.2
					, color = factor (joker_count)))
	+ facet_wrap (~ Size.Player.1, nrow = 2)
	+ labs (x = "Number of Rounds to complete game", y = "Number of cards in deck", color = "Joker Count", caption = "Number of games by number of Player 1 cards") 
)

ggsave ("LenGame by JokerCount and Player 1.png", plot = last_plot())


(o1 <- ggplot (data = df_game) 
	+ geom_count (mapping = aes (x = Length.of.game, 
					y = Size.Player.1+ Size..Player.2
					, color = factor (joker_count)))
	+ facet_wrap (~ imbalance, nrow = 2)
	+ labs (x = "Number of Rounds to complete game", y = "Number of cards in deck", color = "Joker Count", caption = "Number of games by level of imbalance (random cards removed from Player 1)") 
)

ggsave ("imbalance effects.png", plot = last_plot())






(t <- ggplot ( data = df_game %>%
		filter (Infinite.Game == "No") %>%
		group_by (Size.Player.1, Is_odd_deck = (Size.Player.1 + Size..Player.2)%%2) %>%
		summarize (avg_length_of_game = mean (Length.of.game))
	)
		+ geom_point (mapping = aes (x = avg_length_of_game, y = Size.Player.1, color = factor (Is_odd_deck)))
		+ labs (x = "Average Game Length", y = "Size Player 1", color = "0- Even Deck, 1- Odd Deck", caption = "Length of Game (finite games only)")

)

ggsave ("Avg LenGame Player 1(finite).png", plot = last_plot())


(t3 <- ggplot ( data = df_game %>%
		filter (Infinite.Game == "No") %>%
		group_by (Size.Player.1, 
					Is_odd_deck = (Size.Player.1 + Size..Player.2)%%2, 
					winner = Player1.end.position)
	)
		+ geom_count (mapping = aes (x = Size.Player.1, y = winner, color = factor (Is_odd_deck)))
)


(t3 <- ggplot ( data = df_game %>%
		filter (Infinite.Game == "No") %>%
		group_by (Size.Player.1, 
					Is_odd_deck = (Size.Player.1 + Size..Player.2)%%2, 
					winner = factor(ifelse (Player1.end.position > 0,  {"Player1"} ,{"Player2"})))
	)
		+ geom_count (mapping = aes (x = Size.Player.1, y = winner, color = factor (Is_odd_deck)))
		+ facet_wrap (~ imbalance, nrow = 2)
)

library (dplyr)
(t5 <- ggplot ( data = df_game_csv %>%
		filter (Infinite.Game == "No") %>%
		group_by (Size.Player.1, Size..Player.2, 
					Is_odd_deck = factor ((Size.Player.1 + Size..Player.2)%%2), 
					winner = factor(ifelse (Player1.end.position > 0,  {"Player1"} ,{"Player2"}))
	)
		+ geom_count (mapping = aes (x = Size.Player.1, color = winner))
		+ facet_wrap (~ imbalance, nrow = 2)
		+ labs (y = "Count of games", caption = "Game Count by Imbalance")
)
)

# library (hrbrthemes)
# hrbrthemes::import_roboto_condensed() 

new_df <- df_game %>% count( total_cards =  Size.Player.1 + Size..Player.2, 
						Is_odd =  total_cards %% 2 ,
						Is_infinite = Infinite.Game, joker_count)

new_df

library(ggplot2)
library (scales)
#gives a plot
(q <- ggplot (df_game_csv)
	+ geom_bar (mapping = aes (x = factor ((Size.Player.1 + Size..Player.2) %% 2) , 
		 y = Size.Player.1 ,  
		 group = factor (Infinite.Game), 
		 
		fill = factor (ifelse (Infinite.Game == "Yes", "Infinite Game", ifelse(Player1.end.position == 0, "Player2", "Player1" )))))
	+ facet_wrap (~ Max.Player.1, nrow = 1)
#	+ scale_fill_hue (c= 80, l=45)
	+ scale_fill_brewer (palette = "Spectral")
)


(q <- ggplot (df_game_csv)
	+ geom_count (mapping = aes (x = Size.Player.1 , y = Player1.end.position ,    
		color = factor (ifelse (Infinite.Game == "Yes", "Infinite Game", ifelse(Player1.end.position == 0, "Player2", "Player1" )))))
	+ facet_wrap ( ~ factor (Max.Player.1) , nrow = 2)
#	+ scale_fill_hue (c= 80, l=45)
	+ scale_fill_brewer (palette = "Spectral")
)



library(dplyr)
library(ggplot2)

df_for_plot <- df_game_csv %>% 
		group_by(winner = factor (ifelse (Infinite.Game == "Yes", "Infinite Game", ifelse(Player1.end.position == 0, "Player2", "Player1" )))) %>%
    	mutate(proportions = scales::percent(count/sum(count), 0.01))

df_for_plot %>% ggplot(aes(x = withCallingHandlers(), y = Count, fill = Flag)) +
    geom_col(position = 'dodge') 
	+
    geom_text(aes(label=paste0(Count, '(', proportions, ')'))




(q <- ggplot (df_game_csv)
	+ geom_count (mapping = aes (x = Size.Player.1 , y = Player1.end.position ,   group = factor (Max.Player.1), 
		color = factor (ifelse (df_game_csv$Infinite.Game == "Yes", "Infinite Game", ifelse(df_game_csv$Size.Player.1 == 0, "Player2", "Player1" )))))
#	+ scale_fill_hue (c= 80, l=45)
	+ scale_fill_brewer (palette = "Spectral")
)



library (corrplot)
(	t <- {
		corrplot (cor(df_game[, c(2:4, 6:16,18)]), 
		method = "number", #disply the results in numbers, not in images
		cl.pos = "n",
		type = "upper"		#only half the matrix (no repetitions!
		)
		recordPlot()
	})




ggsave ("Corrplot all games.png", plot = replayPlot (t))


library (corrplot)
(	tf <- {corrplot (cor(df_game.finite[, c(2:4, 6:16, 18)]), 
		order = "AOE",
		method = "number", #disply the results in numbers, not in images
		title = "Finite Game Correlations",
		cl.pos = "r",
		cl.length = c(21),
		mar = c(0,0,1,0),
		col = COL2('PRGn'), addCoef.col = 'grey',
		type = "upper"		#only half the matrix (no repetitions!)
	);
	recordPlot()}
)
class (tf)

ggsave ("Corrplot finite games.png", plot = replayPlot (tf))


file_path = "Corrleation Matrix Finite Games.png"
png = (height = 1800, width = 1800, file = filepath , type = "cairo")
corrplot (cor(df_game.finite[, c(2:4, 6:16, 18)]), 
		order = "AOE",
		method = "number", #disply the results in numbers, not in images
		title = "Finite Game Correlations",
		cl.pos = "r",
		cl.length = c(11),
		mar = c(0,0,1,0),
		col = COL2('PRGn'), #addCoef.col = 'grey',
		type = "upper"		#only half the matrix (no repetitions!)
	)
dev.off()




pdf  =  (file = "Corrleation Matrix Finite Games.pdf")

corrplot (cor(df_game.finite[, c(2:4, 6:16, 18)]), 
		order = "AOE",
		method = "number", #disply the results in numbers, not in images
		title = "Finite Game Correlations",
		cl.pos = "n",
		cl.length = c(21),
		mar = c(0,0,1,0),
		col = COL2('PRGn'), addCoef.col = 'grey',
		type = "upper"		#only half the matrix (no repetitions!)
	)
dev.off()






names (df_game)
#display the plot with the facotr
#to disply two plots next to each other

library(ggpubr)
ggarrange (p, q)

ggarrange (p, q, 
	ncol = 2, nrow = 1 )







# plot. Simple plot
plot (0, 0, xlim = df_game [5], ylim = c(df_game[2]), type = "n")
points (df_game[5] ~ infinite_game  )

plot.ts (df_game [1:5])





summary (df_game)

unique (df_game [18])

df_game.finite <- filter (df_game, Infinite.Game == "No")
dim (df_game.finite)

cor (df_game $ Size.Player.1, df_game $ Player1.end.position)

cor (df_game[1:100, 7] , df_game[1:100, 3] )
cor (df_game[101:200, 7] , df_game[101:200, 3] )

df_game [1,]
df_game [,1]


cor (df_game.finite $ Max.Player.1, df_game.finite $ Player1.end.position)
cor (df_game.finite $ Size.Player.1, df_game.finite $ Player1.end.position)

df_game
str (df_game)


# names (df_game)
#  [1] "seed_id"              "Player1.end.position" "Player2.end.position"
#  [4] "Length.of.game"       "Infinite.Game"        "Min.Player.1"        
#  [7] "Max.Player.1"         "Mean.Player.1"        "Median.Player.1"     
# [10] "Size.Player.1"        "Min.Player.2"         "Max.Player.2"        
# [13] "Mean.Player.2"        "Median.Player.2"      "Size..Player.2"   


joined_stats

dim (joined_stats)
names(joined_stats)

# turning df_game[5] column into Factor (it wasn't before)
print (is.factor (df_game$Infinite.Game))
print (class (df_game$Infinite.Game))

df_game [5] <- factor (df_game$Infinite.Game)
print (is.factor (df_game$Infinite.Game))

df_game

#displying the corelation in ggplot 2
library (ggplot2)
ggplot (df_game) +
	aes (x = Player1.end.position, y = Player2.end.position) + # what are we corelating?
	geom_point (colour = "purple") + #colour (?) can be either in HEX or name
	theme_minimal() 


pairs (df_game[2:14]  #
	, main = "Corelation Matrix"
	, pch = 21
	, bg = c("red", "blue")[unclass(df_game$Infinite.Game)]
	, upper.panel = NULL
	)

# end position by Factor of InFinite Game
ggplot (df_game, aes (Size.Player.1 , Player1.end.position ,Infinite.Game)) + geom_point()

#displaying the corelation in a designated ploting package



corrplot (cor(df_game[, c(2:14)]), 
	method = "number", #disply the results in numbers, not in images
	type = "upper"		#only half the matrix (no repetitions!)
)

library (correlation)


correlation::correlation (data = df_game.finite
						, method = "spearman")

unique (df_game[15])
	
df_game.agg <- cast (df_game, Size..Player.2 + Player2.end.position ~ Infinite.Game , sum)
print (df_game.agg)

write.csv (df_game, "df_game.csv", row.names = FALSE)


df_game_csv = read.csv ("/Users/hilagalapo/Documents/Aurora!/df_game.csv", header = TRUE)

