#' @title
#'  Create a new Monty Hall Problem game.
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#' @param ... no arguments are used by the function.
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#' @examples
#'   create_game()
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}

#' @title
#'	Select the initial door.
#' @description
#'	'select_door' selects the initial door choice in the Monty Hall game.
#' @details
#'	The door is randomly selected from the three available doors,
#'	and returns the selection as the contestant's door selection.
#'	It can be any of the three doors, including the door hiding the car.
#' @param ... no arguments are used by the function.
#' @return
#	The function returns a length of 1 numeric vector indicating the door selected.
#' @examples
#'	select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Open a goat door.
#' @description
#'	'open_goat_door' selects a losing door hiding a goat to be opened after the
#'	contestant's initial door selection.
#' @details
#'	Creates a variable storing the control flow function with two arguments for
#'	the first part of the game. In this part, the initial door selection has been
#'	made, and a "losing" door hiding a goat will be opened. Therefore, if the
#'	initial door selection hides a goat, the only other goat door must be opened.
#'	However, if the initial door selection hides a car, than either of the remaining
#'	doors may be opened since they hide goats. This function determines which other
#'	door to open based on the initial selection and returns the number of the door
#'	to be opened.
#' @param
#'	Requires two specified arguments, one for the game and one for the initial door selection.
#' @return
#'	 The function returns a length of 1 numeric vector indicating the goat door opened.
#' @examples
#'	open_goat_door( new.game, first.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   # if contestant selected goat, select the other goat
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'	Determine door strategy.
#' @description
#'	'change_door' selects a final door selection.
#' @details
#'	Selects the contestant's final door choice. Contestants can either choose to stay
#'	with their original pick or switch to the other unopened door.
#' @param
#'	Requires three arguments, including strategy, initial selection, and the opened door selection.
#' @return
#'	The function returns a length of 1 numeric vector indicating the final door selected.
#' @examples
#'	change_door(stay = TRUE, opened.door, first.pick)
#'	change_door(stay = FALSE, opened.door, first.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'	Determine if contestant wins.
#' @description
#'	'determine_winner' function specifies whether a contestant's final choice is the
#'	winning door hiding the car.
#' @details
#'	Uses a control flow to specify the conditions which must be met and
#'	printing the associated results. These requirements include whether the final
#'	door selected is the door hiding the car or not. If so, then the contestant
#'	has won, and WIN will be printed. However, if the final door selected hides a goat,
#'	the contestant has lost, and LOSE will be printed.
#' @param
#'	Requires two arguments, including the final door selection and the game being played
#'	which specifies which door hides the car.
#' @return
#'	The function returns a length of 1 character vector specifying that the contestant has either won or lost.
#' @examples
#'	determine_winner(final.pick.stay, new.game)
#'	determine_winner(final.pick.switch, new.game)

#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}



#' @title
#'	Set up the game simulation.
#' @description
#'	'play_game' combines the previous functions with specified arguments into one function
#'	that executes each step of a single game in order.
#' @details
#'	By combining the previous functions under one single, executable, game it eases the
#'	ability to play the game and loop it.
#' @param ... no arguments are used by the function.
#' @return
#'	The function returns a data frame with two rows and two columns specifying data for which
#'	strategy (stay or switch) is associated with which outcome (win or lose).
#' @examples
#'	play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}







#' @title
#'	Looping the game.
#' @description
#'	'play_n_games' loops the play_game function through a specified number of iterations.
#' @details
#'	Looping the game many times increases the statistical power and makes it more likely
#'	that the simulated statistics will be closer to the actual theoretical values.
#'	The loop must include a collector vector, an iterator, and a binding step.
#' @param
#'	Requires one specified argument, the number of desired iterations in the loop.
#' @return
#'	The function returns a list of wins and losses that can be bound into a proportions
#'	table with the row proportions for each strategy in the game.
#' @examples
#'	play_n_games
#'	results.df <- dplyr::bind_rows( results.list ) # binding step
#'	table( results.df ) %>%
#'	prop.table( margin=1 ) %>%  # row proportions
#'	round( 2 ) %>%
#'	print()
#'	return( results.df )
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector vector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list ) # binding step

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
