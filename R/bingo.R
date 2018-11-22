

bingo_card <- function(words, side_len){
  stopifnot(side_len > 0)
  side_len <- round(side_len, digits = 0)
  if(side_len == 1){
    return("JUST")
  }
  total_words <- side_len ** 2
  card <- NULL
  for(i in 1:side_len){
    card <- rbind(card, sample(words, side_len, replace = TRUE))
  }
  centre <- side_len /2 + 1
  card[centre, centre] <- "JUST"

  return(card)
}


send_card <- function(card, player){
  slackr::slackr_setup()
  Sys.setenv("SLACK_CHANNEL" = player)
  slackr::slackr_msg(txt = "your bingo card is:")
  co <- capture.output(card)
  co <- co[-1]
  slackr::slackr_msg(txt = co)
  Sys.setenv("SLACK_CHANNEL" = "")
}


game_setup <- function(words, side_len, players){

  for(player in players){
    player_card <- bingo_card(words, side_len)
    send_card(player_card, player)
  }

  return(TRUE)
}


