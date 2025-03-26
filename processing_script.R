files = list.files("./data")

df = data.frame()

for (file in files){
  match = read.csv(paste("./data/", file, sep = ""))
  winner = NULL
  if (match$winner[1] == "False" || 
      min(match$p1_post_stocksRemaining) == min(match$p2_post_stocksRemaining)) {
      match = match[c(1:which(match$p1_post_stocksRemaining == 
                          min(match$p1_post_stocksRemaining) & 
                          match$p2_post_stocksRemaining == 
                          min(match$p2_post_stocksRemaining))[1]),]
  } else {
    if (min(match$p1_post_stocksRemaining) > min(match$p2_post_stocksRemaining)){
      winner = 1
    } else {
      winner = 2
    }
  }
  
  match$p1_distFromCenter = (match$p1_post_positionX^2+match$p1_post_positionY^2)^(1/2)
  match$p2_distFromCenter = (match$p2_post_positionX^2+match$p2_post_positionY^2)^(1/2)
  
  match$p1_stockLost= logical(nrow(match))
  match$p2_stockLost= logical(nrow(match))
  
  for (i in 1:(nrow(match)-1)) {
    if (match$p1_post_stocksRemaining[i]>match$p1_post_stocksRemaining[i+1]) {
      match$p1_stockLost[i] = TRUE
    }
    if (match$p2_post_stocksRemaining[i]>match$p2_post_stocksRemaining[i+1]) {
      match$p2_stockLost[i] = TRUE
    }
  }
  
  if (is.null(winner)){
    match = match[-c(nrow(match)),]
  } else{
    if (winner == 1){
      match$p2_stockLost[nrow(match)] = TRUE
    } else {
      match$p1_stockLost[nrow(match)] = TRUE
    }
  }
  
  player1 = match[,c(1:8, 18, 20)]
  player2 = match[,c(1, 9:15, 19, 21)]
  
  colnames(player1) = c("frame", "x", "y", "facing_direction", "percent",
                        "shield_size", "stocks", "lcancel", "dist_from_center",
                        "stock_lost")
  
  colnames(player2) = c("frame", "x", "y", "facing_direction", "percent",
                        "shield_size", "stocks", "lcancel", "dist_from_center",
                        "stock_lost")
  
  match = rbind(player1, player2)
  df = rbind(df, match)
}