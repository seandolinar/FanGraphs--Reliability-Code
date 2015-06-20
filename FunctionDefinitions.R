####################
####DATA PREP#######
###################

####PC style sampling####
#For Plate Appearances

data_prep_pc <- function(df, J){
  
  
  agg.df <- aggregate(GameDate ~ PlayerId, data = df, FUN = length)
  agg.df <- agg.df[which(agg.df$GameDate >= J),]
  
  out.df <- df[which(df$PlayerId %in% agg.df$PlayerId),]
  out.player <- unique(out.df$PlayerId)
  key.vector <- out.player
  out.df$key <- out.df$PlayerId
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

# data_prep_pc_boot <- function(df, J){
#   
#   
#   agg.df <- aggregate(GameDate ~ PlayerId, data = df, FUN = length)
#   agg.df <- agg.df[which(agg.df$GameDate >= J),]
#   
#   out.df <- df[which(df$PlayerId %in% agg.df$PlayerId),]
#   out.player <- unique(out.df$PlayerId)
#   key.vector <- sample(x=out.player, size = 200, replace = T
#   out.df$key <- out.df$PlayerId
#   year.list <- unique(out.df$year)
#   out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
#   return(out.list)
# }



data_prep_pc_p <- function(df, J){
  
  
  agg.df <- aggregate(GameDate ~ PitcherId, data = df, FUN = length)
  agg.df <- agg.df[which(agg.df$GameDate >= J),]
  
  out.df <- df[which(df$PitcherId %in% agg.df$PitcherId),]
  out.player <- unique(out.df$PitcherId)
  key.vector <- out.player
  out.df$key <- out.df$PitcherId
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

#For AB
data_prep_pc_ab <- function(df, J){
  
  
  agg.df <- aggregate(AB ~ PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$AB >= J),]
  
  out.df <- df[which(df$PlayerId %in% agg.df$PlayerId),]
  out.df$key <- out.df$PlayerId
  print(out.df)
  out.player <- unique(out.df$PlayerId)
  key.vector <- out.player
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}



#For AB
data_prep_pc_ab_year <- function(df, J, year){
  
  df <- df[which(df$year >= year),]
  agg.df <- aggregate(AB ~ PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$AB >= J),]
  
  out.df <- df[which(df$PlayerId %in% agg.df$PlayerId),]
  out.df$key <- out.df$PlayerId
  print(out.df)
  out.player <- unique(out.df$PlayerId)
  key.vector <- out.player
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

#For BIP
data_prep_pc_bip <- function(df, J){
  
  
  agg.df <- aggregate(BIPyesno ~ PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$BIPyesno >= J),]
  
  out.df <- df[which(df$PlayerId %in% agg.df$PlayerId),]
  out.df$key <- out.df$PlayerId
  out.player <- unique(out.df$PlayerId)
  key.vector <- out.player
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

data_prep_pc_bip_p <- function(df, J){
  
  
  agg.df <- aggregate(BIPyesno ~ PitcherId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$BIPyesno >= J),]
  
  out.df <- df[which(df$PitcherId %in% agg.df$PitcherId),]
  out.df$key <- out.df$PitcherId
  out.player <- unique(out.df$PitcherId)
  key.vector <- out.player
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

data_prep_pc_new_bip <- function(df, J){
  
  
  agg.df <- aggregate(GameDate ~ batter, data = df, FUN = length)
  agg.df <- agg.df[which(agg.df$GameDate >= J),]
  
  out.df <- df[which(df$batter %in% agg.df$batter),]
  out.player <- unique(out.df$batter)
  key.vector <- out.player
  out.df$key <- out.df$batter
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

data_prep_pc_new_bip_p <- function(df, J){
  
  
  agg.df <- aggregate(GameDate ~ pitcher, data = df, FUN = length)
  agg.df <- agg.df[which(agg.df$GameDate >= J),]
  
  out.df <- df[which(df$pitcher %in% agg.df$pitcher),]
  out.player <- unique(out.df$pitcher)
  key.vector <- out.player
  out.df$key <- out.df$pitcher
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}


####JP style sampling####
#For Plate Appearances
data_prep_jp <- function(df, K){
  
  
  agg.df <- aggregate(GameDate ~ year + PlayerId, data = df, FUN = length)
  agg.df <- agg.df[which(agg.df$GameDate >= K),]
  key.vector <- paste(agg.df$year,agg.df$PlayerId)
  
  out.df <- df[which(paste(df$year,df$PlayerId) %in% key.vector),]
  out.df$key <- paste(out.df$year,out.df$PlayerId)
  
    
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

data_prep_jp_year <- function(df, K, year){
  
  df <- df[which(df$year >= year),]
  agg.df <- aggregate(GameDate ~ year + PlayerId, data = df, FUN = length)
  agg.df <- agg.df[which(agg.df$GameDate >= K),]
  key.vector <- paste(agg.df$year,agg.df$PlayerId)
  
  out.df <- df[which(paste(df$year,df$PlayerId) %in% key.vector),]
  out.df$key <- paste(out.df$year,out.df$PlayerId)
  
  
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
}

#For At Bats
data_prep_jp_ab <- function(df, K){
  
  
  agg.df <- aggregate(AB ~ year + PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$AB >= K),]
  key.vector <- paste(agg.df$year,agg.df$PlayerId)
  
  out.df <- df[which((paste(df$year,df$PlayerId) %in% key.vector & df$AB == 1)),]
  out.df$key <- paste(out.df$year,out.df$PlayerId)
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
  
  
}

data_prep_jp_ab_year <- function(df, K, year){
  
  df <- df[which(df$year >= year),]
  agg.df <- aggregate(AB ~ year + PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$AB >= K),]
  key.vector <- paste(agg.df$year,agg.df$PlayerId)
  
  out.df <- df[which((paste(df$year,df$PlayerId) %in% key.vector & df$AB == 1)),]
  out.df$key <- paste(out.df$year,out.df$PlayerId)
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list) 
}

data_prep_jp_ab_noyear <- function(df, K){
  
  
  agg.df <- aggregate(AB ~ PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$AB >= K),]
  key.vector <- paste(agg.df$PlayerId)
  
  out.df <- df[which(df$PlayerId %in% key.vector & df$AB == 1),]
  out.df$key <- out.df$PlayerId
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
  
  
}


#For Batted Balls
data_prep_jp_bip <- function(df, K){
  
  df <- df[which(df$HR != 1),] #removes all HRs
  agg.df <- aggregate(BIPyesno ~ year + PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$BIPyesno >= K),]
  key.vector <- paste(agg.df$year,agg.df$PlayerId)
  
  out.df <- df[which((paste(df$year,df$PlayerId) %in% key.vector & df$BIPyesno == 1)),]
  out.df$key <- paste(out.df$year,out.df$PlayerId)
  
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
  
  
}

data_prep_jp_bip_year <- function(df, K, year){
  
  df <- df[which(df$year >= year),]
  df <- df[which(df$HR != 1),] #removes all HRs
  
  agg.df <- aggregate(BIPyesno ~ year + PlayerId, data = df, FUN = sum)
  agg.df <- agg.df[which(agg.df$BIPyesno >= K),]
  key.vector <- paste(agg.df$year,agg.df$PlayerId)
  
  out.df <- df[which((paste(df$year,df$PlayerId) %in% key.vector & df$BIPyesno == 1)),]
  out.df$key <- paste(out.df$year,out.df$PlayerId)
  
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
  
  
}



#Takes Data Parse and makes it into a value matrix
matrix_parse <- function(df.obj, K, FUN, Random = T){
  
  stat.matrix <- NULL 
  df <- df.obj$df
  #key <- df.obj$key
  #key <- paste(df$year,df$PlayerId)
  for (i in df.obj$key.vector){
    
    #player.year.df <-  subset(df, paste(df$year,df$PlayerId) == i)
    player.year.df <- df[which(df$key == i),]
    player.vector <- FUN(player.year.df, K, Random)
    stat.matrix <- rbind(stat.matrix, player.vector)
    
  }
  
  return(stat.matrix)
}

matrix_parse_boot <- function(df.obj, K, FUN, Random = T){
  
  stat.matrix <- NULL 
  df <- df.obj$df

  key.boot <- sample(x=df.obj$key.vector, size=200, replace=T)
  for (i in key.boot){
    
    #player.year.df <-  subset(df, paste(df$year,df$PlayerId) == i)
    player.year.df <- df[which(df$key == i),]
    player.vector <- FUN(player.year.df, K, Random)
    stat.matrix <- rbind(stat.matrix, player.vector)
    
  }
  
  return(stat.matrix)
}




##################################
####Cronbach Alpha Evaluation####
#################################

FG_alpha <- function(df, K=ncol(df)){
  
  wide.vector <- c()
  tall.vector <- c()
  
  for (j in 1:ncol(df[,1:K])){
    
    tall.vector <- c(tall.vector, var(df[,j]))
    
  }
  for (i in 1:nrow(df[,1:K])){
    
    wide.vector <- c(wide.vector, sum(data.matrix(df[,1:K])[i,]))
    
  }
  
  alpha <- K/(K-1)*(1-sum(tall.vector)/var(wide.vector))
  X_bar <- mean(wide.vector)
  sd <- sqrt(var(wide.vector))
  return(list(alpha = alpha, X_bar = X_bar, K = K, sd = sd, within=tall.vector, between=wide.vector))
  
}

###################
####STAT PARSE####
##################

#Randomized
stat_random <- function(df, K){
  
  len.df = nrow(df)
  set.seed(1)
  return(df[sample(1:len.df, size=K),])
  
}




#PA-based

K_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$K[1:K])
  }
  else return(df$K[1:K])
}

BB_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$BB[1:K])
  }
  else return(df$BB[1:K])
}

OBP_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$OnBase[1:K])
  }
  else return(df$OnBase[1:K])
}

HR_parse <- function(df,K, random=T){
  
  if (random) {
    return(stat_random(df,K)$HR[1:K])
  }
  else return(df$HR[1:K])
}
 
HBP_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$HBP[1:K])
  }
  else return(df$HBP[1:K])
}

X1B_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$S[1:K])
  }
  else return(df$S[1:K])
}

X2B_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$D[1:K])
  }
  else return(df$D[1:K])
}


X3B_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$T[1:K])
  }
  else return(df$T[1:K])
}


X2B3B_parse <- function(df, K, random=T){
  
  if (random) {
    random.df <- stat_random(df,K)
    return(random.df$D[1:K]+random.df$T[1:K])
  }
  else return(df$D[1:K]+df$T[1:K])
}

ISO_parse <- function(df, K, random=T){
  
  if (random) {
    random.df <- stat_random(df,K)
    return(random.df$D[1:K]+random.df$T[1:K]*2+random.df$HR[1:K]*3)
  }
  else return(df$D[1:K]+df$T[1:K]*2+df$HR[1:K]*3)
 
}

wOBA_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$wOBA[1:K])
  }
  else return(df$wOBA[1:K])
}





#AB-based

AVG_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$hit[1:K])
  }
  else return(df$hit[1:K])
}

SLG_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$SLG[1:K])
  }
  else return(df$SLG[1:K])
}

#BIP-based -- FROM main data set

GB_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$GB[1:K])
  }
  else return(df$GB[1:K])
}


#BIP-based  -- FROM 2009-2014_BIPvelocity.csv

HH_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$Hard[1:K])
  }
  else return(df$Hard[1:K])
}

MH_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$Med[1:K])
  }
  else return(df$Med[1:K])
}

SH_parse <- function(df,K,random=T){
  
  if (random) {
    return(stat_random(df,K)$Soft[1:K])
  }
  else return(df$Soft[1:K])
}




