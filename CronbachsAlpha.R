#FanGraphs Reliability Project
#v 0.0 -- sean setup
#v 0.1 -- first build
#v 0.2 -- adding pitchers and evaluation
#v 0.3 -- cleaning up code
#v 1.0 -- prep code for FG post

## functions_have_underscores()
## variables.have.periods
## i,j,k are index variables
## a,b,c,d,e,f are test variables.  If they make it in to actual code, these are errors.
##
## local variables in functions use simplest names as possible such as df or vector

#sets working directory
#setwd('~/fg/projects/fg_SSS/GITHUB/FanGraphs---Reliability-data/data') #Sean Mac
setwd('~/projects/fg/fg_SSS/') #Sean Linux

out.dir <- '~/projects/fg/fg_SSS/out/'

######FUNCTION LOAD######

source('FanGraphs--Reliability Code/FunctionDefinitions.R')

######DATA LOAD######

data.ALL <- read.csv('FanGraphs---Reliability-data/data/2002-2015_MLBPlateAppearances.csv')
data.ALL <- data.ALL[which(data.ALL$year < 2015),]



#creates list to get functions for specific stats
#FUN.list - PA-based
FUN.list <- list(list(K_parse,'K_pct'),list(BB_parse,'BB_pct'),list(OBP_parse,'OBP'),
                 list(HR_parse,'HR'), list(HBP_parse,'HBP'), list(X1B_parse,'X1B'),
                 list(wOBA_parse,'wOBA'),list(X2B_parse,'2B_pct'),list(X3B_parse,'3B_pct'))

#AB-based
AB.list <- list(list(AVG_parse,'AVG'), list(SLG_parse,'SLG'), list(ISO_parse,'ISO'))

#BIP-based
BIP.list <- list(list(AVG_parse,'BABIP'),list(wOBA_parse,'wOBABIP'))


#########################
####Carleton Sampling####
#########################

#Alternate sampling method
#PA.list Sets interval for PA/AB/or BIP
#PC.denom.list sets the requirement for the prep function
PA.list <- seq(from=10, to=2000, by=10)
PC.denom.list <- list(PA=2000,AB=2000,BIP=1000)

#Sets if the data matrix out of the prep function to be random or chronological...T = random, F = chronological
Random = F

df.prep <- data_prep_pc(data.ALL, PC.denom.list$PA)
ab.prep <- data_prep_pc_ab(data.ALL, PC.denom.list$AB)
bip.prep <- data_prep_pc_bip(data.ALL, PC.denom.list$BIP)

ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
  for (j in FUN.list) {
    
    player.year.matrix <- matrix_parse(df.prep,i,j[[1]], Random) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
  }
  
  for (j in AB.list) {
    
    player.year.matrix <- matrix_parse(ab.prep,i,j[[1]], Random) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
    
  }
  
  for (j in BIP.list) {
    
    player.year.matrix <- matrix_parse(bip.prep,i,j[[1]], Random) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
    
  }
 
 
  
}

proc.time() - ptm

#df for output
write.csv(out.df, file=paste(out.dir, 'fg_alpha_out_PC_.csv'), row.names=F)


##########################
####FanGraphs Sampling####
##########################

#Sets if the data matrix out of the prep function to be random or chronological...T = random, F = chronological
Random = T
PA.list <- seq(from=10, to=500, by=10) #600 PA, 500 AB, 400 BIP

ptm <- proc.time() #times
out.df <- NULL #calibrates out data frame
for (i in PA.list){
  
 
  df.prep <- data_prep_jp_year(data.ALL, i, 2009)
  ab.prep <- data_prep_jp_ab_year(data.ALL, i, 2009)
  bip.prep <- data_prep_jp_bip_year(data.ALL, i, 2009)
  
  
    for (j in FUN.list) {
      
      player.year.matrix <- matrix_parse(df.prep,i,j[[1]],random) #get rid of the matrix_parse i
      alpha.list <- FG_alpha(player.year.matrix)
      out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                             sample_sd = alpha.list$sd)
      out.df <- rbind(out.df, out.list)
    }
  
  for (j in AB.list) {
    
    player.year.matrix <- matrix_parse(ab.prep,i,j[[1]],random) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
    
  }
   
    for (j in BIP.list) {
      
      player.year.matrix <- matrix_parse(bip.prep,i,j[[1]],random) #get rid of the matrix_parse i
      alpha.list <- FG_alpha(player.year.matrix)
      out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                             sample_sd = alpha.list$sd)
      out.df <- rbind(out.df, out.list)
      
    }
      
  
}
proc.time() - ptm


write.csv(out.df, file=paste(out.dir, 'fg_alpha_out_JP_20092014_.csv'), row.names=F)






