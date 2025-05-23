#' Extract and Compute Several Metrics from Model Output
#'
#' This function processes the output of a certain model (possibly related to fisheries)
#' to extract, compute, and format various metrics.
#'
#' @param M A list or data structure that holds the model output/results. Expected to have several
#'        named elements, including `fit`, `R`, `SSB`, etc.
#' @param proj_file (Optional) A file path to a projection file for Tier 3 results.
#'        Default is NULL.
#'
#' @return A list `B` that contains various extracted and computed metrics.
#'
#' @importFrom utils read.table
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' model_result <- list(...) # Example model result here
#' metrics <- get_vars(model_result)
#' }
get_vars <- function(M, proj_file = NULL) {
  # ... [rest of your function code here]
  B <- list()
  A <- read.table("data/intro_table.dat",header = TRUE)
  A$a1 <- format(A[1,4] ,big.mark               = ",",scientific=F,digits=1)
  A$a2 <- format(A[2,4] ,big.mark               = ",",scientific=F,digits=1)
  A$b1 <- format(A[1,5] ,big.mark               = ",",scientific=F,digits=1)
  A$b2 <- format(A[2,5] ,big.mark               = ",",scientific=F,digits=1)
  A$c1 <- format(A[1,6] ,big.mark               = ",",scientific=F,digits=1)
  A$c2 <- format(A[2,6] ,big.mark               = ",",scientific=F,digits=1)
  A$d1 <- format(A[1,7] ,big.mark               = ",",scientific=F,digits=1)
  A$d2 <- format(A[2,7] ,big.mark               = ",",scientific=F,digits=1)
  A$e1 <- format(A[1,11] ,big.mark              = ",",scientific=F,digits=1)
  A$e2 <- format(A[2,11] ,big.mark              = ",",scientific=F,digits=1)
  A$f1 <- format(A[1,12] ,big.mark              = ",",scientific=F,digits=1)
  A$f2 <- format(A[2,12] ,big.mark              = ",",scientific=F,digits=1)
  A$g1 <- format(A[1,13] ,big.mark              = ",",scientific=F,digits=1)
  A$g2 <- format(A[2,13] ,big.mark              = ",",scientific=F,digits=1)
	B$it <- A #format(A[1,2],  big.mark=",",scientific=F,digits=0)
	B$npar <- M$fit$nopar
	# String versions of these values
	B$age3plus<-M$fit$est[M$fit$names=="age_3_plus_biom"][1:length(M$R[,1])]
	B$age3plus.sd<-M$fit$std[M$fit$names=="age_3_plus_biom"][1:length(M$R[,1])]
	B$age3plus.cv<- M$age3plus.sd/M$age3plus
	B$bmsy    <- M$fit$est[M$fit$names=="Bmsy"];       B$bmsys      <- format(B$bmsy,big.mark=",",scientific=F,digits=1)
	B$bmsyr   <- M$fit$est[M$fit$names=="Bmsy2"];      B$bmsyrs      <- format(B$bmsyr,big.mark=",",scientific=F,digits=1)
	B$bmsy.sd <- M$fit$std[M$fit$names=="Bmsy"];
	B$bmsy.cv <- B$bmsy.sd/B$bmsy
	B$sprmsy  <- M$fit$est[M$fit$names=="SPR_OFL"];
	B$sprmsy.sd <- M$fit$std[M$fit$names=="SPR_OFL"];
	B$sprmsy.cv <- B$sprmsy.sd/B$sprmsy
	B$steep   <- M$fit$est[M$fit$names=="steepness"];
	B$b0      <- M$fit$est[M$fit$names=="Bzero"];      B$b0s        <- format(B$b0,  big.mark=",",scientific=F,digits=1)
	B$b100    <- M$fit$est[M$fit$names=="SB100"];      B$b100s      <- format(B$b100,big.mark=",",scientific=F,digits=1)
	B$dynb0   <- M$fit$est[M$fit$names=="B_Bnofsh"];   B$dynb0s     <- format(B$dynb0,big.mark=",",scientific=F,digits=2)
	B$repl_yld<- M$repl_yld;                           B$repl_ylds  <- format(round(B$repl_yld,-1),big.mark=",",scientific=F,digits=1)
	B$curssb   <- M$SSB[length(M$SSB[,1]),2];          B$curssbs    <- format(round(B$curssb,-1),big.mark=",",scientific=F,digits=1)
	B$curssb.sd<- M$SSB[length(M$SSB[,1]),3];          B$curssb.sds <- format(round(B$curssb.sd,-1),big.mark=",",scientific=F,digits=1)
	B$curssb.cv <- B$curssb.sd/B$curssb
	B$nextyrssb<- as.numeric(M$future_SSB[1,2]);      B$nextyrssbs <- format(round(B$nextyrssb,-2) ,big.mark=",",scientific=F)
	B$nextyrssb.sd <- as.numeric(M$future_SSB.sd[1,2])
	B$nextyrssb.cv <- B$nextyrssb.sd/B$nextyrssb
	B$avgrec <- mean(M$R[14:54,2]);     B$avgrecs <- format(round(B$avgrec,-1),big.mark=",",scientific=F)
	B$yc2000 <- (M$R[M$R[,1]==2001,2]); B$yc2000s <- format(round(B$yc2000,-2),big.mark=",",scientific=F)
	B$yc2006 <- (M$R[M$R[,1]==2007,2]); B$yc2006s <- format(round(B$yc2006,-2),big.mark=",",scientific=F)
	B$yc2008 <- (M$R[M$R[,1]==2009,2]); B$yc2008s <- format(round(B$yc2008,-2),big.mark=",",scientific=F)
	B$yc2012 <- (M$R[M$R[,1]==2013,2]); B$yc2012s <- format(round(B$yc2012,-2),big.mark=",",scientific=F)
	B$yc2013 <- (M$R[M$R[,1]==2014,2]); B$yc2013s <- format(round(B$yc2013,-2),big.mark=",",scientific=F)
	B$yc2018 <- (M$R[M$R[,1]==2019,2]); B$yc2018s <- format(round(B$yc2018,-2),big.mark=",",scientific=F)
	B$MAR_bts  <- median(abs(log(M$ob_bts)-log(M$eb_bts)))
	B$MAR_ats  <- median(abs(log(M$ob_ats)-log(M$eb_ats)))
	B$MAR_avo  <- median(abs(log(M$obs_avo)-log(M$pred_avo)))
	B$MAR_cpue <- median(abs(log(M$obs_cpue)-(M$pred_cpue)))
	B$rmse_bts <- mean((log(M$ob_bts)-log(M$eb_bts))^2)^.5
	B$rmse_ats <- mean((log(M$ob_ats)-log(M$eb_ats))^2)^.5
	B$rmse_avo <- mean((log(M$obs_avo)-log(M$pred_avo))^2)^.5
	B$rmse_cpue<- mean((log(M$obs_cpue)-log(M$pred_cpue))^2)^.5
	B$sdnr_bts <- M$sdnr_bts
	B$sdnr_ats <- M$sdnr_ats
	B$sdnr_avo <- M$sdnr_avo

	B$msyr <- M$fit$est[M$fit$names=="Fmsy2"]
	B$b40  <- .40*B$b100; B$b40s  <- format(B$b40,big.mark=",",scientific=F,digits=1)
	B$b35  <- .35*B$b100; B$b35s  <- format(B$b35,big.mark=",",scientific=F,digits=1)

	pdf     <- as.data.frame(M$T1)
	names(pdf)   <- c("Year","ABC","OFL", "SSB","age3plus","CatchFut","harmeanF","arithmeanF","geomB","SPRABC","SPROFL","Tier2","Tier1.5","Fabc","Fofl","Adj","Fmsyr")
	B$arithmeanF <-  round(pdf$arithmeanF[1],3)
	B$harmeanF   <-  round(pdf$harmeanF[1],3)
	B$maxFabc    <-  B$harmeanF
	B$age3plus1  <-  pdf$age3plus[1]
	B$age3plus1s <-  format(round(1e3*pdf$age3plus[1],-3),big.mark=",",scientific=F,digits=1)
	B$age3plus2  <-  pdf$age3plus[2]
	B$age3plus2s <-  format(round(1e3*pdf$age3plus[2],-3),big.mark=",",scientific=F,digits=1)
	B$ssb1       <-  pdf$SSB[1]
	B$ssb2       <-  pdf$SSB[2]
	B$ssb1s      <-  format(round(1e3*pdf$SSB[1],-3),big.mark=",",scientific=F,digits=1)
	B$ssb2s      <-  format(round(1e3*pdf$SSB[2],-3),big.mark=",",scientific=F,digits=1)
	B$ABC_biom1  <-  pdf$geomB[1]; B$ABC_biom1s  <- format(round(B$ABC_biom1,0),big.mark=",",scientific=F,digits=1)
	B$ABC_biom2  <-  pdf$geomB[2]; B$ABC_biom2s  <- format(round(B$ABC_biom2,0),big.mark=",",scientific=F,digits=1)
	B$FABC1      <-  pdf$Fabc[1];  B$FABC1s      <- format(round(B$FABC1,3),big.mark=",",scientific=F,digits=3)
	B$FABC2      <-  pdf$Fabc[2];  B$FABC2s      <- format(round(B$FABC2,3),big.mark=",",scientific=F,digits=3)
	B$FOFL1      <-  pdf$Fofl[1];  B$FOFL1s      <- format(round(B$FOFL1,3),big.mark=",",scientific=F,digits=3)
	B$FOFL2      <-  pdf$Fofl[2];  B$FOFL2s      <- format(round(B$FOFL2,3),big.mark=",",scientific=F,digits=3)
	B$Adj1       <-  pdf$Adj[1];   B$Adj1s       <- format(round(B$Adj1,3),big.mark=",",scientific=F,digits=3)
	B$Adj2       <-  pdf$Adj[2];   B$Adj2s       <- format(round(B$Adj2,3),big.mark=",",scientific=F,digits=3)

	B$Tier2_ABC1   <-  pdf$Tier2[1]
	B$Tier2_ABC2   <-  pdf$Tier2[2]
	B$Tier1.5_ABC1 <-  pdf$Tier1.5[1]
	B$Tier1.5_ABC2 <-  pdf$Tier1.5[2]

	B$Tier2_ABC1s   <-  format(round(1e3*B$Tier2_ABC1,-3),big.mark=",",scientific=F,digits=1)
	B$Tier2_ABC2s   <-  format(round(1e3*B$Tier2_ABC2,-3),big.mark=",",scientific=F,digits=1)
	B$Tier1.5_ABC1s <-  format(round(1e3*B$Tier1.5_ABC1,-3),big.mark=",",scientific=F,digits=1)
	B$Tier1.5_ABC2s <-  format(round(1e3*B$Tier1.5_ABC2,-3),big.mark=",",scientific=F,digits=1)

	B$maxabc1    <-  pdf$ABC[1]
	B$maxabc2    <-  pdf$ABC[2]
	B$maxabc1s   <-  format(round(1e3*pdf$ABC[1],-3),big.mark=",",scientific=F,digits=1)
	B$maxabc2s   <-  format(round(1e3*pdf$ABC[2],-3),big.mark=",",scientific=F,digits=1)
	B$ofl1       <-  pdf$OFL[1]
	B$ofl2       <-  pdf$OFL[2]
	B$ofl1s      <-  format(round(1e3*pdf$OFL[1],-3),big.mark=",",scientific=F,digits=1)
	B$ofl2s      <-  format(round(1e3*pdf$OFL[2],-3),big.mark=",",scientific=F,digits=1)
	#B$bfs        <- read.csv("../doc/data/proj.csv",header=T)
  B <- c(B,get_tier3_res(proj_file))
	#B$abc1       <- 0.85*B$maxabc1 #B$Tier3_ABC1
	#B$abc2       <- 0.85*B$maxabc2 #B$Tier3_ABC2
	# Mean F
	if (dim(M$future_catch)[1]>8) {
		B$abc1constF <- M$future_catch[12,1]
		B$abc2constF <- M$future_catch[12,2]
	} else{
		B$abc1constF <- M$future_catch[4,1]
		B$abc2constF <- M$future_catch[4,2]
  }

	# Set recommended ABC here--Tier 3
	B$abc1       <- (B$Tier3_ABC1)
	B$abc2       <- (B$Tier3_ABC2)
	#B$abc1       <- (B$Tier2_ABC1)
	#B$abc2       <- (B$Tier2_ABC2)

	B$abc1constFs<- format(round(1e3*B$abc1constF,-3),big.mark=",",scientific=F,digits=1)
	B$abc2constFs<- format(round(1e3*B$abc2constF,-3),big.mark=",",scientific=F,digits=1)

	B$abc1s      <- format(round(1e3*B$abc1,-3),big.mark=",",scientific=F,digits=1)
	B$abc2s      <- format(round(1e3*B$abc2,-3),big.mark=",",scientific=F,digits=1)
	B$fabc1      <- round(B$Tier2_ABC1 /B$ABC_biom1,3)
	B$fabc2      <- round(B$Tier2_ABC2 /B$ABC_biom2,3)
	B$fabc1s     <- B$Tier2_ABC1 /B$ABC_biom1

	# Decision table stuff
	ord <- c(8,2:3,1,4:7)
  B$catch_dec_tab <- M$dec_tab_catch[ord]
  B$pfcur_fmsy    <- 1- pnorm(1,M$Fcur_Fmsy[ord] ,    M$Fcur_Fmsy.sd[ord]  )
  B$pbcur_bmsy    <- pnorm(1, M$Bcur_Bmsy[ord]   ,  M$Bcur_Bmsy.sd[ord]   )
  B$pbcur_bmean   <- pnorm(1, M$Bcur_Bmean[ord]  ,  M$Bcur_Bmean.sd[ord] )
  B$pbcur2_bmsy   <- pnorm(1, M$Bcur2_Bmsy[ord]  ,  M$Bcur2_Bmsy.sd[ord]  )
  B$pbcur2_b20    <- pnorm(1, M$Bcur2_B20[ord]   ,  M$Bcur2_B20.sd[ord]  )
  B$pbcur3_bcur   <- pnorm(1, M$Bcur3_Bcur[ord]  ,  M$Bcur3_Bcur.sd[ord]  )
  B$pbcur3_bmean  <- pnorm(1, M$Bcur3_Bmean[ord] ,  M$Bcur3_Bmean.sd[ord])
  B$plta1_5r      <- 1-pnorm(1, M$LTA1_5R[ord]     ,  M$LTA1_5R.sd[ord]    )
  B$pmatdiv1      <- pnorm(1, M$MatAgeDiv1[ord]  ,  M$MatAgeDiv1.sd[ord] )
  B$pmatdiv2      <- pnorm(1, M$MatAgeDiv2[ord]  ,  M$MatAgeDiv2.sd[ord] )
  B$prel_effort   <- 1-pnorm(1, M$RelEffort[ord]   ,  M$RelEffort.sd[ord]  )
  B$plta1_5       <- 1-pnorm(1, M$LTA1_5[ord]      ,  M$LTA1_5.sd[ord]    )
  B$bmsystatus   <- ifelse(B$bmsy<B$ssb1,"above","below")
	B$b40status    <- ifelse(B$b40 <B$ssb1,"above","below")
	B$tier1ab1     <- ifelse(B$bmsy<B$ssb1,"1a","1b")
	B$tier1ab2     <- ifelse(B$bmsy<B$ssb2,"1a","1b")
	B$tier2ab1     <- ifelse(B$bmsy<B$ssb1,"2a","2b")
	B$tier2ab2     <- ifelse(B$bmsy<B$ssb2,"2a","2b")
	B$tier3ab1     <- ifelse(B$b40 <B$ssb1,"3a","3b")
	B$tier3ab2     <- ifelse(B$b40 <B$ssb2,"3a","3b")

  return(B)
}
