#' Bar plot of variable importances, passing just a dataframe
#' of the name, importance, and response (target levels, faceted
#' if multinomial).  Note: user must sort and set up the data frame
#' properly depending on the model output.
#' 
#' @param df data frame 
#' @export
splotVarImpBase <- function(df, xlabel) {
	
	## Note: with coord_flip(), ylab() refers to y variable (aes(y=xxx)), but 
	## in the theme(), the xxx.y settings are for x variable (flipped y-axis)
	ggplot(df, aes(x=name, y=importance, width=.4)) +
			geom_bar(stat='identity', fill="steelblue") +
			ylab(xlabel) +
			theme_bw() +
			theme(axis.title.y =element_blank(),
					axis.title.x =element_text(size=12, face = "bold"),
					axis.text.x =element_text(angle=45, hjust=1),
					strip.text = element_text(size=10, face = "bold")) +
			coord_flip() +
			facet_wrap(~response, nrow = 1)
	
}



#' Bar plot of variable importances.
#' 
#' @param name names of variables (sorted by order of plot, high to low)
#' @param importance of variables (sorted by order of plot, high to low)
#' @param numVars the total number of variables to plot (default=25)
#' @param responseOrder the order of responses if multinomial
#' @return plot
#' @export
splotVarImp <- function(name, importance, numVars=25, responseOrder=NULL, xlabel="Variable Importance") {
	require(tidyr)
	if (dim(importance)[2]==1) {
		df= importance
		df$name=name
	} else {
		df = data.frame(name=name, importance)
	}
	df$name=factor(df$name,levels=rev(name))
	if (length(name)>numVars) df=df[1:numVars, ]
	df = df %>% gather(response, importance, -name)
	if (!is.null(responseOrder)) df$response=factor(df$response, levels=responseOrder)
	else df$response=factor(df$response)
	imp=splotVarImpBase(df, xlabel)
}



#' Plots the ratios of TP, TN, FP, and FN at each probability cutoff value. 
#' 
#' @param prob  predicted probability 
#' @param obs observed labels
#' @param posLabel 'positive' label
#' @param negLabel 'negative' label
#' @param probSeq the probability threshold sequence to use
#' @export
splotCMProbs <- function(prob, obs, posLabel, negLabel, 
		probSeq=seq(0.005,0.995, by=0.005)) {
	require(RColorBrewer)
	require(ggplot2)
	
	tabs=list()
	tabs = lapply(probSeq, function(cut) {
				optPredLabels=ifelse(prob>=cut, posLabel,negLabel)
				optPredLabels=factor(optPredLabels, levels=c(negLabel, posLabel))
				cmtemp = confusionMatrix(optPredLabels, obs)
				df = as.data.frame(cmtemp$table)
				df$cut = cut
				df
			})
	
	posDF=do.call("rbind", tabs)
	
	posDF$Reference = as.character(posDF$Reference)
	posDF$Reference[posDF$Prediction==negLabel & posDF$Reference==negLabel]="True Negative"
	posDF$Reference[posDF$Prediction==posLabel & posDF$Reference==negLabel]="False Positive"
	posDF$Reference[posDF$Prediction==posLabel & posDF$Reference==posLabel]="True Positive"
	posDF$Reference[posDF$Prediction==negLabel & posDF$Reference==posLabel]="False Negative"
	posDF$Reference = factor(posDF$Reference, levels=c("True Positive", "False Negative", 
					"False Positive", "True Negative"))
	
	diff=posDF$cut[5]-posDF$cut[1]
	
	# Get max cut beyond which all is the same
	posDFTemp = posDF %>% select(-Prediction) %>% distinct(Reference, Freq)
	posDF = posDF %>% filter(cut<=max(posDFTemp$cut))
	xlims=c(min(posDF$cut), max(posDF$cut))
	
	# Get the minimum % value to use
	tempDF = posDF %>% group_by(cut) %>% 
			mutate(prop=Freq/sum(Freq))%>% 
			dplyr::summarize(maxVal=max(prop)) 
	ylims=c(min(tempDF$maxVal),1)
	
	
	
	plot1 <- ggplot(posDF, aes(x=cut, y=Freq, fill=Reference)) +
			geom_bar(stat="identity", position="fill", width=diff)+
#			scale_y_continuous("Proportion", limits=ylims) + 
			scale_fill_manual("Predicted",values = brewer.pal(5,"YlGnBu")[c(2:(5))])+
			xlab("Probability Theshold") +
			ylab("Proportion") +
			coord_cartesian(ylim=ylims, xlim=xlims) +
			theme_bw() +
			theme(axis.title = element_text(face="bold"))
	
	plot1
}



#' Ratio of true positives to false positives.
#' 
#' @param prob  predicted probability 
#' @param obs observed labels
#' @param posLabel 'positive' label
#' @param negLabel 'negative' label
#' @param probSeq the probability threshold sequence to use
#' @export
splotTPFPProbs <- function(prob, obs, posLabel, negLabel, probSeq=seq(0.05,0.95, by=0.05)) {
	require(ggplot2)
	require(dplyr)
	
	
	tabs=list()
	tabs = lapply(probSeq, function(cut) {
				optPredLabels=ifelse(prob>=cut, posLabel,negLabel)
				optPredLabels=factor(optPredLabels, levels=c(negLabel, posLabel))
				cmtemp = confusionMatrix(optPredLabels, obs)
				df = as.data.frame(cmtemp$table)
				df$cut = cut
				df
			})
	
	tabDF=do.call("rbind", tabs)
	
#	print(str(tabDF))
	
	posDF=tabDF %>% dplyr::filter(Prediction==posLabel)
	posDF = posDF %>% dplyr::mutate(Freq = ifelse(Reference==negLabel,-1*Freq,Freq))
	posDF$Reference = as.character(posDF$Reference)
	
#	posDF = subset(tabDF, Prediction==posLabel)
#	posDF$Freq=ifelse(posDF$Reference==negLabel,-1*posDF$Freq, posDF$Freq)
	posDF$Reference[posDF$Reference==negLabel]="False Positives"
	posDF$Reference[posDF$Reference==posLabel]="True Positives"
	
	Freq=posDF$Freq
	CutVal = posDF$cut
	
	plot1 <- ggplot(posDF, aes(x=cut, y=Freq, fill=Reference)) +
			geom_bar(stat="identity", position="identity")+
			geom_text(aes(label=ifelse(Freq < 0,-1*Freq,Freq)),
					hjust = ifelse(Freq > 0,0,1),colour = "Black", size=4) +
			coord_flip() +
			scale_y_continuous("Number of Observations",expand=c(0.25,0)) + 
			xlab("Probability Theshold") +
			theme_bw() +
			theme(axis.text.x  = element_blank(),axis.title = element_text(face="bold"))
	
	plot1
}



#' Typical ROC plot, with ggvis hover for cutoff point.
#' 
#' @param prob predicted probability 
#' @param obs observed labels
#' @export
iplotROC <- function(prob, obs) {
	require(ROCR)
	require(Hmisc)
	require(ggvis)
	
	if (is.character(obs)) obs = factor(obs)
	# Hmisc calcs (for Gini, also spits out AUC)
	pmetrics=rcorr.cens(prob, obs)[1:2]
	
	# ROCR calcs for tpr/fpr
	eval <- prediction(prob, obs)
	perf=performance(eval,"tpr","fpr")
	
	f1 = performance(eval, 'f')
	best_f1 = which.max(f1@y.values[[1]])
	
	rocDF=data.frame(x=perf@x.values[[1]], y=perf@y.values[[1]], cutoff=perf@alpha.values[[1]])
	rocDF = rocDF %>% mutate(cutoff=ifelse(is.infinite(cutoff),1,cutoff))
	
	fDF = rocDF[unlist(best_f1),]
	
	rocDF %>% ggvis(x = ~x, y = ~y) %>% 
			layer_points(data=fDF, size := 500, fillOpacity := 0.5, fill:= "steelblue") %>%
			layer_text(data=fDF, x=~x+0.05, y=~y-0.05, 
					text:= "F-1", baseline:="middle", fontSize:=12, fontWeight:="bold") %>%  
			layer_lines(strokeWidth := 5) %>%
			layer_points(size := 100, fillOpacity := 0.0001, fill = ~cutoff) %>% #fill := NA) %>%
			add_axis("x", title = "False Positive Rate", 
					properties = axis_props(title = list(fontSize = 12))) %>%
			add_axis("y", title = "True Positive Rate", 
					properties = axis_props(title = list(fontSize = 12))) %>%
			hide_legend("fill") %>%
			add_tooltip(function(data) {paste0("Cutoff: ", round(as.numeric(data$cutoff), digits=2))}, "hover") 
}





#' Typical ROC plot
#' 
#' @param prob predicted probability 
#' @param obs observed labels
#' @export
splotROC <- function(prob, obs) {
	require(ROCR)
	require(Hmisc)
	require(ggplot2)
	
	if (is.character(obs)) obs = factor(obs)
	# Hmisc calcs (for Gini, also spits out AUC)
	pmetrics=rcorr.cens(prob, obs)[1:2]
	
	# ROCR calcs for tpr/fpr
	eval <- prediction(prob, obs)
	perf=performance(eval,"tpr","fpr")
	
	rocDF=data.frame(x=perf@x.values[[1]], y=perf@y.values[[1]], cutoff=perf@alpha.values[[1]])
	rocDF = rocDF %>% mutate(cutoff=ifelse(is.infinite(cutoff),1,cutoff))
	
	f1 = performance(eval, 'f')
	best_f1 = which.max(f1@y.values[[1]])
	fDF = rocDF[unlist(best_f1),]
	
	myPalette <- colorRampPalette(c("firebrick","gold","steelblue"))
	
	g=ggplot(rocDF, aes(x=x, y=y, colour=cutoff))+
			geom_point(data=fDF, shape=20, size=10, colour="steelblue", alpha=.5) +
			annotate("text", x = fDF$x+0.05, y = fDF$y-0.05, size=4.5,  
					hjust=0, label = paste0("F-1 Cutoff:",round(as.numeric(fDF$cutoff),3))) +
			geom_line(size=2)+
			scale_colour_gradientn("Probability\nThreshold", colours = myPalette(11), values=seq(0,1,by=.1)) +
#			guide_legend("Probability\nThreshold") +
			#			geom_point(size=5, shape=20)+
			theme_bw()+
			xlab("False Positive Rate") +
			ylab("True Positive Rate") +
			annotate("text", y = .1, x = .9, size=4.5, 
					label = paste("AUC: ",round(pmetrics[1],digits=3),"\n","Gini: ",round(pmetrics[2],digits=3),sep="")) +
			theme(axis.title = element_text(face="bold"))
	g
}




#' Bar plot of confusion matrices
#' 
#' @param pred predicted labels.
#' @param obs observed labels.
#' @export
splotCM <- function (pred, obs) {
	require(caret)
	require(RColorBrewer)
	require(ggplot2)
	
	CM = confusionMatrix(pred, obs)
	
	mat=CM$table
	mat=as.data.frame(mat)
	
	levs = length(unique(mat$Reference))
	
	g = ggplot(mat,aes(Reference,y=as.numeric(Freq), fill=Prediction)) + 
			geom_bar(stat = "identity", position = "stack", color="gray50") +
#			xlab("") +
			ylab("Frequency") +
			theme_bw()+ 
			scale_fill_manual("Predicted",values = brewer.pal(levs+1,"YlGnBu")[c(2:(levs+1))])+
			guides(fill = guide_legend(override.aes = list(colour = NULL)))+
			theme(legend.key = element_rect(colour = "gray50"),
					axis.title.x =element_blank(),
					axis.title.y =element_text(size=12, face = "bold", colour = "black"),
					axis.text.x= element_text(size=10, angle=45, hjust = 1),
					axis.text.y= element_text(size=10, angle=90, hjust=.5))
	g	
}




#' Bar plot of confusion matrices faceted for different probability thresholds.
#' 
#' @param prob probability values
#' @param obs observed labels
#' @param posLabel 'positive' label
#' @param negLabel 'negative' label
#' @param probSeq the probability threshold sequence to use
#' @export
splotCMSeq <- function (prob, obs, posLabel, negLabel, probSeq=seq(0.1,0.9, by=0.1)) {
  require(caret)
  require(RColorBrewer)
  
  mat=NULL
  vals=NULL
  for (cut in probSeq) {
    pred = ifelse(prob >= cut, posLabel, negLabel)
    pred = factor(pred, levels=c(negLabel, posLabel))
    if (is.null(mat)) {
      CM= confusionMatrix(pred, obs, positive=posLabel)
      mat=CM$table
      mat=as.data.frame(mat)
      mat$Cutoff=cut
      
      vals=as.data.frame(t(CM$byClass))
      vals$Cutoff=cut
    } else {
      CM= confusionMatrix(pred, obs, positive=posLabel)
      matt=CM$table
      matt=as.data.frame(matt)
      matt$Cutoff=cut
      mat = rbind(mat, matt)
      
      valst=as.data.frame(t(CM$byClass))
      valst$Cutoff=cut
      vals = rbind(vals, valst)			
    }
  }
  
  levs = length(unique(mat$Reference))
  maxVal = max(mat$Freq)
  
  
  vals$x=posLabel
  vals$y=1.2*maxVal	
  vals$label = paste(vals$Cutoff,": TP=",round(vals$Sensitivity,2),", TN=", round(vals$Specificity,2), sep="")
  vals$Prediction=negLabel
  
  mat = mat %>% left_join(vals %>% dplyr::select(Cutoff, label), by="Cutoff")
  
  g = ggplot(mat,aes(Reference,y=as.numeric(Freq), fill=Prediction)) + 
      geom_bar(stat = "identity", position = "stack", color="gray50") +
      xlab("") +
      ylab("Frequency")+
      theme_bw()+ 
      scale_fill_manual("Predicted",values = brewer.pal(levs+1,"YlGnBu")[c(2:(levs+1))])+
      guides(fill = guide_legend(override.aes = list(colour = NULL)))+
      theme(legend.key = element_rect(colour = "gray50"),
          strip.text= element_text(size=8, face="bold"),
          axis.title.x =element_blank(),
          axis.title.y =element_text(size=12, face = "bold", colour = "black"),
          axis.text.x= element_text(size=10, angle=45, hjust = 1),
          axis.text.y= element_text(size=10, angle=90, hjust=.5)) +
      facet_wrap(~label)
  g	
}




#' Plots the lift for binomial response.
#' 
#' @param prob  predicted probability 
#' @param obs observed labels
#' @param posLabel 'positive' label
#' @param negLabel 'negative' label
#' @param probSeq the probability threshold sequence to use
#' @export
splotLift <- function (prob, obs, posLabel, negLabel) {
	require(dplyr)
	require(Hmisc)
	require(ggplot2)
	
	if (is.character(obs)) obs = as.factor(obs)
	
	df =data.frame(obs=obs, pred=prob, stringsAsFactors=FALSE)
	
	
	numUni=length(unique(df$obs))
	splits=split(1:length(df[,1]),sort(1:length(df[,1]) %% 10))
	
	nms=names(df)[which(names(df) != "obs")]
	nms=posLabel
	
	#set of dataframe to hold the resultant predictions
	dfPlot=data.frame(Response=sort(rep(nms,10)), Decile=rep(1:10,length(nms)), Value=1)
	
	perfVals=data.frame(nms=nms, auc=0, gini=0)
	
	
	dftemp = df %>% arrange(desc(pred))
	nm=posLabel #str_replace(nms[i],"pred.","")
	#get the frequency per decile
	
	for (j in 1:10){
		indices=unlist(splits[1:j])
		dftemp2=dftemp[indices,]
		dfPlot[dfPlot$Response==nm & dfPlot$Decile==j,"Value"]=(length(dftemp2[dftemp2$obs==nm,1])/length(dftemp2[,1]))/(length(df[df$obs==nm,1])/length(df[,1]))
	}
	
	
	# Get the AUC and gini associated with each
	tvals=rcorr.cens(dftemp$pred, dftemp$obs)[1:2]
	perfVals[perfVals$nms==nm,which(names(perfVals)=="auc")]=tvals['C Index']
	perfVals[perfVals$nms==nm,which(names(perfVals)=="gini")]=tvals['Dxy']
	
	
	textString=paste("AUC=",round(perfVals[perfVals$nms==posLabel,which(names(perfVals)=="auc")],digits=2),
			"\nGini=",round(perfVals[perfVals$nms==posLabel,which(names(perfVals)=="gini")],digits=2),sep="")			
	
	dfPlot$Decile=dfPlot$Decile*10
	
	maxval=max(dfPlot$Value)
	
	g=ggplot(dfPlot,aes(x=Decile, y=Value)) + 
			geom_line(size=1.5)+
			geom_abline(aes(slope=0, intercept=1, colour="red"))+
			xlab("Predicted (decreasing by score)") +
			ylab("Lift") +
			annotate("text", y = maxval*.95, x = 95, size=4.5, 
					label = paste("AUC: ",round(perfVals$auc,digits=3),"\n","Gini: ",round(perfVals$gini,digits=3),sep="")) +
			theme_bw() +
			theme(axis.title = element_text(face="bold"))  
	
	g
}




#' Plots the standardized residuals versus the predicted values.
#' 
#' @param obs observed values
#' @param pred predicted values
#' @export
splotResids <- function (obs, pred) {
	require(ggplot2)
	
	df=data.frame(obs=obs, pred=pred, resid=obs-pred)
	df$stdres = df$resid/sd(df$resid)
	
	xlims=c(min(df$pred), max(df$pred))
	ylims=c(min(df$stdres), max(df$stdres))
	
	
	g = ggplot(df,aes(x=pred,y=stdres))+
			stat_density2d(aes(alpha=..level..),alpha=.05, geom="polygon",fill="darkblue") +
			geom_point(fill="darkblue", colour=NA,shape=21, size=4, alpha=.5)+
			scale_alpha_continuous(guide=FALSE)+ #
			scale_x_continuous("Predicted",expand=c(0.2, 0))+
			scale_y_continuous("Standardized Residuals",expand=c(0.2, 0))+ 
			theme_bw() +
			theme(axis.title = element_text(face="bold"))  
	g
}





#' QQ plot for a normal distirbution.
#' 
#' @param obs observed values
#' @param pred predicted values
#' @export
splotQQNorm <- function (obs, pred) {
	require(ggplot2)
	
	df=data.frame(obs=obs, pred=pred, resid=obs-pred)
	
	# following four lines from base R's qqline()
	vec=obs-pred
	y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	
	g = ggplot(df, aes(sample = resid)) + 
			stat_qq(shape=21, colour=NA, fill="darkblue", size=5,alpha=.5) +
			xlab("Normal Quantiles") +
			ylab("Residuals")+
			geom_abline(slope = slope, intercept = int,colour="darkblue", linetype="dashed", size=1) +
			theme_bw() +
			theme(axis.title = element_text(face="bold"))  
	g
}


#' Observed versus predicted values with residuals.
#' 
#' @param obs observed values
#' @param pred predicted values
#' @export
splotObsPred = function(obs, pred) {
	require(ggplot2)
	
	df= data.frame(obs=obs, pred=pred, resid=obs-pred)
	df$stdres = abs(df$resid)/sd(df$resid)
	
	g = ggplot(df,aes(x=obs,y=pred, fill=stdres))+
			stat_density2d(aes(alpha=..level..),alpha=.075, geom="polygon",fill="darkblue") +
			scale_alpha_continuous(guide=FALSE)+ #
			geom_point(shape=21, size=4, colour=NA, alpha=.75) +
			scale_fill_gradient("Standardized\nResiduals\n(absolute val.)",low = "darkblue", high = "orangered") +
			scale_x_continuous("Observed",expand=c(0.2, 0)) + 
			scale_y_continuous("Predicted",expand=c(0.2, 0)) + 
			theme_bw() +
			theme(axis.title = element_text(face="bold"))  
	
	g	
}






#' Plots splines from an mgcv gam model (or gamm, just pass mod$gam).
#' This has less functionality than the plot.gam() function (no all.terms=T), 
#' it's just a default using ggplot with facet for visual effects. Since
#' it doesn't have an all.terms=T option, it will only plot the 
#' spline terms.  
#' 
#' @param mod gam model
#' @param residuals add residuals (default=F)
#' @param rug add rug plot at bottom of x-axis (default=F)
#' @export
splotGAMSplines <- function(mod, residuals=FALSE, rug=FALSE) {
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  
  ## Grab the data to plot, returned via plot.gam as of v1.8.5
  x=capture.output({
        png("NUL")
        plotdata <- plot.gam(mod, pages = 1, all.terms=TRUE)
        dev.off()
      })


if (residuals) {
	fv <- as.data.frame(predict(mod,type="terms")) ## get term estimates
	fv = fv %>% dplyr::select(starts_with("s(")) ## remove non-spline terms
	names(fv) = gsub("^s|[(]|[)]","", names(fv)) ## replace with variable name for later 
	prsd1 <- residuals(mod,type="working") #+ fv[,1]
}



  ## Create a long dataset for ggplot
  predDat = NULL
  resDat = NULL
  levs =c()
  
  for (spls in plotdata) {
    tdat = data.frame(x=spls$x, fit=spls$fit[,1], lower=spls$fit[,1]-spls$se, upper=spls$fit[,1]+spls$se)
    tdat$var = spls$xlab
    levs=c(levs,spls$xlab)
    if (is.null(predDat)) {
		predDat=tdat
	} else {
		predDat=rbind(predDat, tdat)
	}

	if (residuals) {
		rtdat = fv %>% dplyr::select(one_of(spls$xlab))
		names(rtdat)="resid"
		rtdat$resid = prsd1 + rtdat$resid
		rtdat$x=mod$model[,spls$xlab]
		rtdat$var = spls$xlab
		
		if (is.null(resDat)) {
			resDat = rtdat
		} else {
			resDat = rbind(resDat, rtdat)
		}
	}
  }

  predDat$var = factor(predDat$var, level=levs)

  datLng = mod$model %>% dplyr::select(one_of(levs)) %>% gather(var, x)
  datLng$fit=0  
  
  
  g= ggplot(data=predDat, aes(x=x, y=fit)) +
      geom_line() +
      geom_ribbon(aes(ymax=upper, ymin=lower), alpha=.25) +
      facet_wrap(~var, scales="free_x") +
      ylab("Partial Effect") +
      theme_bw() +
      theme(strip.text = element_text(size=10, face = "bold", colour = "black"),
          axis.title.x =element_blank(),
          axis.title.y =element_text(size=12, face = "bold", colour = "black"),
          axis.text.x= element_text(size=10, angle=45, hjust = 1),
          axis.text.y= element_text(size=10, angle=90, hjust=.5))
  
	if (rug) g = g + geom_rug(data=datLng, sides="b", size=.1, position=position_jitter(width = 0.1)) 
	if (residuals) g = g + geom_point(data=resDat, aes(x=x, y=resid), size=.01, alpha=0.25) 
	
  print(g)
}
