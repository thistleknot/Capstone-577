Input
	filterlist.txt (github)
		columns
		factor id, conversion profile, description, category flag

	Prerequisites
		PostgreSQL-10.4-1-win64-bigsql
			set pw to Read1234

		pgadmin4-4.3-x86

	Update 
		in cleandatacode.r
			sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
			
		put following files in folder
		
		share drive
		https://drive.google.com/drive/u/1/folders/1K-_ZnaQxEPVFo9iXG5w1GO6Zd_Z21Dv9
		
			34574-0001-Data.csv
			34574-0001-Data.csv
			36149-0001-Data.csv
			36407-0001-Data.csv
			36799-0001-Data.csv
			37183-0001-Data.csv
			
			or
			
			combined.csv (577 share drive)	
	
Steps
	createdb.bat
		creates database
		
	then run 
		cleandatacode.r

	what it does is two loops.  One changes the seed each pass/iteration of cv over the sample.  The second pass is the base and it resamples from the population (not from a separate training partition, everything get's rescrambled, including holdout).  

	line 237
	#static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))
	line 266
	#monte carlo resample of non holdout!
	
	line 240: 
	NewDF.holdout
	Holdout is always same static regardless of monte carlo resample.
	
	Semiotic Grid construction (confusion Matrix)
		binary classifier terms
	
	Vars
		widthDivisor
			Sets
				% of sample size in terms of holdout as well as training/validation partitions
				
				Min of 1 seed width
				min of 1 CV width
				
				Default is 3, integer's from 2 to 5 are recommended.
				CV width
				CV passes
				# of models
				Width of loops
		
Hypothesis
		at the 5% sample level using the aggregate data of the subset of the holdout.  We will derive a regression equation there.  Then we will check it against the population and see if it was within range.  If so, we will confirm our hypothesis.  As an addendum, we can derive a population level... this part I think I'd like to ask the professor about.
		
		V7118profile <- c("V7118","V7202","V7551","V8502")   
		eventually reduced to ("V7118, "V7202") (after holdout analysis)

		so... I can derive a regression equation at the holdout analysis juncture, and then see if it holds when I derive a newdataset of the population and derive it's new coefficient terms fitting a model.  See if it's terms fit within the range predicted at the holdout analysis level.  At these junctures, the datasets are aggregated independently (different sampling due to inclusion of different factors, i.e. na drops records based on what columns are included)
		
		Feature extractor will extract features that should display in random samples as significant.
		
		StepAIC
		
		vs CV
		
		different things
		
		V8517 still resulting on 0...
		
Category Flags
	Y's (0)
		V7101,1,"EVR SMK CIG,REGL",1
		V7104,1,"EVER DRINK",1
		V7112,1,"#XMJ+HS/LIFETIME",1
		V7115,1,"#X LSD/LIFETIME",1
		V7118,1,"#X PSYD/LIFETIME",1
		V7127,1,"#X AMPH/LIFETIME",1
		V7097,1,"#X SED/BARB/LIFE",1
		V7133,1,"#X TRQL/LIFETIME",1
		V7139,1,"#X NARC/LIFETIME",1
		V7142,1,"#X INHL/LIFETIME",1
		V8451,1,"#X BEER/LIFETIME",1
		V7426,1,"#X SMKLESS/EVER",1
		V7121,1,"#X CRACK/LIFETIM",1
		V7124,1,"#XOTH COKE/LIFE",1
		V7164,1,"#X MDMA/LIFETIM",1
		V7145,1,"#X STRD/LIFETIME",1
		V7109,1,"#XDRUNK/LIFETIME",1
		V7152,1,"#X H LIF USE NDL",1
		V7155,1,"#X H LIF W/O NDL",1
		V7158,1,"#X INJECTOTH/LIF",1
		V7161,1,"#X ROHYPNOL/LIFE",1
		V7601,1,"#X METHAMPH/LIFE",1
		V8480,1,"#X FLVRDALC/LIFE",1	

	X categories

		Geography (1)
			V507,0,"",0
				
		Gender (2)
			V7202,0,"R'S SEX",0

		GPA (3) (always excluded from substances cv results)
			Midpoint is B+
			V7221,2,"R HS GRADE/D=1",0	
			Median: #7: B+ 95% conf interval confirmed

		Violence (4) (always excluded from substances cv results)
			V8517,1,"FRQ GANG FIGHT",0

		Father1 (5) (always excluded from substances cv results)
			V7206,0,"R'S HSHLD FATHER",0

		Father2 (6  (always excluded from substances cv results), Median is 5, college graduate
			V7215,2,"FATHR EDUC LEVEL",0
			#5: for college grad father, 95% conf confirmed
					
		Habits (7) (always excluded from substances cv results)
			V7551,2,"#HR/W INTERNET S",0
			#4: 3-5 Hours Internet #95% conf confirmed
			
			V7552,2,"DALY WEB FACEBK",0
			#5: 6-9 Hours Facebook # 95% conf confirmed
			
			V7553,2,"#HR GAMING",0
			#4 3-5 Hours Gaming # 95% conf confirmed
			
			V7562,2,"#HR TEXT",0 
			#4 3-5 Hours Texting # 95% conf confirmed
			
			V7563,2,"#HR TALK CELL",0
			#2: <1 Hour talking on cell phone # 95% conf confirmed

		Health (8) 
			V8526,3,"OFTN EAT BRKFST",0
			V8527,3,"OFTN EAT GN VEG",0
			V8528,3,"OFTN EAT FRUIT",0
			V8529,3,"OFTN EXERCISE",0
			V8530,3,"OFTN 7HRS SLEEP",0
			V8531,3,"OFTN SLEEP <SHLD",0	

		Psychological (9)
			V8502,3,"LIFE MEANINGLESS",0
			V8505,3,"I ENJOY LIFE",0
			V8509,3,"FUTURE HOPELESS",0
			V8512,3,"SATISFD W MYSELF",0
			V8514,3,"GOOD TO BE ALIVE",0
			V8536,3,"FUTR R LIFE WRSE",0
			V7501,3,"OFTN FEEL LONELY",0
			V7507,3,"OFT WSH MOR FRND",0
			V8565,3,"I AM OFTEN BORED",0



	Milestone 1
		Identified relationships as of commit 3063ef8

	Milestone 2
		Cross validation
		Identified relationships using colsums over a subsample of entire database
		
	Milestone 3
		Implemented Cross Validation and stopped using stepAIC and opted for bestglm which allows for the smallest model to converge based on lowest cv error.  Bestglm has built in CV, which elinates the need for an external loop.
		By aggregating these terms from each subdomain up, we construct a terms list that we feed into a new bestglm formula which spits out a new list.  This list when applied to the population shows extermely significant on all terms.  We tried monte carlo with a manual CV loop but find the cross validation does all the work of comprehensively testing our sample.		

		
	Milestone 4
		Implemented cross validation, monte carlo resampling of sub sample, and resampling of holdout/non holdout data in 2 distinct loops.  I use a width factor that controls all the loops to include # of k folds and cv iterations.  Set to 2 atm.  Default will be 3.		
			
	Milestone 4.2 59eff5e
		
	Milestone 5
		Widthdiviser

	Milestone 6
		In correct formula's, but implementation of 4thpass.

		Major Functions

		Future Direction:
			Exclude >1q
			Problem is... these are limited to 5 in the subset window, would be nice to modify the bestglm.r file to include all the subsets up to k and then filter by those colsums >=median or even >= 3q
			
			Exclude min of best subset on final bout after 3rd pass?
		  
		  sub_returnCVNames <- function(data_sent){
			holderOfData <- cbind(data.frame(data_sent[,-1 , drop = FALSE]),data.frame(data_sent[,1 , drop = FALSE]))
			
			if (widthDiviser==1)  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
			if (!widthDiviser==1) B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
			
			set<-round(colSums(B$Subsets))[-1]
			
			if(!is.null(B$Subsets))
			{
			  cverrs = B$Subsets[, "CV"]
			  sdCV = B$Subsets[, "sdCV"]
			  CVLo = cverrs - sdCV
			  CVHi = cverrs + sdCV
			  ymax = max(CVHi)
			  ymin = min(CVLo)
			  k = 0:(length(cverrs) - 1)
			  if(!(ymax=="Inf" || ymax=="-Inf")) plot(k, cverrs, ylim = c(ymin, ymax), type = "n", yaxt = "n")
			  points(k,cverrs,cex = 2,col="red",pch=16)
			  lines(k, cverrs, col = "red", lwd = 2)
			  axis(2, yaxp = c(0.6, 1.8, 6))
			  segments(k, CVLo, k, CVHi,col="blue", lwd = 2)
			  eps = 0.15
			  segments(k-eps, CVLo, k+eps, CVLo, col = "blue", lwd = 2)
			  segments(k-eps, CVHi, k+eps, CVHi, col = "blue", lwd = 2)
			  indMin = which.min(cverrs)
			  fmin = sdCV[indMin]
			  cutOff = fmin + cverrs[indMin]
			  abline(h = cutOff, lty = 2)
			  indMin = which.min(cverrs)
			  fmin = sdCV[indMin]
			  cutOff = fmin + cverrs[indMin]
			  min(which(cverrs < cutOff))
			}
		   
			left=length(set)-3
			result <- set[1:left]
			
			#aboveMedianCV <- as.character(rownames(data.frame(which(result >= median(result)))))
			return(as.character(rownames(data.frame(which(result >= median(result))))))
		  }  		
	
	Milestone 7
		Median based tabulaton of cross validated results which are back projected onto population show always significant

		2c75bc9	
		
		widthSize 1
		
		7221

		
			  if(length(extract)>1)
				for (i in 1:length(extract))
				{
				  finalList <- rbind(finalList,extract[i])
				}
			  if(length(extract)==1)
			  {
				finalList <- rbind(finalList,extract)
			  }          
			  
			  print(c("2a: ", table(finalList)))
			  
			  finalListReduced <- c()
			  tabled <- table(finalList[,,drop=FALSE])
			  print(tabled)
			  if(length(tabled)==1) finalListReduced <- row.names(data.frame(tabled[tabled >= quantile(tabled)[3]]))
			  if(!length(tabled)==1) finalListReduced <- c(as.character(data.frame(table(finalList)[table(finalList) >= quantile(table(finalList))[3]])[,1]))
			  
			  print(c("3: ", finalListReduced))
			  hist((data.frame(table(finalList)))[,2])

			 >= median approach for colsums makes no sense
			 
		Milestone 8
			using numRuns >= 50% as bar for inclusion of cv results
			Combing categories of single factors into their own group
			removed a resampling that was redundent ensuring proper holdout was being performed
			
			V7118 and dependent var V8565, in one model it's almost .05 significant, in another it's like .8.  difference being widthSize, perfect example of when a var is significant in one model but not another.  Most likely due to the info is already included in other vars and would be a prime candidate for PCA
				
			incorrect >= 50%...

	Milestone 8.1
	
	219e503
	
		Implemented correct >= 25% as V8517 would not hit this threshold (33% across 4 variables)
		
		Seed appendix for widthSize 1, 2, 3, 5
		
		note widthSize starts excluding at the 25% threshold and excludes the entire v7118 series!
		
		I think it's related to the the holdoutreset loop.  The patterns are being captured on a specific loop, or are present in that % of the population that is captured on that loop.  I'm not overly concerned other than to include it.
		
		reseed bug
		
	Mmilestone 8.2
		
	Milestone 9 90395b6
		
		data.train and data.test were not being utilized correctly.  Basically there was no filtering taking place at the second juncture.
		
	Milestone 9.1
		same algorithm, but with data.train and data.test doing na's during there needed times only.
		
		0b166be
		
	Milestone 10
		Implemented widthSize iterations as well as ability to filter by >= or > median
		
		>= Median
		10
			
				
			[1] "seed:  5"
			[1] "Y: V7221"

				V7101 V7202 V7206 V7215 V7552 V7553 V7563 V8505 V8509 V8512	V8514 V8526 V8527 V8528 V8529
				0.02  0.54  0.29  0.23  0.01  0.01  0.01  0.01  0.01  0.07	0.03  0.05  0.01  0.01  0.02
				[1] "3: "   "V7202" "V7206" "V7215"
				[1] "population"

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.5315  -1.3172   0.8606   1.0437   1.8077

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -1.41675    0.02778  -51.00   <2e-16 ***
				V7202        1.12847    0.02847   39.63   <2e-16 ***
				V7206        0.61111    0.01546   39.53   <2e-16 ***
				V7215        0.48966    0.01361   35.25   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 130165  on 95593  degrees of freedom
				Residual deviance: 123770  on 95590  degrees of freedom
				AIC: 12361

				Number of Fisher Scoring iterations: 4

			[1] "seed:  5"
			[1] "Y: V8517"

				V7202 V7552 V7563 V8514 V8526 V8527 V8528 V8529 V8530 V8531 V8536 V8565 
				 0.03  0.01  0.01  0.01  0.10  0.06  0.44  0.19  0.29  0.49  0.02  0.52 
				[1] "3: "   "V8528" "V8529" "V8530" "V8531" "V8565"
				[1] "population"

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-4.8221  -0.1079  -0.1079  -0.1079   3.2093  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -5.14391    0.03581 -143.64   <2e-16 ***
				V8528        3.19977    0.08804   36.34   <2e-16 ***
				V8529        2.53552    0.09235   27.46   <2e-16 ***
				V8530        2.83001    0.09317   30.38   <2e-16 ***
				V8531        3.66254    0.10393   35.24   <2e-16 ***
				V8565        4.54251    0.08529   53.26   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 12349  on 137827  degrees of freedom
				AIC: 12361

				Number of Fisher Scoring iterations: 8

			[1] "seed:  5"
			[1] "Y: V7118"	

				V7202 V7206 V7215 V7221 V7501 V7507 V7551 V7552 V7553 V7562 V7563 V8502 V8505 V8509 V8512 V8517 V8526 V8527 V8528 V8529 V8530 V8531 V8536 V8565 
				 0.79  0.08  0.04  0.02  0.05  0.01  0.01  0.02  0.01  0.09  0.06  0.03  0.02  0.03  0.03  0.02  0.03  0.01  0.01  0.02  0.03  0.05  0.01  0.02 

				[1] "3: "   "V7202"
				[1] "population"

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.2660  -1.2660  -0.4456   1.0913   2.1722  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -2.25995    0.07116  -31.76   <2e-16 ***
				V7202        2.46591    0.07839   31.46   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 8016.4  on 6048  degrees of freedom
				Residual deviance: 6591.4  on 6047  degrees of freedom
				AIC: 6595.4

				Number of Fisher Scoring iterations: 4		

		7
			V7221
				V7101      V7202      V7206      V7215      V7551      V7562      V7563      V8502      V8509      V8512      V8514      V8526      V8527      V8528 
				0.02040816 0.69387755 0.36734694 0.26530612 0.02040816 0.02040816 0.04081633 0.08163265 0.04081633 0.08163265 0.04081633 0.08163265 0.06122449 0.02040816 
					 V8529 
				0.02040816 
				[1] "3: "   "V7202" "V7206" "V7215"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.5315  -1.3172   0.8606   1.0437   1.8077  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -1.41675    0.02778  -51.00   <2e-16 ***
				V7202        1.12847    0.02847   39.63   <2e-16 ***
				V7206        0.61111    0.01546   39.53   <2e-16 ***
				V7215        0.47966    0.01361   35.25   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 130165  on 95593  degrees of freedom
				Residual deviance: 123778  on 95590  degrees of freedom
				AIC: 123786

				Number of Fisher Scoring iterations: 4
				
			V8517
				 V7202      V8505      V8514      V8526      V8527      V8528      V8529      V8530      V8531      V8536      V8565 
				0.02040816 0.02040816 0.02040816 0.06122449 0.08163265 0.53061224 0.14285714 0.32653061 0.40816327 0.02040816 0.77551020 
				[1] "3: "   "V8528" "V8530" "V8531" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-4.8529  -0.1132  -0.1132  -0.1132   3.1792  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -5.04736    0.03403 -148.34   <2e-16 ***
				V8528        4.14358    0.08639   47.97   <2e-16 ***
				V8530        3.79488    0.09350   40.59   <2e-16 ***
				V8531        4.28039    0.10532   40.64   <2e-16 ***
				V8565        4.60376    0.08415   54.71   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 13087  on 137828  degrees of freedom
				AIC: 13097

				Number of Fisher Scoring iterations: 8	
				
			V7118

				[1] "3: "   "V7202"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.2660  -1.2660  -0.4456   1.0913   2.1722  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -2.25995    0.07116  -31.76   <2e-16 ***
				V7202        2.46591    0.07839   31.46   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 8016.4  on 6048  degrees of freedom
				Residual deviance: 6591.4  on 6047  degrees of freedom
				AIC: 6595.4

				Number of Fisher Scoring iterations: 4				
				
		5
		
			V7221
				V7202 V7206 V7215 V7562 V7563 V8502 V8512 V8526 V8527 
				 0.68  0.08  0.40  0.04  0.04  0.04  0.20  0.04  0.08 
				[1] "3: "   "V7202" "V7215"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.4654  -1.2486   0.9145   0.9145   1.7215  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -1.22407    0.02689  -45.52   <2e-16 ***
				V7202        1.38979    0.02748   50.57   <2e-16 ***
				V7215        0.48985    0.01350   36.28   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 130165  on 95593  degrees of freedom
				Residual deviance: 125336  on 95591  degrees of freedom
				AIC: 125342

				Number of Fisher Scoring iterations: 4

			V8517
				V8526 V8528 V8529 V8530 V8531 V8536 V8565 
				 0.08  0.60  0.24  0.16  0.52  0.04  0.60 
				[1] "3: "   "V8528" "V8529" "V8531" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-4.7495  -0.1148  -0.1148  -0.1148   3.1701  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -5.01831    0.03362 -149.28   <2e-16 ***
				V8528        4.26634    0.08543   49.94   <2e-16 ***
				V8529        3.60323    0.09474   38.03   <2e-16 ***
				V8531        3.69923    0.11385   32.49   <2e-16 ***
				V8565        4.72832    0.08141   58.08   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 13282  on 137828  degrees of freedom
				AIC: 13292

				Number of Fisher Scoring iterations: 8
				
			V7118
				V7202 V7206 V7221 V7501 V7551 V7552 V7563 V8502 V8512 V8514 V8517 V8527 V8531 V8536 V8565 
				 0.76  0.08  0.04  0.04  0.04  0.04  0.08  0.08  0.08  0.04  0.04  0.08  0.12  0.04  0.04 
				[1] "3: "   "V7202"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.2660  -1.2660  -0.4456   1.0913   2.1722  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -2.25995    0.07116  -31.76   <2e-16 ***
				V7202        2.46591    0.07839   31.46   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 8016.4  on 6048  degrees of freedom
				Residual deviance: 6591.4  on 6047  degrees of freedom
				AIC: 6595.4

				Number of Fisher Scoring iterations: 4		
				
		3
		
			V7221

				V7202     V7206     V7215     V7563     V8512     V8514     V8530 
				0.5555556 0.1111111 0.3333333 0.1111111 0.1111111 0.1111111 0.1111111 
				[1] "3: "   "V7202"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
				   Min      1Q  Median      3Q     Max  
				-1.365  -1.365   1.001   1.001   1.650  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -1.06556    0.02634  -40.45   <2e-16 ***
				V7202        1.49615    0.02723   54.95   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 130165  on 95593  degrees of freedom
				Residual deviance: 126657  on 95592  degrees of freedom
				AIC: 126661

				Number of Fisher Scoring iterations: 4

			V8517

				V8528     V8529     V8530     V8531     V8565 
				0.3333333 0.1111111 0.5555556 0.4444444 0.7777778 
				[1] "3: "   "V8530" "V8531" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-5.0223  -0.1218  -0.1218  -0.1218   3.1327  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -4.89963    0.03233 -151.57   <2e-16 ***
				V8530        6.28307    0.06644   94.56   <2e-16 ***
				V8531        6.07075    0.08175   74.26   <2e-16 ***
				V8565        5.15773    0.07476   68.99   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 15475  on 137829  degrees of freedom
				AIC: 15483

				Number of Fisher Scoring iterations: 7

			V7118

					V7202     V7215     V7221     V7562     V7563     V8514     V8528 
					0.7777778 0.1111111 0.1111111 0.1111111 0.1111111 0.1111111 0.1111111 
					[1] "3: "   "V7202"
					[1] "population"

					Call:
					NULL

					Deviance Residuals: 
						Min       1Q   Median       3Q      Max  
					-1.2660  -1.2660  -0.4456   1.0913   2.1722  

					Coefficients:
								Estimate Std. Error z value Pr(>|z|)    
					(Intercept) -2.25995    0.07116  -31.76   <2e-16 ***
					V7202        2.46591    0.07839   31.46   <2e-16 ***
					---
					Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

					(Dispersion parameter for binomial family taken to be 1)

						Null deviance: 8016.4  on 6048  degrees of freedom
					Residual deviance: 6591.4  on 6047  degrees of freedom
					AIC: 6595.4

					Number of Fisher Scoring iterations: 4
					
	> Median
		[1] "Y: V7221"
		
			V7563 V8514 
			 1.00  0.72 
			[1] "3: "   "V7563" "V8514"
			[1] "population"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.0399  -0.9521  -0.9521   1.3214   1.4258  

			Coefficients:
						 Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.556153   0.007059 -78.787   <2e-16 ***
			V7563       -0.011012   0.013561  -0.812    0.417    
			V8514        0.223779   0.010709  20.896   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 242609  on 182337  degrees of freedom
			Residual deviance: 242101  on 182335  degrees of freedom
			AIC: 242107

			Number of Fisher Scoring iterations: 4


		V8517
			V7563 V8514 
			 1.00  0.71 
			[1] "3: "   "V7563" "V8514"
			[1] "population"

			Call:
			NULL

			Deviance Residuals: 
				 Min        1Q    Median        3Q       Max  
			-0.67580  -0.24683  -0.24683  -0.00008   2.64819  

			Coefficients:
						 Estimate Std. Error  z value Pr(>|z|)    
			(Intercept)  -3.47599    0.02107 -165.002   <2e-16 ***
			V7563       -16.09008   59.83971   -0.269    0.788    
			V8514         2.11546    0.02574   82.174   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 61417  on 137832  degrees of freedom
			Residual deviance: 49321  on 137830  degrees of freedom
			AIC: 49327

			Number of Fisher Scoring iterations: 18

		V7118
			
			V7563 V8514 
			 1.00  0.65 
			[1] "3: "   "V7563" "V8514"
			[1] "population"

			Call:
			NULL

			Deviance Residuals: 
			   Min      1Q  Median      3Q     Max  
			-1.395  -0.976  -0.976   1.157   1.393  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.49408    0.02541  -19.44   <2e-16 ***
			V7563        0.99286    0.05470   18.15   <2e-16 ***
			V8514        0.54243    0.04645   11.68   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 15198  on 11039  degrees of freedom
			Residual deviance: 14804  on 11037  degrees of freedom
			AIC: 14810

			Number of Fisher Scoring iterations: 4
			
	7
		[1] "Y: V7221"	
		   V7563    V8514 
			1.000000 0.755102 
			[1] "3: "   "V7563" "V8514"
			[1] "population"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.0399  -0.9521  -0.9521   1.3214   1.4258  

			Coefficients:
						 Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.556153   0.007059 -78.787   <2e-16 ***
			V7563       -0.011012   0.013561  -0.812    0.417    
			V8514        0.223779   0.010709  20.896   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 242609  on 182337  degrees of freedom
			Residual deviance: 242101  on 182335  degrees of freedom
			AIC: 242107

			Number of Fisher Scoring iterations: 4

		V8517
				
			V7563     V8514 
			1.0000000 0.6734694 
			[1] "3: "   "V7563" "V8514"
			[1] "population"

			Call:
			NULL

			Deviance Residuals: 
				 Min        1Q    Median        3Q       Max  
			-0.67580  -0.24683  -0.24683  -0.00008   2.64819  

			Coefficients:
						 Estimate Std. Error  z value Pr(>|z|)    
			(Intercept)  -3.47599    0.02107 -165.002   <2e-16 ***
			V7563       -16.09008   59.83971   -0.269    0.788    
			V8514         2.11546    0.02574   82.174   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 61417  on 137832  degrees of freedom
			Residual deviance: 49321  on 137830  degrees of freedom
			AIC: 49327

			Number of Fisher Scoring iterations: 18

		V7118		
			V7563     V8514 
			1.0000000 0.7346939 
			[1] "3: "   "V7563" "V8514"
			[1] "population"

			Call:
			NULL

			Deviance Residuals: 
			   Min      1Q  Median      3Q     Max  
			-1.395  -0.976  -0.976   1.157   1.393  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.49408    0.02541  -19.44   <2e-16 ***
			V7563        0.99286    0.05470   18.15   <2e-16 ***
			V8514        0.54243    0.04645   11.68   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 15198  on 11039  degrees of freedom
			Residual deviance: 14804  on 11037  degrees of freedom
			AIC: 14810

			Number of Fisher Scoring iterations: 4
			
					
			
V7221 R HS GRADE/D=1
	V7202 R'S SEX
	V7206 R'S HSHLD FATHER
	
V8517 Gang Fight
	V8530 OFTN 7HRS SLEEP
	V8565 I AM OFTEN BORED
	
V7118 #X PSYD/LIFETIME
	V7202 R'S SEX

Percent formula  
	To get more dispersion, increase holdoutreset loop to say 100 and set resample loop max to 1.  This will be somewhat expensive but will force a reshuffle of the dataset each pass.  As is a CV of 10 simulates this effectively enough without being as expensive.
	
	Imagine the holdout is you shuffle the cards (~reseed & holdout sample) and then 
		take a set of cards away from a deck (macro: holdoutset & nonholdoutset)
		and resample is you take a subsample from this set (micro: pretrain=data.train & holdout=data.text)
		
		Due to this sampling, variations occur during 2a tablization within each holdoutSet's iteration
	
	The ^3 is for reseed, holdout reset, remonte carlo.
	
		Normally, one only needs to do one seed, therefore reducing to ^2	
		
	The +3x is for post evaluation and is used for evaluating the model and if the experiement holds (i.e. always significant population terms), then this part can be viewed as not counted towards the overall cost.
	
	Therefore widthDiviser of 5+ can be accomodated for.  With a max of 10, for ^2 with a 10x10 = 100%, which would result in 100 passes and would obviously monte carlo simulate 100%, but not quite converge.
	
  (x^3)+(3x)
  only the mc set is what costs anything (pre non holdout size) , not the resampling from this set (i.e. pretrain)
  
  the ^3 is the final pruning of the set of factors through a new double wide merged partition set
  then a subsequent one where a fit is derived to confirm significance
  then a 3rd where measures of accuracy are performed  

Cross validation means that if the patterns show up more than half the time (5 out of 10 passes), they are probably truly significant.  I usually have a higher bar, such as the top quartile, or top 10-20%.
	These are not just cross validated against training sets 10 times.  
	To report a positive number.  The significant factors found during the training partition are applied to the validation partition, and what remains as significant are reported.

Inferences
	Factors that were included above the median # of times... (see factorAnalysiswFactors.png)

	#HR GAMING
	#HR TEXT 
	#HR TALK CELL
	OFTN EAT FRUIT
	OFTN EXERCISE
	OFTN 7HRS SLEEP
	OFTN SLEEP <SHLD
	SATISFD W MYSELF
	GOOD TO BE ALIVE
	I AM OFTEN BORED

Chosen Research Questions
V7118: #X PSYD/LIFETIME
	V507W: W (7)
	V7553: #HR GAMING (7)
	V7562: #HR TEXT (6)
	V8528: OFTN EAT FRUIT (5)
	V8529: OFTN EXERCISE (6)
	V8530: OFTN 7HRS SLEEP (8)
	V8512: SATISFD W MYSELF (5)
	V8514: GOOD TO BE ALIVE (5)
	V8565: I AM OFTEN BORED (5)
	
Note
	V8517
	set preTrain to .25
	
	If sample size is too small and nrFolds value is set too high... will get errors when there is no values in data[-folds], possibly due to na's? mis conversion?

	folds <- rep_len(1:nrFolds, nrow(data))

	
V8517: FRQ GANG FIGHT
	V7553: #HR GAMING (6)
	V7562: #HR TEXT (5)
	V7563: #HR TALK CELL (5)
	V7501: OFTN FEEL LONELY (5)
	V7507: OFT WSH MOR FRND (6)

V7221: R HS GRADE/D=1
	V7553: #HR GAMING (6)
	V7562: #HR TEXT (6)
	V8530: OFTN 7HRS SLEEP (6)
	V8531: OFTN SLEEP <SHLD (9)
	V7501: OFTN FEEL LONELY (7)
	V8565: I AM OFTEN BORED (7)	
	
Citations

	http://www.et.bs.ehu.es/cran/web/packages/bestglm/vignettes/bestglm.pdf
	
	CV (2009?)
	
	Measures of central tendency (Median)
	
	Resampling / simulation methods: monte carlo, bootstrapping, jackknifing, cross-validation, randomization tests, and permutation tests
	
		https://stats.stackexchange.com/questions/104040/resampling-simulation-methods-monte-carlo-bootstrapping-jackknifing-cross
	
	Median confidence interval
	
		https://www.ucl.ac.uk/child-health/short-courses-events/about-statistical-courses/research-methods-and-statistics/chapter-8-content-8
	
	Why more predictor terms are necessary for full models as well as nested f tests and large n's
		https://stats.stackexchange.com/questions/77245/comparing-nested-binary-logistic-regression-models-when-n-is-large
	
		uses lrm package
		
		"here is an extensive literature on why one should prefer full models to restricted/parsimonious models. My understanding are few reasons to prefer the parsimonious model. However, larger models may not be feasible for many clinical applications."
		
	Durbin Watson test
	
	Data Mining for Busines Analytics

		Classification Matrix
			p 118
				Find propensity, derive new classification matrix at propensity that gives best class of interest rate (i.e. 1).
		
		ROC
			p 115, 120-121			
				goal is a model with a high sensitivity
				
		
		Lift charts
			p 125-126			
		
				case can be assigned to the class with the highest probability.  In many cases, a single class is of special interest, so we will focus on that particular lcass and compare the propensity of belonging to that class to ac utoff value set by the analyst.... If hte probability of belonging to the lcass of interest is above the cutoff, the case is assigned to that class.

				i.e. if holdout/test value is above a preset threshold (based on prior model result propensity threshold set to achieve optimal class specification) assign 1, else 0.		
			
		Decile charts
			p 127			
			
		Oversampling (nice to have)
			p 127-134		
	
	Applied Regression Modelling
		104-109
			nested model tests (anova) similar to t-tests. basically testing if there is a need to include the coefficient term
			we don't need to do this
	
		p 150
			natural log for response (using a quantitative example)
			
		p 206
			check collinearity
			cross validation kind of takes care of this
			if any pair of x's correlation is greater than the highest predictor and y, then it is too highly correlated, drop one var.
		
		Page 268-273 binary logistic regression
			
			p 270
				logit (y) to get probability
				
				increase in 1 (binary classifier) means that logit will increase by coefficient size, odds will be multiplied by exp(coefficient).
				Odds ratio
				
			nested (anaysis of variance).
				Is variance reduced?
				nested compares rss
				nested model test for GLM's compares the values of an analogous quantity
					the residual deviance, for two nested models.
					The resulting test is then called an "Analysis of deviance test or sometimes called a "likelihood ratio" test.
					
					this is the chi-squared value using a df equal to the # of factors being removed.
					Derive residual deviance for each model
					
					All this is reported in R while outputting the glm model as "Residual deviance"
					https://www.theanalysisfactor.com/r-glm-model-fit/
					
					https://iainpardoe.com/arm2e/spss/
					Computer help #8
					
						"To find a percentile (critical value) for a chi-squared distribution, select Transform > Compute Variable. Type a name (with no spaces) in the Target Variable box (e.g., "cvchisq"). Then type IDF.CHISQ(p, df) into the Numeric Expression box. Here p is the lower-tail area (i.e., one minus the significance level) and df is the degrees of freedom. For example, IDF.CHISQ(0.95, 2) returns the 95th percentile of the chi-squared distribution with 2 degrees of freedom (5.991)."
					
				Since models are 0's and 1's, can do a factorial grid of propensities and map them accordingly from highest to lowest and get demographics?
				
				There would literally be this many combinations based on # of factors, the biggest algorithm I have is 5 factors, so that is 16 distinct probabilities.
				1,2,4,8,16,32
		
	Predictor Plots
		141, 258, 266
			could map one class with different effects of other vars
		Would have to map logit propensity changes for changes of each variable assuming other variables were held constant.
		Normally assumes the mean for other variables...  I suppose in this case, the median? Should really have all propensities mapped.
			graphics: 224-228
		
	Odds calculation wikipedia
		https://en.wikipedia.org/wiki/Logistic_regression
		
		One additional hour of study is estimated to increase log-odds of passing by 1.5046, so multiplying odds of passing by {\displaystyle \exp(1.5046)\approx 4.5.} {\displaystyle \exp(1.5046)\approx 4.5.} The form with the x-intercept (2.71) shows that this estimates even odds (log-odds 0, odds 1, probability 1/2) for a student who studies 2.71 hours.
	
	
	
Appendix
	