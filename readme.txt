Input
	filterlist.txt (github)
		columns
		factor id, description, conversion profile, category flag

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
	
V7221 R HS GRADE/D=1
	V7202 R'S SEX
	V7206 R'S HSHLD FATHER
	
V8517 Gang Fight
	V8530 OFTN 7HRS SLEEP
	V8565 I AM OFTEN BORED
	
V7118 #X PSYD/LIFETIME
	V7202 R'S SEX


Percent formula  
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
	
Appendix
		widthSize 1
			
			[1] "seed:  5"
			[1] "Y: V7221"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7553" "V7562" "V8526" "V8527" "V8531" "V8505" "V8509" "V8512" "V8514" "V8565"
				[1] "2: "   "V8512"
						V8512 
				"2a: "    "1" 
				[1] "loop:  2 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7553" "V7562" "V8526" "V8527" "V8531" "V8505" "V8509" "V8512" "V8514" "V8565"
				[1] "2: "   "V8512"
						V8512 
				"2a: "    "1" 
				[1] "seed:  6"
				[1] "loop:  3 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7206" "V7101" "V7551" "V7552" "V8527" "V8529" "V8530" "V8505" "V8509" "V8512" "V8514" "V8536" "V7501" "V8565"
				[1] "2: "   "V8502" "V8512" "V8565"
												  V8502               V8512               V8565 
							 "2a: " "0.333333333333333"                 "1" "0.333333333333333" 
				[1] "loop:  4 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7206" "V7101" "V7551" "V7552" "V8527" "V8529" "V8530" "V8505" "V8509" "V8512" "V8514" "V8536" "V7501" "V8565"
				[1] "2: "   "V8502" "V8512" "V8565"
						V8502  V8512  V8565 
				"2a: "  "0.5"    "1"  "0.5" 

				V8502 V8512 V8565 
				  0.5   1.0   0.5 
				[1] "3: "   "V8502" "V8512" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.3885  -1.2187   0.9801   1.1366   1.3197  

				Coefficients:
							 Estimate Std. Error z value Pr(>|z|)    
				(Intercept)  0.096696   0.005807  16.653   <2e-16 ***
				V8502       -0.286123   0.017054 -16.778   <2e-16 ***
				V8512        0.386916   0.010777  35.901   <2e-16 ***
				V8565       -0.139007   0.015414  -9.019   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 251507  on 182337  degrees of freedom
				Residual deviance: 249857  on 182334  degrees of freedom
				AIC: 249865

				Number of Fisher Scoring iterations: 4

			[1] "seed:  5"
			[1] "Y: V8517"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7206" "V7551" "V7552" "V7563" "V8526" "V8527" "V8528" "V8530" "V8512" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  2 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7206" "V7551" "V7552" "V7563" "V8526" "V8527" "V8528" "V8530" "V8512" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "seed:  6"
				[1] "loop:  3 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V7101" "V8527" "V8528" "V8531" "V8502" "V8505" "V8514" "V8536" "V8565"
				[1] "2: "   "V8514" "V8565"
												  V8514               V8565 
							 "2a: " "0.333333333333333"                 "1" 
				[1] "loop:  4 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V7101" "V8527" "V8528" "V8531" "V8502" "V8505" "V8514" "V8536" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "  "0.5"    "1" 

				V8514 V8565 
				  0.5   1.0 
				[1] "3: "   "V8514" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-2.6590  -0.1585  -0.1585  -0.1585   2.9610  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -4.37123    0.02676 -163.32   <2e-16 ***
				V8514        2.32668    0.03265   71.27   <2e-16 ***
				V8565        5.55016    0.06025   92.11   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 34637  on 137830  degrees of freedom
				AIC: 34643

				Number of Fisher Scoring iterations: 7

			[1] "seed:  5"
			[1] "Y: V7118"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7551" "V7552" "V7553" "V8527" "V8528" "V8530" "V8502" "V8505" "V8512" "V7501" "V7507"
				[1] "2: "   "V8502" "V8509" "V8536" "V7501" "V8565"
						V7501  V8502  V8509  V8536  V8565 
				"2a: "    "1"    "1"    "1"    "1"    "1" 
				[1] "loop:  2 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7551" "V7552" "V7553" "V8527" "V8528" "V8530" "V8502" "V8505" "V8512" "V7501" "V7507"
				[1] "2: "   "V8502" "V8509" "V8536" "V7501" "V8565"
						V7501  V8502  V8509  V8536  V8565 
				"2a: "    "1"    "1"    "1"    "1"    "1" 
				[1] "seed:  6"
				[1] "loop:  3 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7206" "V7551" "V7553" "V7562" "V8526" "V8529" "V8531" "V8502" "V8509" "V8512" "V8536" "V7501"
				[1] "2: "   "V8505"
												  V7501               V8502               V8505               V8509               V8536               V8565 
							 "2a: " "0.666666666666667" "0.666666666666667" "0.333333333333333" "0.666666666666667" "0.666666666666667" "0.666666666666667" 
				[1] "loop:  4 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7206" "V7551" "V7553" "V7562" "V8526" "V8529" "V8531" "V8502" "V8509" "V8512" "V8536" "V7501"
				[1] "2: "   "V8505"
						V7501  V8502  V8505  V8509  V8536  V8565 
				"2a: "  "0.5"  "0.5"  "0.5"  "0.5"  "0.5"  "0.5" 

				V7501 V8502 V8505 V8509 V8536 V8565 
				  0.5   0.5   0.5   0.5   0.5   0.5 
				[1] "3: "   "V7501" "V8502" "V8505" "V8509" "V8536" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
				   Min      1Q  Median      3Q     Max  
				-1.914  -1.000  -1.000   1.366   1.368  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -0.43235    0.02367 -18.265  < 2e-16 ***
				V7501        1.09073    0.09993  10.914  < 2e-16 ***
				V8502        0.62403    0.07778   8.023 1.03e-15 ***
				V8505        0.22701    0.05014   4.528 5.96e-06 ***
				V8509        0.54849    0.08927   6.144 8.04e-10 ***
				V8536        0.68912    0.15571   4.426 9.62e-06 ***
				V8565       -0.00637    0.07085  -0.090    0.928    
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 15198  on 11039  degrees of freedom
				Residual deviance: 14725  on 11033  degrees of freedom
				AIC: 14739

				Number of Fisher Scoring iterations: 4

				There were 50 or more warnings (use warnings() to see the first 50)
				> 
				> source(paste0(sourceDir,"4thpass.R"))
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
				   Min      1Q  Median      3Q     Max  
				-1.914  -1.000  -1.000   1.366   1.368  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -0.43235    0.02367 -18.265  < 2e-16 ***
				V7501        1.09073    0.09993  10.914  < 2e-16 ***
				V8502        0.62403    0.07778   8.023 1.03e-15 ***
				V8505        0.22701    0.05014   4.528 5.96e-06 ***
				V8509        0.54849    0.08927   6.144 8.04e-10 ***
				V8536        0.68912    0.15571   4.426 9.62e-06 ***
				V8565       -0.00637    0.07085  -0.090    0.928    
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 15198  on 11039  degrees of freedom
				Residual deviance: 14725  on 11033  degrees of freedom
				AIC: 14739

				Number of Fisher Scoring iterations: 4
				
		widthSize 2

			[1] "seed:  5"
			[1] "Y: V7221"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7551" "V7562" "V8526" "V8527" "V8529" "V8502" "V8509" "V8512" "V8536" "V7507"
				[1] "2: "   "V8509" "V8536"
						V8509  V8536 
				"2a: "    "1"    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7552" "V7553" "V7563" "V8526" "V8527" "V8528" "V8530" "V8502" "V8509" "V8512" "V8536" "V7501"
				[1] "2: "   "V8502"
						V8502  V8509  V8536 
				"2a: "  "0.5"  "0.5"  "0.5" 
				[1] "loop:  3 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7551" "V7562" "V8526" "V8527" "V8529" "V8502" "V8509" "V8512" "V8536" "V7507"
				[1] "2: "   "V8509" "V8536"
												  V8502               V8509               V8536 
							 "2a: " "0.333333333333333" "0.666666666666667" "0.666666666666667" 
				[1] "loop:  4 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7552" "V7553" "V7563" "V8526" "V8527" "V8528" "V8530" "V8502" "V8509" "V8512" "V8536" "V7501"
				[1] "2: "   "V8502"
						V8502  V8509  V8536 
				"2a: "  "0.5"  "0.5"  "0.5" 
				[1] "seed:  6"
				[1] "loop:  5 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7206" "V7551" "V7552" "V7563" "V8526" "V8527" "V8530" "V8502" "V8509" "V8512" "V8536" "V7501"
				[1] "2: "   "V8505"
						V8502  V8505  V8509  V8536 
				"2a: "  "0.4"  "0.2"  "0.4"  "0.4" 
				[1] "loop:  6 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7206" "V7215" "V7101" "V7552" "V7562" "V8526" "V8527" "V8528" "V8502" "V8514" "V8536" "V7501" "V7507"
				[1] "2: "   "V8505" "V8509" "V8536"
												  V8502               V8505               V8509               V8536 
							 "2a: " "0.333333333333333" "0.333333333333333"               "0.5"               "0.5" 
				[1] "loop:  7 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7206" "V7551" "V7552" "V7563" "V8526" "V8527" "V8530" "V8502" "V8509" "V8512" "V8536" "V7501"
				[1] "2: "   "V8505"
												  V8502               V8505               V8509               V8536 
							 "2a: " "0.285714285714286" "0.428571428571429" "0.428571428571429" "0.428571428571429" 
				[1] "loop:  8 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7206" "V7215" "V7101" "V7552" "V7562" "V8526" "V8527" "V8528" "V8502" "V8514" "V8536" "V7501" "V7507"
				[1] "2: "   "V8505" "V8509" "V8536"
						V8502  V8505  V8509  V8536 
				"2a: " "0.25"  "0.5"  "0.5"  "0.5" 

				V8502 V8505 V8509 V8536 
				 0.25  0.50  0.50  0.50 
				[1] "3: "   "V8502" "V8505" "V8509" "V8536"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
				   Min      1Q  Median      3Q     Max  
				-1.389  -1.226   1.012   1.130   1.335  

				Coefficients:
							 Estimate Std. Error z value Pr(>|z|)    
				(Intercept)  0.113683   0.005856  19.413   <2e-16 ***
				V8502       -0.197958   0.019498 -10.153   <2e-16 ***
				V8505        0.289244   0.010227  28.282   <2e-16 ***
				V8509       -0.279418   0.022373 -12.489   <2e-16 ***
				V8536        0.082517   0.041659   1.981   0.0476 *  
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 251507  on 182337  degrees of freedom
				Residual deviance: 250221  on 182333  degrees of freedom
				AIC: 250231

				Number of Fisher Scoring iterations: 4

			[1] "seed:  5"
			[1] "Y: V8517"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V8527" "V8528" "V8529" "V8530" "V8531" "V8502" "V8505" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7221" "V7206" "V7552" "V7553" "V7563" "V8527" "V8529" "V8530" "V8531" "V8502" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8505" "V8565"
						V8505  V8565 
				"2a: "  "0.5"    "1" 
				[1] "loop:  3 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V8527" "V8528" "V8529" "V8530" "V8531" "V8502" "V8505" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8505               V8565 
							 "2a: " "0.333333333333333"                 "1" 
				[1] "loop:  4 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7221" "V7206" "V7552" "V7553" "V7563" "V8527" "V8529" "V8530" "V8531" "V8502" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8505" "V8565"
						V8505  V8565 
				"2a: "  "0.5"    "1" 
				[1] "seed:  6"
				[1] "loop:  5 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7215" "V7551" "V7552" "V7563" "V8526" "V8527" "V8529" "V8531" "V8514" "V8536" "V7501" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8505  V8514  V8565 
				"2a: "  "0.4"  "0.2"    "1" 
				[1] "loop:  6 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7221" "V7215" "V7551" "V7552" "V7563" "V7101" "V8526" "V8527" "V8529" "V8502" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502" "V8514" "V8565"
												  V8502               V8505               V8514               V8565 
							 "2a: " "0.166666666666667" "0.333333333333333" "0.333333333333333"                 "1" 
				[1] "loop:  7 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7215" "V7551" "V7552" "V7563" "V8526" "V8527" "V8529" "V8531" "V8514" "V8536" "V7501" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
												  V8502               V8505               V8514               V8565 
							 "2a: " "0.142857142857143" "0.285714285714286" "0.428571428571429"                 "1" 
				[1] "loop:  8 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7221" "V7215" "V7551" "V7552" "V7563" "V7101" "V8526" "V8527" "V8529" "V8502" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502" "V8514" "V8565"
						V8502  V8505  V8514  V8565 
				"2a: " "0.25" "0.25"  "0.5"    "1" 

				V8502 V8505 V8514 V8565 
				 0.25  0.25  0.50  1.00 
				[1] "3: "   "V8502" "V8505" "V8514" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
				   Min      1Q  Median      3Q     Max  
				-2.824  -0.150  -0.150  -0.150   2.998  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -4.48126    0.02779 -161.28   <2e-16 ***
				V8502        0.94170    0.04054   23.23   <2e-16 ***
				V8505        0.75607    0.05371   14.08   <2e-16 ***
				V8514        1.63005    0.05535   29.45   <2e-16 ***
				V8565        5.12109    0.06029   84.95   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 34013  on 137828  degrees of freedom
				AIC: 34023

				Number of Fisher Scoring iterations: 7

			[1] "seed:  5"
			[1] "Y: V7118"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7551" "V7553" "V7562" "V7563" "V8526" "V8530" "V8531" "V8502" "V8512" "V8514" "V8536" "V7501"
				[1] "2: "   "V8505" "V8509" "V7501"
						V7501  V8505  V8509 
				"2a: "    "1"    "1"    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7206" "V7551" "V7553" "V7562" "V8526" "V8528" "V8531" "V8505" "V8509" "V8512" "V8514" "V8536"
				[1] "2: "   "V8565"
						V7501  V8505  V8509  V8565 
				"2a: "  "0.5"  "0.5"  "0.5"  "0.5" 
				[1] "loop:  3 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7551" "V7553" "V7562" "V7563" "V8526" "V8530" "V8531" "V8502" "V8512" "V8514" "V8536" "V7501"
				[1] "2: "   "V8505" "V8509" "V7501"
												  V7501               V8505               V8509               V8565 
							 "2a: " "0.666666666666667" "0.666666666666667" "0.666666666666667" "0.333333333333333" 
				[1] "loop:  4 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7206" "V7551" "V7553" "V7562" "V8526" "V8528" "V8531" "V8505" "V8509" "V8512" "V8514" "V8536"
				[1] "2: "   "V8565"
						V7501  V8505  V8509  V8565 
				"2a: "  "0.5"  "0.5"  "0.5"  "0.5" 
				[1] "seed:  6"
				[1] "loop:  5 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8526" "V8527" "V8530" "V8502" "V8509" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502" "V8512"
						V7501  V8502  V8505  V8509  V8512  V8565 
				"2a: "  "0.4"  "0.2"  "0.4"  "0.4"  "0.2"  "0.4" 
				[1] "loop:  6 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7553" "V7563" "V8529" "V8530" "V8531" "V8502" "V8505" "V8509" "V7507" "V8565"
				[1] "2: "   "V8502" "V8509"
												  V7501               V8502               V8505               V8509               V8512               V8565 
							 "2a: " "0.333333333333333" "0.333333333333333" "0.333333333333333"               "0.5" "0.166666666666667" "0.333333333333333" 
				[1] "loop:  7 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8526" "V8527" "V8530" "V8502" "V8509" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502" "V8512"
												  V7501               V8502               V8505               V8509               V8512               V8565 
							 "2a: " "0.285714285714286" "0.428571428571429" "0.285714285714286" "0.428571428571429" "0.285714285714286" "0.285714285714286" 
				[1] "loop:  8 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7553" "V7563" "V8529" "V8530" "V8531" "V8502" "V8505" "V8509" "V7507" "V8565"
				[1] "2: "   "V8502" "V8509"
						V7501  V8502  V8505  V8509  V8512  V8565 
				"2a: " "0.25"  "0.5" "0.25"  "0.5" "0.25" "0.25" 

				V7501 V8502 V8505 V8509 V8512 V8565 
				 0.25  0.50  0.25  0.50  0.25  0.25 
				[1] "3: "   "V7501" "V8502" "V8505" "V8509" "V8512" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.6613  -0.9997  -0.9997   1.3253   1.3662  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -0.43358    0.02380 -18.214  < 2e-16 ***
				V7501        1.09197    0.09997  10.923  < 2e-16 ***
				V8502        0.63832    0.07771   8.214  < 2e-16 ***
				V8505        0.09244    0.07501   1.232   0.2178    
				V8509        0.59763    0.08872   6.736 1.63e-11 ***
				V8512        0.16354    0.07619   2.146   0.0318 *  
				V8565        0.03183    0.07003   0.455   0.6494    
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 15198  on 11039  degrees of freedom
				Residual deviance: 14741  on 11033  degrees of freedom
				AIC: 14755

				Number of Fisher Scoring iterations: 4
				
		widthSize 3
				
			[1] "seed:  5"
			[1] "Y: V7221"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7563" "V8526" "V8528" "V8530" "V8502" "V8505" "V8512" "V7501" "V8565"
				[1] "2: "   "V8509" "V8512"
						V8509  V8512 
				"2a: "    "1"    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7553" "V8526" "V8530" "V8531" "V8509" "V8512" "V8514" "V7501" "V8565"
				[1] "2: "   "V8512"
						V8509  V8512 
				"2a: "  "0.5"    "1" 
				[1] "loop:  3 holdoutReset:  1 resample:  3"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7553" "V8526" "V8527" "V8531" "V8502" "V8505" "V8512" "V8514" "V8565"
				[1] "2: "   "V8509"
												  V8509               V8512 
							 "2a: " "0.666666666666667" "0.666666666666667" 
				[1] "loop:  4 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7563" "V8526" "V8528" "V8530" "V8502" "V8505" "V8512" "V7501" "V8565"
				[1] "2: "   "V8509" "V8512"
						V8509  V8512 
				"2a: " "0.75" "0.75" 
				[1] "loop:  5 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7553" "V8526" "V8530" "V8531" "V8509" "V8512" "V8514" "V7501" "V8565"
				[1] "2: "   "V8512"
						V8509  V8512 
				"2a: "  "0.6"  "0.8" 
				[1] "loop:  6 holdoutReset:  2 resample:  3"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7553" "V8526" "V8527" "V8531" "V8502" "V8505" "V8512" "V8514" "V8565"
				[1] "2: "   "V8509"
												  V8509               V8512 
							 "2a: " "0.666666666666667" "0.666666666666667" 
				[1] "loop:  7 holdoutReset:  3 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7563" "V8526" "V8528" "V8530" "V8502" "V8505" "V8512" "V7501" "V8565"
				[1] "2: "   "V8509" "V8512"
												  V8509               V8512 
							 "2a: " "0.714285714285714" "0.714285714285714" 
				[1] "loop:  8 holdoutReset:  3 resample:  2"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7553" "V8526" "V8530" "V8531" "V8509" "V8512" "V8514" "V7501" "V8565"
				[1] "2: "   "V8512"
						  V8509   V8512 
				 "2a: " "0.625"  "0.75" 
				[1] "loop:  9 holdoutReset:  3 resample:  3"
				 [1] "1: "   "V7206" "V7215" "V7551" "V7552" "V7553" "V8526" "V8527" "V8531" "V8502" "V8505" "V8512" "V8514" "V8565"
				[1] "2: "   "V8509"
												  V8509               V8512 
							 "2a: " "0.666666666666667" "0.666666666666667" 

					V8509     V8512 
				0.6666667 0.6666667 
				[1] "3: "   "V8509" "V8512"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.3720  -1.2151   0.9945   1.1401   1.3119  

				Coefficients:
							 Estimate Std. Error z value Pr(>|z|)    
				(Intercept)  0.088317   0.005733   15.40   <2e-16 ***
				V8509       -0.399054   0.018380  -21.71   <2e-16 ***
				V8512        0.358407   0.010374   34.55   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 251507  on 182337  degrees of freedom
				Residual deviance: 249901  on 182335  degrees of freedom
				AIC: 249907

				Number of Fisher Scoring iterations: 4

			[1] "seed:  5"
			[1] "Y: V8517"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7221" "V7206" "V7551" "V7552" "V7563" "V8526" "V8528" "V8530" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7553" "V7563" "V7101" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  3 holdoutReset:  1 resample:  3"
				 [1] "1: "   "V7221" "V7206" "V7551" "V7552" "V7563" "V8527" "V8529" "V8530" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  4 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7221" "V7206" "V7551" "V7552" "V7563" "V8526" "V8528" "V8530" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  5 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7553" "V7563" "V7101" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  6 holdoutReset:  2 resample:  3"
				 [1] "1: "   "V7221" "V7206" "V7551" "V7552" "V7563" "V8527" "V8529" "V8530" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  7 holdoutReset:  3 resample:  1"
				 [1] "1: "   "V7221" "V7206" "V7551" "V7552" "V7563" "V8526" "V8528" "V8530" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  8 holdoutReset:  3 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7553" "V7563" "V7101" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 
				[1] "loop:  9 holdoutReset:  3 resample:  3"
				 [1] "1: "   "V7221" "V7206" "V7551" "V7552" "V7563" "V8527" "V8529" "V8530" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8565 
				"2a: "    "1" 

				V8565 
					1 
				[1] "3: "   "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-2.1882  -0.2539  -0.2539  -0.2539   2.6271  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -3.41872    0.01561  -219.0   <2e-16 ***
				V8565        5.71713    0.05576   102.5   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 40161  on 137831  degrees of freedom
				AIC: 40165

				Number of Fisher Scoring iterations: 6

			[1] "seed:  5"
			[1] "Y: V7118"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7551" "V7552" "V7553" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7501" "V7507" "V8565"
				[1] "2: "   "V8509" "V8514"
						V8509  V8514 
				"2a: "    "1"    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8527" "V8528" "V8529" "V8531" "V8502" "V8505" "V8512" "V7501" "V7507"
				[1] "2: "   "V8565"
						V8509  V8514  V8565 
				"2a: "  "0.5"  "0.5"  "0.5" 
				[1] "loop:  3 holdoutReset:  1 resample:  3"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7551" "V7552" "V7553" "V8529" "V8530" "V8531" "V8509" "V8512" "V8514" "V7501" "V7507" "V8565"
				[1] "2: "   "V8512" "V7501"
												  V7501               V8509               V8512               V8514               V8565 
							 "2a: " "0.333333333333333" "0.333333333333333" "0.333333333333333" "0.333333333333333" "0.333333333333333" 
				[1] "loop:  4 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7551" "V7552" "V7553" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7501" "V7507" "V8565"
				[1] "2: "   "V8509" "V8514"
						V7501  V8509  V8512  V8514  V8565 
				"2a: " "0.25"  "0.5" "0.25"  "0.5" "0.25" 
				[1] "loop:  5 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8527" "V8528" "V8529" "V8531" "V8502" "V8505" "V8512" "V7501" "V7507"
				[1] "2: "   "V8565"
						V7501  V8509  V8512  V8514  V8565 
				"2a: "  "0.2"  "0.4"  "0.2"  "0.4"  "0.4" 
				[1] "loop:  6 holdoutReset:  2 resample:  3"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7551" "V7552" "V7553" "V8529" "V8530" "V8531" "V8509" "V8512" "V8514" "V7501" "V7507" "V8565"
				[1] "2: "   "V8512" "V7501"
												  V7501               V8509               V8512               V8514               V8565 
							 "2a: " "0.333333333333333" "0.333333333333333" "0.333333333333333" "0.333333333333333" "0.333333333333333" 
				[1] "loop:  7 holdoutReset:  3 resample:  1"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7551" "V7552" "V7553" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7501" "V7507" "V8565"
				[1] "2: "   "V8509" "V8514"
												  V7501               V8509               V8512               V8514               V8565 
							 "2a: " "0.285714285714286" "0.428571428571429" "0.285714285714286" "0.428571428571429" "0.285714285714286" 
				[1] "loop:  8 holdoutReset:  3 resample:  2"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8527" "V8528" "V8529" "V8531" "V8502" "V8505" "V8512" "V7501" "V7507"
				[1] "2: "   "V8565"
						  V7501   V8509   V8512   V8514   V8565 
				 "2a: "  "0.25" "0.375"  "0.25" "0.375" "0.375" 
				[1] "loop:  9 holdoutReset:  3 resample:  3"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7551" "V7552" "V7553" "V8529" "V8530" "V8531" "V8509" "V8512" "V8514" "V7501" "V7507" "V8565"
				[1] "2: "   "V8512" "V7501"
												  V7501               V8509               V8512               V8514               V8565 
							 "2a: " "0.333333333333333" "0.333333333333333" "0.333333333333333" "0.333333333333333" "0.333333333333333" 

					V7501     V8509     V8512     V8514     V8565 
				0.3333333 0.3333333 0.3333333 0.3333333 0.3333333 
				[1] "3: "   "V7501" "V8509" "V8512" "V8514" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
				   Min      1Q  Median      3Q     Max  
				-1.615  -1.008  -1.008   1.303   1.356  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -0.41155    0.02365 -17.401   <2e-16 ***
				V7501        1.06994    0.09993  10.707   <2e-16 ***
				V8509        0.99337    0.07434  13.363   <2e-16 ***
				V8512        0.13761    0.07822   1.759   0.0786 .  
				V8514        0.14831    0.07560   1.962   0.0498 *  
				V8565        0.12080    0.06903   1.750   0.0801 .  
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 15198  on 11039  degrees of freedom
				Residual deviance: 14807  on 11034  degrees of freedom
				AIC: 14819

				Number of Fisher Scoring iterations: 4		

		widthSize 5 (note the one dropped term... makes me consider thresholds.  3 terms rose, we selected two wit  a .25 threshold)

			[1] "seed:  5"
			[1] "Y: V7221"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7562" "V8527" "V8529" "V8531" "V8509" "V8512" "V8514" "V7507" "V8565"
				[1] "2: "   "V8512"
						V8512 
				"2a: "    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7551" "V7563" "V8526" "V8529" "V8531" "V8505" "V8512" "V8536" "V7501" "V8565"
				[1] "2: "   "V8512"
						V8512 
				"2a: "    "1" 
				[1] "loop:  3 holdoutReset:  1 resample:  3"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7563" "V8526" "V8527" "V8531" "V8502" "V8512" "V8536" "V7507" "V8565"
				[1] "2: "   "V8509"
												  V8509               V8512 
							 "2a: " "0.333333333333333" "0.666666666666667" 
				[1] "loop:  4 holdoutReset:  1 resample:  4"
				 [1] "1: "   "V7202" "V7206" "V7552" "V7562" "V7563" "V8526" "V8529" "V8531" "V8502" "V8505" "V8509" "V8512" "V8514"
				[1] "2: "   "V8509"
						V8509  V8512 
				"2a: "  "0.5"  "0.5" 
				[1] "loop:  5 holdoutReset:  1 resample:  5"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7552" "V7553" "V8526" "V8527" "V8528" "V8505" "V8509" "V8512" "V7501" "V7507"
				[1] "2: "   "V8514"
						V8509  V8512  V8514 
				"2a: "  "0.4"  "0.4"  "0.2" 
				[1] "loop:  6 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7562" "V8527" "V8529" "V8531" "V8509" "V8512" "V8514" "V7507" "V8565"
				[1] "2: "   "V8512"
												  V8509               V8512               V8514 
							 "2a: " "0.333333333333333"               "0.5" "0.166666666666667" 
				[1] "loop:  7 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7551" "V7563" "V8526" "V8529" "V8531" "V8505" "V8512" "V8536" "V7501" "V8565"
				[1] "2: "   "V8512"
												  V8509               V8512               V8514 
							 "2a: " "0.285714285714286" "0.571428571428571" "0.142857142857143" 
				[1] "loop:  8 holdoutReset:  2 resample:  3"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7563" "V8526" "V8527" "V8531" "V8502" "V8512" "V8536" "V7507" "V8565"
				[1] "2: "   "V8509"
						  V8509   V8512   V8514 
				 "2a: " "0.375"   "0.5" "0.125" 
				[1] "loop:  9 holdoutReset:  2 resample:  4"
				 [1] "1: "   "V7202" "V7206" "V7552" "V7562" "V7563" "V8526" "V8529" "V8531" "V8502" "V8505" "V8509" "V8512" "V8514"
				[1] "2: "   "V8509"
												  V8509               V8512               V8514 
							 "2a: " "0.444444444444444" "0.444444444444444" "0.111111111111111" 
				[1] "loop:  10 holdoutReset:  2 resample:  5"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7552" "V7553" "V8526" "V8527" "V8528" "V8505" "V8509" "V8512" "V7501" "V7507"
				[1] "2: "   "V8514"
						V8509  V8512  V8514 
				"2a: "  "0.4"  "0.4"  "0.2" 
				[1] "loop:  11 holdoutReset:  3 resample:  1"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7562" "V8527" "V8529" "V8531" "V8509" "V8512" "V8514" "V7507" "V8565"
				[1] "2: "   "V8512"
												  V8509               V8512               V8514 
							 "2a: " "0.363636363636364" "0.454545454545455" "0.181818181818182" 
				[1] "loop:  12 holdoutReset:  3 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7551" "V7563" "V8526" "V8529" "V8531" "V8505" "V8512" "V8536" "V7501" "V8565"
				[1] "2: "   "V8512"
												  V8509               V8512               V8514 
							 "2a: " "0.333333333333333"               "0.5" "0.166666666666667" 
				[1] "loop:  13 holdoutReset:  3 resample:  3"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7563" "V8526" "V8527" "V8531" "V8502" "V8512" "V8536" "V7507" "V8565"
				[1] "2: "   "V8509"
												  V8509               V8512               V8514 
							 "2a: " "0.384615384615385" "0.461538461538462" "0.153846153846154" 
				[1] "loop:  14 holdoutReset:  3 resample:  4"
				 [1] "1: "   "V7202" "V7206" "V7552" "V7562" "V7563" "V8526" "V8529" "V8531" "V8502" "V8505" "V8509" "V8512" "V8514"
				[1] "2: "   "V8509"
												  V8509               V8512               V8514 
							 "2a: " "0.428571428571429" "0.428571428571429" "0.142857142857143" 
				[1] "loop:  15 holdoutReset:  3 resample:  5"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7552" "V7553" "V8526" "V8527" "V8528" "V8505" "V8509" "V8512" "V7501" "V7507"
				[1] "2: "   "V8514"
						V8509  V8512  V8514 
				"2a: "  "0.4"  "0.4"  "0.2" 
				[1] "loop:  16 holdoutReset:  4 resample:  1"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7562" "V8527" "V8529" "V8531" "V8509" "V8512" "V8514" "V7507" "V8565"
				[1] "2: "   "V8512"
							V8509    V8512    V8514 
				  "2a: "  "0.375" "0.4375" "0.1875" 
				[1] "loop:  17 holdoutReset:  4 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7551" "V7563" "V8526" "V8529" "V8531" "V8505" "V8512" "V8536" "V7501" "V8565"
				[1] "2: "   "V8512"
												  V8509               V8512               V8514 
							 "2a: " "0.352941176470588" "0.470588235294118" "0.176470588235294" 
				[1] "loop:  18 holdoutReset:  4 resample:  3"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7563" "V8526" "V8527" "V8531" "V8502" "V8512" "V8536" "V7507" "V8565"
				[1] "2: "   "V8509"
												  V8509               V8512               V8514 
							 "2a: " "0.388888888888889" "0.444444444444444" "0.166666666666667" 
				[1] "loop:  19 holdoutReset:  4 resample:  4"
				 [1] "1: "   "V7202" "V7206" "V7552" "V7562" "V7563" "V8526" "V8529" "V8531" "V8502" "V8505" "V8509" "V8512" "V8514"
				[1] "2: "   "V8509"
												  V8509               V8512               V8514 
							 "2a: " "0.421052631578947" "0.421052631578947" "0.157894736842105" 
				[1] "loop:  20 holdoutReset:  4 resample:  5"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7552" "V7553" "V8526" "V8527" "V8528" "V8505" "V8509" "V8512" "V7501" "V7507"
				[1] "2: "   "V8514"
						V8509  V8512  V8514 
				"2a: "  "0.4"  "0.4"  "0.2" 
				[1] "loop:  21 holdoutReset:  5 resample:  1"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7562" "V8527" "V8529" "V8531" "V8509" "V8512" "V8514" "V7507" "V8565"
				[1] "2: "   "V8512"
												  V8509               V8512               V8514 
							 "2a: " "0.380952380952381" "0.428571428571429"  "0.19047619047619" 
				[1] "loop:  22 holdoutReset:  5 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7551" "V7563" "V8526" "V8529" "V8531" "V8505" "V8512" "V8536" "V7501" "V8565"
				[1] "2: "   "V8512"
												  V8509               V8512               V8514 
							 "2a: " "0.363636363636364" "0.454545454545455" "0.181818181818182" 
				[1] "loop:  23 holdoutReset:  5 resample:  3"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7563" "V8526" "V8527" "V8531" "V8502" "V8512" "V8536" "V7507" "V8565"
				[1] "2: "   "V8509"
												  V8509               V8512               V8514 
							 "2a: " "0.391304347826087" "0.434782608695652" "0.173913043478261" 
				[1] "loop:  24 holdoutReset:  5 resample:  4"
				 [1] "1: "   "V7202" "V7206" "V7552" "V7562" "V7563" "V8526" "V8529" "V8531" "V8502" "V8505" "V8509" "V8512" "V8514"
				[1] "2: "   "V8509"
												  V8509               V8512               V8514 
							 "2a: " "0.416666666666667" "0.416666666666667" "0.166666666666667" 
				[1] "loop:  25 holdoutReset:  5 resample:  5"
				 [1] "1: "   "V7202" "V7215" "V7101" "V7552" "V7553" "V8526" "V8527" "V8528" "V8505" "V8509" "V8512" "V7501" "V7507"
				[1] "2: "   "V8514"
						V8509  V8512  V8514 
				"2a: "  "0.4"  "0.4"  "0.2" 

				V8509 V8512 V8514 
				  0.4   0.4   0.2 
				[1] "3: "   "V8509" "V8512"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-1.3720  -1.2151   0.9945   1.1401   1.3119  

				Coefficients:
							 Estimate Std. Error z value Pr(>|z|)    
				(Intercept)  0.088317   0.005733   15.40   <2e-16 ***
				V8509       -0.399054   0.018380  -21.71   <2e-16 ***
				V8512        0.358407   0.010374   34.55   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 251507  on 182337  degrees of freedom
				Residual deviance: 249901  on 182335  degrees of freedom
				AIC: 249907

				Number of Fisher Scoring iterations: 4
				
			[1] "seed:  5"
			[1] "Y: V8517"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7552" "V7553" "V7563" "V8527" "V8528" "V8529" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "    "1"    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8530" "V8509" "V8512" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8514  V8565 
				"2a: "  "0.5"    "1" 
				[1] "loop:  3 holdoutReset:  1 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V8528" "V8529" "V8530" "V8531" "V8505" "V8509" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.333333333333333"                 "1" 
				[1] "loop:  4 holdoutReset:  1 resample:  4"
				 [1] "1: "   "V7221" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						V8514  V8565 
				"2a: " "0.25"    "1" 
				[1] "loop:  5 holdoutReset:  1 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V7552" "V7562" "V7563" "V7101" "V8526" "V8528" "V8531" "V8505" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "  "0.4"    "1" 
				[1] "loop:  6 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7552" "V7553" "V7563" "V8527" "V8528" "V8529" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "  "0.5"    "1" 
				[1] "loop:  7 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8530" "V8509" "V8512" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.428571428571429"                 "1" 
				[1] "loop:  8 holdoutReset:  2 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V8528" "V8529" "V8530" "V8531" "V8505" "V8509" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
						  V8514   V8565 
				 "2a: " "0.375"     "1" 
				[1] "loop:  9 holdoutReset:  2 resample:  4"
				 [1] "1: "   "V7221" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.333333333333333"                 "1" 
				[1] "loop:  10 holdoutReset:  2 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V7552" "V7562" "V7563" "V7101" "V8526" "V8528" "V8531" "V8505" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "  "0.4"    "1" 
				[1] "loop:  11 holdoutReset:  3 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7552" "V7553" "V7563" "V8527" "V8528" "V8529" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
												  V8514               V8565 
							 "2a: " "0.454545454545455"                 "1" 
				[1] "loop:  12 holdoutReset:  3 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8530" "V8509" "V8512" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.416666666666667"                 "1" 
				[1] "loop:  13 holdoutReset:  3 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V8528" "V8529" "V8530" "V8531" "V8505" "V8509" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.384615384615385"                 "1" 
				[1] "loop:  14 holdoutReset:  3 resample:  4"
				 [1] "1: "   "V7221" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.357142857142857"                 "1" 
				[1] "loop:  15 holdoutReset:  3 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V7552" "V7562" "V7563" "V7101" "V8526" "V8528" "V8531" "V8505" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "  "0.4"    "1" 
				[1] "loop:  16 holdoutReset:  4 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7552" "V7553" "V7563" "V8527" "V8528" "V8529" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
							V8514    V8565 
				  "2a: " "0.4375"      "1" 
				[1] "loop:  17 holdoutReset:  4 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8530" "V8509" "V8512" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.411764705882353"                 "1" 
				[1] "loop:  18 holdoutReset:  4 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V8528" "V8529" "V8530" "V8531" "V8505" "V8509" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.388888888888889"                 "1" 
				[1] "loop:  19 holdoutReset:  4 resample:  4"
				 [1] "1: "   "V7221" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.368421052631579"                 "1" 
				[1] "loop:  20 holdoutReset:  4 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V7552" "V7562" "V7563" "V7101" "V8526" "V8528" "V8531" "V8505" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "  "0.4"    "1" 
				[1] "loop:  21 holdoutReset:  5 resample:  1"
				 [1] "1: "   "V7206" "V7215" "V7552" "V7553" "V7563" "V8527" "V8528" "V8529" "V8531" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
												  V8514               V8565 
							 "2a: " "0.428571428571429"                 "1" 
				[1] "loop:  22 holdoutReset:  5 resample:  2"
				 [1] "1: "   "V7202" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8530" "V8509" "V8512" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.409090909090909"                 "1" 
				[1] "loop:  23 holdoutReset:  5 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7551" "V7552" "V7563" "V8528" "V8529" "V8530" "V8531" "V8505" "V8509" "V8536" "V7507" "V8565"
				[1] "2: "   "V8565"
												  V8514               V8565 
							 "2a: " "0.391304347826087"                 "1" 
				[1] "loop:  24 holdoutReset:  5 resample:  4"
				 [1] "1: "   "V7221" "V7215" "V7551" "V7552" "V7553" "V7562" "V7563" "V8526" "V8528" "V8529" "V8531" "V8505" "V8509" "V8514" "V8536" "V8565"
				[1] "2: "   "V8565"
						  V8514   V8565 
				 "2a: " "0.375"     "1" 
				[1] "loop:  25 holdoutReset:  5 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V7552" "V7562" "V7563" "V7101" "V8526" "V8528" "V8531" "V8505" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514" "V8565"
						V8514  V8565 
				"2a: "  "0.4"    "1" 

				V8514 V8565 
				  0.4   1.0 
				[1] "3: "   "V8514" "V8565"
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
					Min       1Q   Median       3Q      Max  
				-2.6590  -0.1585  -0.1585  -0.1585   2.9610  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -4.37123    0.02676 -163.32   <2e-16 ***
				V8514        2.32668    0.03265   71.27   <2e-16 ***
				V8565        5.55016    0.06025   92.11   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 61417  on 137832  degrees of freedom
				Residual deviance: 34637  on 137830  degrees of freedom
				AIC: 34643

				Number of Fisher Scoring iterations: 7	
				
			[1] "seed:  5"
			[1] "Y: V7118"
				[1] "loop:  1 holdoutReset:  1 resample:  1"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7507" "V8565"
				[1] "2: "   "V7507"
						V7507 
				"2a: "    "1" 
				[1] "loop:  2 holdoutReset:  1 resample:  2"
				 [1] "1: "   "V7202" "V7206" "V7215" "V7551" "V7553" "V7562" "V8526" "V8527" "V8531" "V8502" "V8512" "V8514" "V8536" "V7507"
				[1] "2: "   "V8565"
						V7507  V8565 
				"2a: "  "0.5"  "0.5" 
				[1] "loop:  3 holdoutReset:  1 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7551" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8502" "V8505" "V8509" "V8536" "V7507"
				[1] "2: "   "V7501"
												  V7501               V7507               V8565 
							 "2a: " "0.333333333333333" "0.333333333333333" "0.333333333333333" 
				[1] "loop:  4 holdoutReset:  1 resample:  4"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8529" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502"
						V7501  V7507  V8502  V8565 
				"2a: " "0.25" "0.25" "0.25" "0.25" 
				[1] "loop:  5 holdoutReset:  1 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7553" "V7562" "V8527" "V8530" "V8531" "V8502" "V8505" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514"
						V7501  V7507  V8502  V8514  V8565 
				"2a: "  "0.2"  "0.2"  "0.2"  "0.2"  "0.2" 
				[1] "loop:  6 holdoutReset:  2 resample:  1"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7507" "V8565"
				[1] "2: "   "V7507"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.166666666666667" "0.333333333333333" "0.166666666666667" "0.166666666666667" "0.166666666666667" 
				[1] "loop:  7 holdoutReset:  2 resample:  2"
				 [1] "1: "   "V7202" "V7206" "V7215" "V7551" "V7553" "V7562" "V8526" "V8527" "V8531" "V8502" "V8512" "V8514" "V8536" "V7507"
				[1] "2: "   "V8565"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.142857142857143" "0.285714285714286" "0.142857142857143" "0.142857142857143" "0.285714285714286" 
				[1] "loop:  8 holdoutReset:  2 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7551" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8502" "V8505" "V8509" "V8536" "V7507"
				[1] "2: "   "V7501"
						  V7501   V7507   V8502   V8514   V8565 
				 "2a: "  "0.25"  "0.25" "0.125" "0.125"  "0.25" 
				[1] "loop:  9 holdoutReset:  2 resample:  4"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8529" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.222222222222222" "0.222222222222222" "0.222222222222222" "0.111111111111111" "0.222222222222222" 
				[1] "loop:  10 holdoutReset:  2 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7553" "V7562" "V8527" "V8530" "V8531" "V8502" "V8505" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514"
						V7501  V7507  V8502  V8514  V8565 
				"2a: "  "0.2"  "0.2"  "0.2"  "0.2"  "0.2" 
				[1] "loop:  11 holdoutReset:  3 resample:  1"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7507" "V8565"
				[1] "2: "   "V7507"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.181818181818182" "0.272727272727273" "0.181818181818182" "0.181818181818182" "0.181818181818182" 
				[1] "loop:  12 holdoutReset:  3 resample:  2"
				 [1] "1: "   "V7202" "V7206" "V7215" "V7551" "V7553" "V7562" "V8526" "V8527" "V8531" "V8502" "V8512" "V8514" "V8536" "V7507"
				[1] "2: "   "V8565"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.166666666666667"              "0.25" "0.166666666666667" "0.166666666666667"              "0.25" 
				[1] "loop:  13 holdoutReset:  3 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7551" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8502" "V8505" "V8509" "V8536" "V7507"
				[1] "2: "   "V7501"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.230769230769231" "0.230769230769231" "0.153846153846154" "0.153846153846154" "0.230769230769231" 
				[1] "loop:  14 holdoutReset:  3 resample:  4"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8529" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.214285714285714" "0.214285714285714" "0.214285714285714" "0.142857142857143" "0.214285714285714" 
				[1] "loop:  15 holdoutReset:  3 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7553" "V7562" "V8527" "V8530" "V8531" "V8502" "V8505" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514"
						V7501  V7507  V8502  V8514  V8565 
				"2a: "  "0.2"  "0.2"  "0.2"  "0.2"  "0.2" 
				[1] "loop:  16 holdoutReset:  4 resample:  1"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7507" "V8565"
				[1] "2: "   "V7507"
							V7501    V7507    V8502    V8514    V8565 
				  "2a: " "0.1875"   "0.25" "0.1875" "0.1875" "0.1875" 
				[1] "loop:  17 holdoutReset:  4 resample:  2"
				 [1] "1: "   "V7202" "V7206" "V7215" "V7551" "V7553" "V7562" "V8526" "V8527" "V8531" "V8502" "V8512" "V8514" "V8536" "V7507"
				[1] "2: "   "V8565"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.176470588235294" "0.235294117647059" "0.176470588235294" "0.176470588235294" "0.235294117647059" 
				[1] "loop:  18 holdoutReset:  4 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7551" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8502" "V8505" "V8509" "V8536" "V7507"
				[1] "2: "   "V7501"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.222222222222222" "0.222222222222222" "0.166666666666667" "0.166666666666667" "0.222222222222222" 
				[1] "loop:  19 holdoutReset:  4 resample:  4"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8529" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.210526315789474" "0.210526315789474" "0.210526315789474" "0.157894736842105" "0.210526315789474" 
				[1] "loop:  20 holdoutReset:  4 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7553" "V7562" "V8527" "V8530" "V8531" "V8502" "V8505" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514"
						V7501  V7507  V8502  V8514  V8565 
				"2a: "  "0.2"  "0.2"  "0.2"  "0.2"  "0.2" 
				[1] "loop:  21 holdoutReset:  5 resample:  1"
				 [1] "1: "   "V7202" "V8517" "V7206" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8505" "V8509" "V8514" "V7507" "V8565"
				[1] "2: "   "V7507"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: "  "0.19047619047619" "0.238095238095238"  "0.19047619047619"  "0.19047619047619"  "0.19047619047619" 
				[1] "loop:  22 holdoutReset:  5 resample:  2"
				 [1] "1: "   "V7202" "V7206" "V7215" "V7551" "V7553" "V7562" "V8526" "V8527" "V8531" "V8502" "V8512" "V8514" "V8536" "V7507"
				[1] "2: "   "V8565"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.181818181818182" "0.227272727272727" "0.181818181818182" "0.181818181818182" "0.227272727272727" 
				[1] "loop:  23 holdoutReset:  5 resample:  3"
				 [1] "1: "   "V7202" "V7221" "V7206" "V7551" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8531" "V8502" "V8505" "V8509" "V8536" "V7507"
				[1] "2: "   "V7501"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.217391304347826" "0.217391304347826" "0.173913043478261" "0.173913043478261" "0.217391304347826" 
				[1] "loop:  24 holdoutReset:  5 resample:  4"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7562" "V7563" "V8526" "V8527" "V8528" "V8529" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8502"
												  V7501               V7507               V8502               V8514               V8565 
							 "2a: " "0.208333333333333" "0.208333333333333" "0.208333333333333" "0.166666666666667" "0.208333333333333" 
				[1] "loop:  25 holdoutReset:  5 resample:  5"
				 [1] "1: "   "V7202" "V7221" "V8517" "V7552" "V7553" "V7562" "V8527" "V8530" "V8531" "V8502" "V8505" "V8509" "V8514" "V8536" "V7507" "V8565"
				[1] "2: "   "V8514"
						V7501  V7507  V8502  V8514  V8565 
				"2a: "  "0.2"  "0.2"  "0.2"  "0.2"  "0.2" 

				V7501 V7507 V8502 V8514 V8565 
				  0.2   0.2   0.2   0.2   0.2 
				[1] "3: "
				[1] "population"

				Call:
				NULL

				Deviance Residuals: 
				   Min      1Q  Median      3Q     Max  
				-1.095  -1.095  -1.095   1.262   1.262  

				Coefficients:
							Estimate Std. Error z value Pr(>|z|)    
				(Intercept) -0.19738    0.01913  -10.32   <2e-16 ***
				---
				Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				(Dispersion parameter for binomial family taken to be 1)

					Null deviance: 15198  on 11039  degrees of freedom
				Residual deviance: 15198  on 11039  degrees of freedom
				AIC: 15200

				Number of Fisher Scoring iterations: 3				