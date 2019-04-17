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
			
		WidthSize 1
			7221

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


			V8517

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

			V7118

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

		WidthSize 3
			7221

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


			V8517


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

			V7118

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
	
	