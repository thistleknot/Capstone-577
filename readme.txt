Input	

	10 holdout's of 10% x 2 = 200%
		3 resamples at 33% each
		30 runs
		
	columns
		factor id, conversion profile, description, category flag
		
		threshold
			so far .5 results in none's
			.275 to .33 seems to be the sweet spot
			.25 results in nons
		
			gangfight.txt
			gpalist.txt
			psyDFilterList.txt

	To run 
		Modify in cleandatacode.R
			sourceDir to point to where you downloaded and extracted the sourcefolder to.  This should be the same folder that output sits in.			
		Run cleandatacode.R which starts the simulation up until the point where it starts to output tabulation results.
			Then stop the simulation
				You can let it run all the way through, but the files necessary to run the rest of the simulation (*final.csv) are already in the output folder.
		Run saveCSVs.R and allow it to finish
		Run 4thpass.R
			
	Update 
		in cleandatacode.r
			sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
			
	Outputs
		cleandatacode.R
			*final.csv
		saveCSVs.R
			*filtered.csv
	
	The system consists of 3 different main files.  Below is the order in which they are called.  Cleandatacode.R can be thought of as the main class file in C++.
	
	Cleandatacode.R
		creates tabulated results for threshold filtering sequenced by common outputted terms vs ranked by propensity.
		Derives final.csv which stores cross validated thresholds, 
		Does factor reduction/pooling
		
	saveCSVs.R
		does further factor reduction.
		Ensures factor reduced terms work well with each other using Cross Validation and derives final terms
		
	4thpass.R
		Derive optimal class confusion matrix
		ROC plots
		Check VIF for collinearity
	
	Other files related to cleaning, transforming, indexing, class balancing, and aggregating the data into dataframes are (and in their proper order of being called).  All these files are assumed to be sourced from cleandatacode.R
		newDF.R
		reseedboth.R
		reseedTest.R
		reseedTrain.R
		MCResampleTest.R
		MCResampleTrain.R
		redrawTrain.R
		redrawTest.R
	
	The project creates a training and holdout partition. Both partitions use cross validation using a modified bestglm algorithm which expands on the best subsets algorithm by including more relevant terms.
	
	Libraries
		dplyr
		plyr
		RPostgreSQL
		ggplot2
		anchors
		caret
		corrplot
		MASS
		car
		leaps
		bestglm
		compare
		R.utils
		tidyr
		stringr
		
	
	
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
	NA's
	Records
		182338 
		[1] "min one:" "0.014"   
		[1] "min zero:" "0.043"
	
		Min Class: 2552.732 records
		*.03 = 76 records for holdoutSet of which 1/widthSize is taken (1/3) which is 25
		just barely the min required with bias balanced classes?
		
			Based on these numbers
			
			min one size class is 2552
			2552*CVRuns_pct_threshold
				=value below
				
				/widthSize
					=25 records per holdout
					
					/widthSize
			
			2552*.1
				255
			2552*.07
				178
			2552*.05
				127
			2552*.04
				102
			2552*.03
				76
				
		
		the pattern of similar na %'s has me wondering if some years are missing data... 
		I only check medians on certain factors... 

			[1] "V7118"

					-1          0          1 
			0.93945310 0.03325143 0.02729546 
			[1] "V7221"

					-1          0          1 
			0.44102710 0.04268447 0.51628843 
			[1] "V7552"

				   -1         0         1 
			0.2161261 0.2240893 0.5597846 
			[1] "V7551"

					-1          0          1 
			0.09232853 0.71359782 0.19407364 
			[1] "V7553"

				   -1         0         1 
			0.1081453 0.7134004 0.1784543 
			[1] "V7562"

				   -1         0         1 
			0.1168873 0.7142669 0.1688458 
			[1] "V7563"

					-1          0          1 
			0.04332065 0.71545701 0.24122235 
			[1] "V8512"

				   -1         0         1 
			0.1309162 0.5658667 0.3032171 
			[1] "V8502"

					-1          0          1 
			0.34671325 0.55844640 0.09484035 
			[1] "V8505"

				   -1         0         1 
			0.1203205 0.5620551 0.3176244 
			[1] "V8509"

				   -1         0         1 
			0.3642082 0.5645779 0.0712139 
			[1] "V8514"

					-1          0          1 
			0.08559379 0.56671127 0.34769494 
			[1] "V8536"

					-1          0          1 
			0.27011374 0.71585188 0.01403438 
			[1] "V8565"

				   -1         0         1 
			0.1549156 0.7141024 0.1309820 
			[1] "V7202"

					-1          0          1 
			0.47573188 0.04147791 0.48279020 
			[1] "V7215"

					-1          0          1 
			0.45336682 0.03344887 0.51318431 
			[1] "V7206"

				   -1         1 
			0.2493501 0.7506499 
			[1] "V8517"

					-1          0          1 
			0.24407968 0.71170573 0.04421459 
			[1] "V8526"

				   -1         0         1 
			0.1190646 0.7115193 0.1694161 
			[1] "V8527"

					-1          0          1 
			0.09892069 0.71324683 0.18783249 
			[1] "V8528"

					-1          0          1 
			0.05733857 0.71689938 0.22576205 
			[1] "V8529"

				   -1         0         1 
			0.0953833 0.7137130 0.1909037 
			[1] "V8530"

					-1          0          1 
			0.09931556 0.71407496 0.18660948 
			[1] "V8531"

				   -1         0         1 
			0.1502978 0.7142998 0.1354024 

Category Flags
	Y's (0)
		V7202,0,"R'S SEX",2
		V7206,0,"R'S HSHLD FATHER",2
		V7215,2,"FATHR EDUC LEVEL",2
		V7551,2,"#HR/W INTERNET S",3
		V7552,2,"DALY WEB FACEBK",3
		V7553,2,"#HR GAMING",3
		V7562,2,"#HR TEXT",7
		V7563,2,"#HR TALK CELL",7
		V8526,3,"OFTN EAT BRKFST",4
		V8527,3,"OFTN EAT GN VEG",4
		V8528,3,"OFTN EAT FRUIT",4
		V8529,3,"OFTN EXERCISE",4
		V8530,3,"OFTN 7HRS SLEEP",4
		V8531,3,"OFTN SLEEP <SHLD",4
		V8502,3,"LIFE MEANINGLESS",5
		V8505,3,"I ENJOY LIFE",5
		V8509,3,"FUTURE HOPELESS",5
		V8512,3,"SATISFD W MYSELF",5
		V8514,3,"GOOD TO BE ALIVE",6
		V8536,3,"FUTR R LIFE WRSE",6
		V8565,3,"I AM OFTEN BORED",6	

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

	Median assumptions
	
		curious to see how many b+ were in the population.  Grabbing the % nails down what propotion of the population we are splitting by when classifying.	
	
		Code to check medians


			for (interests in c("V7221","V7215","V7551","V7552","V7553","V7562","V7563"))
			{
			  print(paste("interest:",interests))
			  for(year in c("d_2012","d_2013","d_2014","d_2015","d_2016","d_2017"))
			  {
				print(paste("year:",year))
				
				#https://stackoverflow.com/questions/28802652/access-variable-dataframe-in-r-loop
				df <- (get(year)[,interests])
				
				centerpoint = (length(df[df>0]))/2
				
				#print(centerpoint)
				width = round(1.96*sqrt((length(df[df>0])))/2)
				
				lower = (length(df[df>0]))/2 - width
				upper = (length(df[df>0]))/2 + width
				print(paste("lower:", sort(((df[df>0])))[lower]))
				print(paste("median:",median(df[df>0])))
				print(paste("upper:",sort(((df[df>0])))[upper]))
				
				#https://stackoverflow.com/questions/9317830/r-do-i-need-to-add-explicit-new-line-character-with-print
				writeLines("\n")
			  } 
			  writeLines("\n")
			}
	
	
			[1] "interest: V7221"
			[1] "year: d_2012"
			[1] "lower: 7"
			[1] "median: 7"
			[1] "upper: 7"

					 1          2          3          4          5          6          7          8          9 
			0.02487496 0.03540790 0.05895797 0.08880130 0.09877116 0.15666921 0.16597662 0.17766884 0.19138154 


			[1] "year: d_2013"
			[1] "lower: 7"
			[1] "median: 7"
			[1] "upper: 7"

					 1          2          3          4          5          6          7          8          9 
			0.02487496 0.03540790 0.05895797 0.08880130 0.09877116 0.15666921 0.16597662 0.17766884 0.19138154 


			[1] "year: d_2014"
			[1] "lower: 7"
			[1] "median: 7"
			[1] "upper: 7"

					 1          2          3          4          5          6          7          8          9 
			0.02965777 0.04054202 0.05825633 0.09499982 0.10179335 0.14741225 0.16768326 0.17115307 0.18685854 


			[1] "year: d_2015"
			[1] "lower: 7"
			[1] "median: 7"
			[1] "upper: 7"

					 1          2          3          4          5          6          7          8          9 
			0.02965777 0.04054202 0.05825633 0.09499982 0.10179335 0.14741225 0.16768326 0.17115307 0.18685854 


			[1] "year: d_2016"
			[1] "lower: 7"
			[1] "median: 7"
			[1] "upper: 7"

					 1          2          3          4          5          6          7          8          9 
			0.02376975 0.03443402 0.05232558 0.08592445 0.09379417 0.14779006 0.16667737 0.18180650 0.21203264 


			[1] "year: d_2017"
			[1] "lower: 7"
			[1] "median: 7"
			[1] "upper: 7"

					 1          2          3          4          5          6          7          8          9 
			0.02661717 0.03645852 0.05638637 0.08780163 0.09830841 0.14660456 0.16211957 0.18376353 0.20036423 


			[1] "year: d_combined"
			[1] "lower: 7"
			[1] "median: 7"
			[1] "upper: 7"

					 1          2          3          4          5          6          7          8          9 
			0.02649485 0.03706186 0.05721077 0.09018328 0.09888316 0.15073883 0.16621993 0.17758877 0.19536082 




			[1] "interest: V7215"
			[1] "year: d_2012"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7 
			0.03113173 0.08911706 0.21706651 0.12446112 0.23776615 0.15536249 0.14417350 


			[1] "year: d_2013"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7 
			0.03113173 0.08911706 0.21706651 0.12446112 0.23776615 0.15536249 0.14417350 


			[1] "year: d_2014"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7 
			0.03639986 0.09478450 0.22372329 0.11825426 0.22586020 0.13889895 0.16106483 


			[1] "year: d_2015"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7 
			0.03639986 0.09478450 0.22372329 0.11825426 0.22586020 0.13889895 0.16106483 


			[1] "year: d_2016"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7 
			0.03977561 0.09777510 0.21970081 0.11492140 0.22169752 0.13650482 0.16873732 


			[1] "year: d_2017"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7 
			0.04417726 0.10405045 0.21350612 0.10865874 0.21468418 0.13696684 0.17698624 


			[1] "year: d_combined"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7 
			0.03649010 0.09495822 0.21921290 0.11832050 0.22756954 0.14394073 0.15934917 




			[1] "interest: V7551"
			[1] "year: d_2012"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.07359644 0.10894942 0.18832685 0.23557532 0.14897165 0.10472485 0.05058366 0.02401334 0.06025570 


			[1] "year: d_2013"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.07359644 0.10894942 0.18832685 0.23557532 0.14897165 0.10472485 0.05058366 0.02401334 0.06025570 


			[1] "year: d_2014"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.08535249 0.08997930 0.14915378 0.20029222 0.14477049 0.12151467 0.06501887 0.03056131 0.10787775 


			[1] "year: d_2015"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.08535249 0.08997930 0.14915378 0.20029222 0.14477049 0.12151467 0.06501887 0.03056131 0.10787775 


			[1] "year: d_2016"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7          8          9 
			0.08174300 0.06891433 0.12648431 0.20006361 0.15182358 0.13878287 0.07612383 0.03859203 0.11270144 


			[1] "year: d_2017"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5          6          7          8          9 
			0.07762610 0.06293383 0.11614993 0.19736233 0.15698751 0.14449329 0.08132809 0.04060620 0.11730680 


			[1] "year: d_combined"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.07974439 0.08862188 0.15372989 0.21277288 0.15013297 0.12317523 0.06503147 0.03154954 0.09438078 




			[1] "interest: V7552"
			[1] "year: d_2012"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5 
			0.09367440 0.02819264 0.05883681 0.16573659 0.65307571 


			[1] "year: d_2013"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5 
			0.09367440 0.02819264 0.05883681 0.16573659 0.65307571 


			[1] "year: d_2014"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5 
			0.07669975 0.02560177 0.03927365 0.10821368 0.74941934 


			[1] "year: d_2015"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5 
			0.07669975 0.02560177 0.03927365 0.10821368 0.74941934 


			[1] "year: d_2016"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5 
			0.06817449 0.02197245 0.03164586 0.09268046 0.78483578 


			[1] "year: d_2017"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5 
			0.06214633 0.02028144 0.02754269 0.08072512 0.80855326 


			[1] "year: d_combined"
			[1] "lower: 5"
			[1] "median: 5"
			[1] "upper: 5"

					 1          2          3          4          5 
			0.08081672 0.02544295 0.04504110 0.12721477 0.72137844 




			[1] "interest: V7553"
			[1] "year: d_2012"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09102009 0.13431013 0.18048618 0.19746920 0.13819514 0.09668110 0.05527806 0.02242202 0.07914308 


			[1] "year: d_2013"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09102009 0.13431013 0.18048618 0.19746920 0.13819514 0.09668110 0.05527806 0.02242202 0.07914308 


			[1] "year: d_2014"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09117433 0.11342086 0.15742767 0.18867007 0.13469487 0.10454656 0.06199854 0.02844639 0.11415026 


			[1] "year: d_2015"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09117433 0.11342086 0.15742767 0.18867007 0.13469487 0.10454656 0.06199854 0.02844639 0.11415026 


			[1] "year: d_2016"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.08656906 0.11574369 0.15743688 0.18958201 0.13727986 0.11436452 0.06566942 0.03055379 0.09802673 


			[1] "year: d_2017"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09522706 0.10947637 0.14979147 0.19531974 0.13739574 0.11144578 0.06846617 0.03185820 0.09580630 


			[1] "year: d_combined"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09135231 0.12087261 0.16478978 0.19377474 0.13739174 0.10519473 0.06167906 0.02743628 0.09664838 




			[1] "interest: V7562"
			[1] "year: d_2012"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.10860131 0.13530655 0.15010571 0.13797708 0.10648715 0.08857238 0.06687437 0.03961277 0.16145544 


			[1] "year: d_2013"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.10860131 0.13530655 0.15010571 0.13797708 0.10648715 0.08857238 0.06687437 0.03961277 0.16145544 


			[1] "year: d_2014"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09376905 0.14595781 0.15583465 0.15242044 0.11681502 0.08815998 0.06243141 0.03548348 0.14364102 


			[1] "year: d_2015"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09376905 0.14595781 0.15583465 0.15242044 0.11681502 0.08815998 0.06243141 0.03548348 0.14364102 


			[1] "year: d_2016"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.07490956 0.16035327 0.19099808 0.16077889 0.12981485 0.09267929 0.05586295 0.03064482 0.09917004 


			[1] "year: d_2017"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.07014891 0.16228478 0.20230340 0.17717543 0.11842718 0.08852955 0.05304793 0.02687296 0.09597487 


			[1] "year: d_combined"
			[1] "lower: 4"
			[1] "median: 4"
			[1] "upper: 4"

					 1          2          3          4          5          6          7          8          9 
			0.09199348 0.14820213 0.16853006 0.15368683 0.11636782 0.08955796 0.06150158 0.03476843 0.13452872 




			[1] "interest: V7563"
			[1] "year: d_2012"
			[1] "lower: 2"
			[1] "median: 2"
			[1] "upper: 3"

					 1          2          3          4          5          6          7          8          9 
			0.14113578 0.36293652 0.20841236 0.11145822 0.06370635 0.04016512 0.02521477 0.01238425 0.02956599 


			[1] "year: d_2013"
			[1] "lower: 2"
			[1] "median: 2"
			[1] "upper: 3"

					 1          2          3          4          5          6          7          8          9 
			0.14113578 0.36293652 0.20841236 0.11145822 0.06370635 0.04016512 0.02521477 0.01238425 0.02956599 


			[1] "year: d_2014"
			[1] "lower: 2"
			[1] "median: 2"
			[1] "upper: 2"

					 1          2          3          4          5          6          7          8          9 
			0.16021084 0.37974994 0.19404266 0.09843099 0.05614121 0.03518019 0.01949007 0.01127727 0.03996077 


			[1] "year: d_2015"
			[1] "lower: 2"
			[1] "median: 2"
			[1] "upper: 2"

					 1          2          3          4          5          6          7          8          9 
			0.16021084 0.37974994 0.19404266 0.09843099 0.05614121 0.03518019 0.01949007 0.01127727 0.03996077 


			[1] "year: d_2016"
			[1] "lower: 2"
			[1] "median: 2"
			[1] "upper: 2"

					 1          2          3          4          5          6          7          8          9 
			0.15513560 0.38650438 0.19378603 0.10196455 0.05947042 0.03363229 0.01815076 0.01089045 0.03566090 


			[1] "year: d_2017"
			[1] "lower: 2"
			[1] "median: 2"
			[1] "upper: 2"

					 1          2          3          4          5          6          7          8          9 
			0.15236981 0.38197776 0.19368051 0.10228204 0.05710942 0.03967232 0.02235225 0.01006437 0.03522528 


			[1] "year: d_combined"
			[1] "lower: 2"
			[1] "median: 2"
			[1] "upper: 2"

					 1          2          3          4          5          6          7          8          9 
			0.15211447 0.37717609 0.19973810 0.10462564 0.05975582 0.03751348 0.02178016 0.01143892 0.03499076 
	


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
		Incorrect formula's, but implementation of 4thpass.

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
	
	Milestone 11
		Paired datassets
		Null's removed from data
		
	Milestone 12
		Nulls removed
		Bias balancing of data for response term
		
		crossValidated finds terms that were preferred between training and holdout.
		tabulatedCrossValidated
			
	Milestone 13
		segmented subfunctions to separate files
		function to process tabulated results and create csvs aside from the main loop
		
		widthSize 3 output
		removed gang, smoking, gpa, psyd from each list unless it was response
		
	Milestone 14
	
		14.1
			Auto-installer of library packages
			b4d7ab4
			
		14.0: commit: af2ffd1
		Readded gangs, GPA, and psyd so I could extract those columns from NewDF within saveCSVs.R
		
		saveCSVs.R does further class balancing and montecarlo cv factor reduction and gives me a new set of cv's that are gauranteed to be population significant.
		I then compare formula's
		
		Finally.  I then run these through 4thpass.R
		
		Introuced error handling from bestglm throwing an error for some internal reason I can't figureout.   So I'm simply aggregating to NA.  I have no idea what else to do.  The error hardly occurs.  I should track the # of times an error is thrown.
		I only saw it in cvwidthSize 10
		
		added logging of error pairs so I can compare with final tabulated results if it was one of the ones I found significant or not to be concerned.
		error logging did not work and took me over 4 hours of troubleshooting to fallback on a hack of writing a csv to the output folder to log the event... rbind will output to console (but not to a dataframe in an errorloop?)

	Milestone 15 ca9100c
		I output to jpg's and apply mc model to population as well as derive rmse of cv mc model against population as well as population model against itself using round(logit)
		
		V7118

			[1] "C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577//output/V7118-greaterEqual-10-final.csv"
			[1] "y:"    "V7118"
			[[1]]
			[1] "final: "

			$tabulatedCrossValidated
			 [1] <NA>  V8502 V8512 V8509 V8514 V8530 V8505 V7215 V8536 V8526 V7206 V7202 V8528 V7562 V7552 V8529 V8531 V8527 V8565 V7553 V7563 V7551
			Levels: V7202 V7206 V7215 V7551 V7552 V7553 V7562 V7563 V8502 V8505 V8509 V8512 V8514 V8526 V8527 V8528 V8529 V8530 V8531 V8536 V8565

			$Freq
			 [1] 3.517 0.433 0.383 0.350 0.350 0.333 0.317 0.267 0.267 0.233 0.217 0.200 0.150 0.133 0.117 0.117 0.117 0.100 0.100 0.083 0.083 0.067

			   tabulatedCrossValidated  Freq
			2                    V8502 0.433
			3                    V8512 0.383
			4                    V8509 0.350
			5                    V8514 0.350
			6                    V8530 0.333
			7                    V8505 0.317
			8                    V7215 0.267
			9                    V8536 0.267
			10                   V8526 0.233
			11                   V7206 0.217
			12                   V7202 0.200
			13                   V8528 0.150
			14                   V7562 0.133
			15                   V7552 0.117
			16                   V8529 0.117
			17                   V8531 0.117
			18                   V8527 0.100
			19                   V8565 0.100
			20                   V7553 0.083
			21                   V7563 0.083
			22                   V7551 0.067
			 [1] "keep: > " "0.25"     "8"        "V8502"    "V8512"    "V8509"    "V8514"    "V8530"    "V8505"    "V7215"    "V8536"   
				 V8502      V8512      V8509      V8530      V7215 
			 0.5423997  0.5023144  0.8560780 -0.5246812  0.2495730 
				 V7118         V8502            V8512            V8509             V8514          V8530            V8505            V7215            V8536        
			 Min.   :0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
			 1st Qu.:0.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
			 Median :0.5   Median :0.0000   Median :0.0000   Median :0.00000   Median :0.00   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.00000  
			 Mean   :0.5   Mean   :0.1299   Mean   :0.2203   Mean   :0.09739   Mean   :0.25   Mean   :0.1432   Mean   :0.2289   Mean   :0.4157   Mean   :0.02048  
			 3rd Qu.:1.0   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.25   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.00000  
			 Max.   :1.0   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.00   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  

			Call:
			imcdiag(x = x, y = y, method = method, corr = FALSE, vif = vif, 
				tol = tol, conf = conf, cvif = cvif, leamer = leamer, all = all)


			 VIF Multicollinearity Diagnostics

					 VIF detection
			V8502 1.6330         0
			V8512 3.4039         0
			V8509 1.6528         0
			V8514 3.8443         0
			V8530 1.2904         0
			V8505 3.4856         0
			V7215 1.0190         0
			V8536 1.0749         0

			NOTE:  VIF Method Failed to detect multicollinearity


			0 --> COLLINEARITY is not detected by the test

			===================================
			[1] "MC summary"

			Call:
			glm(formula = y ~ ., family = family, data = data.frame(Xy[, 
				c(bestset[-1], FALSE), drop = FALSE], y = y))

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-2.0068  -1.0610  -0.1622   1.1904   1.5326  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.28009    0.04102  -6.828 8.58e-12 ***
			V8502        0.54240    0.11385   4.764 1.90e-06 ***
			V8512        0.50231    0.08016   6.266 3.69e-10 ***
			V8509        0.85608    0.13398   6.390 1.66e-10 ***
			V8530       -0.52468    0.09441  -5.558 2.73e-08 ***
			V7215        0.24957    0.05926   4.212 2.53e-05 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 6903.7  on 4979  degrees of freedom
			Residual deviance: 6664.0  on 4974  degrees of freedom
			AIC: 6676

			Number of Fisher Scoring iterations: 4

			[1] "optCutOff_sens:" "0"              
			[1] "error rate sens: 0.5"
			[1] "yhat.transformed_center sens matrix"
			[1] "n:"   "4980"
				1
			0 0.5
			1 0.5
			[1] "optCutOff_center" "1"               
			[1] "error rate c: 0.4159"
			[1] "yhat.transformed_center conf matrix"
			[1] "n:"   "4980"
				   0      1
			0 0.4257 0.0743
			1 0.3416 0.1584
			[1] "optCutOff_spec" "0.01"          
			[1] "error rate spec: 0.4159"
			[1] "yhat.transformed_spec conf matrix"
			[1] "n:"   "4980"
				   0      1
			0 0.4257 0.0743
			1 0.3416 0.1584
			[1] "MC model applied to Pop :" "0.634457431104765"        
			[1] "Pop model applied to pop :" "0.646058282469799"         
			[1] "CV Model applied to population"

			Call:
			glm(formula = holderOfData, family = binomial(link = "logit"))

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-2.0546  -0.3351  -0.3020  -0.3020   2.4941  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -3.06467    0.08879 -34.517  < 2e-16 ***
			V8512        0.77495    0.12557   6.172 6.76e-10 ***
			V8509        3.80169    0.12489  30.441  < 2e-16 ***
			V8530        0.25677    0.14231   1.804   0.0712 .  
			V7215        0.21286    0.10867   1.959   0.0502 .  
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 3846.6  on 4978  degrees of freedom
			Residual deviance: 2570.2  on 4974  degrees of freedom
			AIC: 2580.2

			Number of Fisher Scoring iterations: 5

				 V7118            V8502            V8512            V8509             V8530            V7215       
			 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
			 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000  
			 Median :0.0000   Median :0.0000   Median :0.0000   Median :0.00000   Median :0.0000   Median :0.0000  
			 Mean   :0.4508   Mean   :0.1234   Mean   :0.2038   Mean   :0.09284   Mean   :0.1352   Mean   :0.4111  
			 3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:1.0000  
			 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000  
			[1] "C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577//output/V7221-greaterEqual-10-final.csv"
			[1] "y:"    "V7221"
			[[1]]
			[1] "final: "

			$tabulatedCrossValidated
			 [1] <NA>  V8526 V7215 V8527 V8512 V8509 V8528 V7202 V8530 V7206 V8502 V8505 V8514 V8529 V7562 V8565 V7553 V7551 V7552 V8536 V7563
			Levels: V7202 V7206 V7215 V7551 V7552 V7553 V7562 V7563 V8502 V8505 V8509 V8512 V8514 V8526 V8527 V8528 V8529 V8530 V8536 V8565

			$Freq
			 [1] 2.067 0.600 0.567 0.550 0.450 0.433 0.417 0.367 0.367 0.350 0.317 0.250 0.250 0.250 0.233 0.183 0.083 0.050 0.050 0.050 0.017

			   tabulatedCrossValidated  Freq
			2                    V8526 0.600
			3                    V7215 0.567
			4                    V8527 0.550
			5                    V8512 0.450
			6                    V8509 0.433
			7                    V8528 0.417
			8                    V7202 0.367
			9                    V8530 0.367
			10                   V7206 0.350
			11                   V8502 0.317
			12                   V8505 0.250
			13                   V8514 0.250
			14                   V8529 0.250
			15                   V7562 0.233
			16                   V8565 0.183
			17                   V7553 0.083
			18                   V7551 0.050
			19                   V7552 0.050
			20                   V8536 0.050
			21                   V7563 0.017
			 [1] "keep: > " "0.25"     "10"       "V8526"    "V7215"    "V8527"    "V8512"    "V8509"    "V8528"    "V7202"    "V8530"    "V7206"    "V8502"   
				 V8526      V7215      V8509      V7202      V7206 
			 0.4628940  0.4800828 -0.4572628  1.1307374  0.5737264 
				 V7221         V8526            V7215            V8527            V8512            V8509             V8528            V7202            V8530            V7206            V8502       
			 Min.   :0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
			 1st Qu.:0.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
			 Median :0.5   Median :0.0000   Median :1.0000   Median :0.0000   Median :0.0000   Median :0.00000   Median :0.0000   Median :1.0000   Median :0.0000   Median :1.0000   Median :0.0000  
			 Mean   :0.5   Mean   :0.1468   Mean   :0.5186   Mean   :0.1816   Mean   :0.2809   Mean   :0.08137   Mean   :0.2247   Mean   :0.9141   Mean   :0.1683   Mean   :0.7074   Mean   :0.1146  
			 3rd Qu.:1.0   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
			 Max.   :1.0   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  

			Call:
			imcdiag(x = x, y = y, method = method, corr = FALSE, vif = vif, 
				tol = tol, conf = conf, cvif = cvif, leamer = leamer, all = all)


			 VIF Multicollinearity Diagnostics

					 VIF detection
			V8526 1.9812         0
			V7215 1.0284         0
			V8527 2.8743         0
			V8512 1.3416         0
			V8509 1.4923         0
			V8528 3.3344         0
			V7202 1.1240         0
			V8530 2.0541         0
			V7206 1.1066         0
			V8502 1.5148         0

			NOTE:  VIF Method Failed to detect multicollinearity


			0 --> COLLINEARITY is not detected by the test

			===================================
			[1] "MC summary"

			Call:
			glm(formula = y ~ ., family = family, data = data.frame(Xy[, 
				c(bestset[-1], FALSE), drop = FALSE], y = y))

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.5805  -1.1639   0.1812   1.0379   2.1445  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -1.73639    0.03818  -45.48   <2e-16 ***
			V8526        0.46289    0.02544   18.20   <2e-16 ***
			V7215        0.48008    0.01775   27.04   <2e-16 ***
			V8509       -0.45726    0.03263  -14.02   <2e-16 ***
			V7202        1.13074    0.03901   28.99   <2e-16 ***
			V7206        0.57373    0.02030   28.26   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 76634  on 55279  degrees of freedom
			Residual deviance: 72472  on 55274  degrees of freedom
			AIC: 72484

			Number of Fisher Scoring iterations: 4

			[1] "optCutOff_sens:" "0"              
			[1] "error rate sens: 0.5"
			[1] "yhat.transformed_center sens matrix"
			[1] "n:"    "55280"
				1
			0 0.5
			1 0.5
			[1] "optCutOff_center" "1"               
			[1] "error rate c: 0.399"
			[1] "yhat.transformed_center conf matrix"
			[1] "n:"    "55280"
				   0      1
			0 0.3482 0.1518
			1 0.2472 0.2528
			[1] "optCutOff_spec" "0.01"          
			[1] "error rate spec: 0.399"
			[1] "yhat.transformed_spec conf matrix"
			[1] "n:"    "55280"
				   0      1
			0 0.3482 0.1518
			1 0.2472 0.2528
			[1] "MC model applied to Pop :" "0.64510140728372"         
			[1] "Pop model applied to pop :" "0.760487828258346"         
			[1] "CV Model applied to population"

			Call:
			glm(formula = holderOfData, family = binomial(link = "logit"))

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-0.8397  -0.6062  -0.5256  -0.4543   2.2228  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -2.38210    0.04933 -48.292  < 2e-16 ***
			V7215        0.30891    0.02471  12.503  < 2e-16 ***
			V8509        0.73996    0.03699  20.004  < 2e-16 ***
			V7202        0.16294    0.05126   3.179  0.00148 ** 
			V7206        0.30926    0.02928  10.561  < 2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 46112  on 55278  degrees of freedom
			Residual deviance: 45406  on 55274  degrees of freedom
			AIC: 45416

			Number of Fisher Scoring iterations: 4

				 V7221            V8526            V7215           V8509             V7202            V7206       
			 Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
			 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.00000   1st Qu.:1.0000   1st Qu.:0.0000  
			 Median :1.0000   Median :0.0000   Median :1.000   Median :0.00000   Median :1.0000   Median :1.0000  
			 Mean   :0.5783   Mean   :0.1506   Mean   :0.533   Mean   :0.07944   Mean   :0.9209   Mean   :0.7226  
			 3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000  
			 Max.   :1.0000   Max.   :1.0000   Max.   :1.000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000  
			[1] "C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577//output/V8517-greaterEqual-10-final.csv"
			[1] "y:"    "V8517"
			[[1]]
			[1] "final: "

			$tabulatedCrossValidated
			 [1] <NA>  V8502 V8509 V8527 V8536 V8512 V7206 V7552 V8514 V8526 V8530 V8505 V7215 V8528 V8565 V8529 V8531 V7202
			Levels: V7202 V7206 V7215 V7552 V8502 V8505 V8509 V8512 V8514 V8526 V8527 V8528 V8529 V8530 V8531 V8536 V8565

			$Freq
			 [1] 2.317 0.617 0.533 0.433 0.333 0.317 0.300 0.267 0.267 0.267 0.250 0.233 0.217 0.150 0.100 0.067 0.067 0.017

			   tabulatedCrossValidated  Freq
			2                    V8502 0.617
			3                    V8509 0.533
			4                    V8527 0.433
			5                    V8536 0.333
			6                    V8512 0.317
			7                    V7206 0.300
			8                    V7552 0.267
			9                    V8514 0.267
			10                   V8526 0.267
			11                   V8530 0.250
			12                   V8505 0.233
			13                   V7215 0.217
			14                   V8528 0.150
			15                   V8565 0.100
			16                   V8529 0.067
			17                   V8531 0.067
			18                   V7202 0.017
			 [1] "keep: > " "0.25"     "9"        "V8502"    "V8509"    "V8527"    "V8536"    "V8512"    "V7206"    "V7552"    "V8514"    "V8526"   
				 V8502      V8527      V8536      V7206      V7552      V8514      V8526 
			 2.0156523  5.3566775  4.9321064 -0.7355915  1.1383145  1.8866605  4.9423597 
				 V8517         V8502            V8509            V8527            V8536             V8512            V7206            V7552            V8514            V8526       
			 Min.   :0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
			 1st Qu.:0.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
			 Median :0.5   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.00000   Median :0.0000   Median :1.0000   Median :1.0000   Median :0.0000   Median :0.0000  
			 Mean   :0.5   Mean   :0.1825   Mean   :0.1406   Mean   :0.2702   Mean   :0.04404   Mean   :0.3733   Mean   :0.7207   Mean   :0.6955   Mean   :0.4462   Mean   :0.2443  
			 3rd Qu.:1.0   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
			 Max.   :1.0   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  

			Call:
			imcdiag(x = x, y = y, method = method, corr = FALSE, vif = vif, 
				tol = tol, conf = conf, cvif = cvif, leamer = leamer, all = all)


			 VIF Multicollinearity Diagnostics

					 VIF detection
			V8502 1.4815         0
			V8509 1.4874         0
			V8527 1.5427         0
			V8536 1.1356         0
			V8512 2.4656         0
			V7206 1.0040         0
			V7552 1.1440         0
			V8514 2.6250         0
			V8526 1.5116         0

			NOTE:  VIF Method Failed to detect multicollinearity


			0 --> COLLINEARITY is not detected by the test

			===================================
			[1] "MC summary"

			Call:
			glm(formula = y ~ ., family = family, data = data.frame(Xy[, 
				c(bestset[-1], FALSE), drop = FALSE], y = y))

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-5.0246  -0.4343  -0.1248   0.1200   2.6394  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -2.71647    0.11031 -24.626   <2e-16 ***
			V8502        2.01565    0.10795  18.671   <2e-16 ***
			V8527        5.35668    0.34380  15.581   <2e-16 ***
			V8536        4.93211    0.59758   8.254   <2e-16 ***
			V7206       -0.73559    0.08648  -8.506   <2e-16 ***
			V7552        1.13831    0.10437  10.906   <2e-16 ***
			V8514        1.88666    0.08269  22.817   <2e-16 ***
			V8526        4.94236    0.32896  15.024   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 11173.5  on 8059  degrees of freedom
			Residual deviance:  4032.3  on 8052  degrees of freedom
			AIC: 4048.3

			Number of Fisher Scoring iterations: 8

			[1] "optCutOff_sens:" "0"              
			[1] "error rate sens: 0.5"
			[1] "yhat.transformed_center sens matrix"
			[1] "n:"   "8060"
				1
			0 0.5
			1 0.5
			[1] "optCutOff_center" "1"               
			[1] "error rate c: 0.1119"
			[1] "yhat.transformed_center conf matrix"
			[1] "n:"   "8060"
				   0      1
			0 0.4697 0.0303
			1 0.0816 0.4184
			[1] "optCutOff_spec" "0.01"          
			[1] "error rate spec: 0.1119"
			[1] "yhat.transformed_spec conf matrix"
			[1] "n:"   "8060"
				   0      1
			0 0.4697 0.0303
			1 0.0816 0.4184
			[1] "MC model applied to Pop :" "0.252417878296369"        
			[1] "Pop model applied to pop :" "0.232831548837981"         
			[1] "CV Model applied to population"

			Call:
			glm(formula = holderOfData, family = binomial(link = "logit"))

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.8324  -0.5996  -0.5696  -0.3735   2.3226  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -2.51605    0.08985 -28.002  < 2e-16 ***
			V8527        0.62375    0.07715   8.085 6.24e-16 ***
			V8536        2.35310    0.12190  19.304  < 2e-16 ***
			V7206       -0.11147    0.06745  -1.653   0.0984 .  
			V7552        0.89111    0.08310  10.724  < 2e-16 ***
			V8514        0.09686    0.06843   1.416   0.1569    
			V8526        0.02348    0.07972   0.294   0.7684    
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 7656.4  on 8058  degrees of freedom
			Residual deviance: 6892.6  on 8052  degrees of freedom
			AIC: 6906.6

			Number of Fisher Scoring iterations: 5

				 V8517             V8502             V8527             V8536              V7206          V7552            V8514            V8526        
			 Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.000000   Min.   :0.00   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
			 1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.00   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
			 Median :0.00000   Median :0.00000   Median :0.00000   Median :0.000000   Median :1.00   Median :1.0000   Median :0.0000   Median :0.00000  
			 Mean   :0.05849   Mean   :0.06342   Mean   :0.03464   Mean   :0.005246   Mean   :0.74   Mean   :0.5613   Mean   :0.2039   Mean   :0.03149  
			 3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:1.00   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.00000  
			 Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.000000   Max.   :1.00   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000

		Milestone 16 
			Proportional symmetrical class balancing achieved via stratified resampling 235e221 - abb5754
			reloop factor: min(%[nrow(ones),nrow(zeros)] / avg(%[nrow(ones),nrow(zeros)] 
			threshold set to .35
			rebalanced classes means cutoff thresholds of .5 for confusion matrix
			switch to modified bestglm for saveCSVs due to not converging on significant terms or any terms with monte carlo resample
			
			is the error that is captured by bestglm when outputing a timestampped csv.  This is returned most likely due to a magnified monte carlo sample that is too large.  I suggest a recursive loop that resamples at half the size until it returns no error.
				It's how I resolved saveCSVs.R monte carlo subsample. (separate file had same issue with bestglm)
				note: Error in qk[, 4] : incorrect number of dimensions
			
			[1] "C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577//output/V7118-greaterEqual-10-final.csv"
			[1] "y:"    "V7118"
			[[1]]
			[1] "final: "

			$tabulatedCrossValidated
			 [1] <NA>  V8514 V8502 V8530 V8512 V7562 V8536 V8526 V8505 V8509 V8531 V7215 V7206 V7552 V8528 V7202 V8527 V8565 V7551 V7563 V8529
			Levels: V7202 V7206 V7215 V7551 V7552 V7562 V7563 V8502 V8505 V8509 V8512 V8514 V8526 V8527 V8528 V8529 V8530 V8531 V8536 V8565

			$Freq
			 [1] 3.683 0.450 0.417 0.383 0.367 0.333 0.333 0.300 0.267 0.267 0.233 0.217 0.183 0.150 0.150 0.133 0.067 0.067 0.033 0.033 0.017

			   tabulatedCrossValidated  Freq
			2                    V8514 0.450
			3                    V8502 0.417
			4                    V8530 0.383
			5                    V8512 0.367
			6                    V7562 0.333
			7                    V8536 0.333
			8                    V8526 0.300
			9                    V8505 0.267
			10                   V8509 0.267
			11                   V8531 0.233
			12                   V7215 0.217
			13                   V7206 0.183
			14                   V7552 0.150
			15                   V8528 0.150
			16                   V7202 0.133
			17                   V8527 0.067
			18                   V8565 0.067
			19                   V7551 0.033
			20                   V7563 0.033
			21                   V8529 0.017
			[1] "keep: > " "0.35"     "4"        "V8514"    "V8502"    "V8530"    "V8512"   
				 V7118            V8514            V8502            V8530           V8512       
			 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
			 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000  
			 Median :1.0000   Median :1.0000   Median :0.0000   Median :1.000   Median :1.0000  
			 Mean   :0.5287   Mean   :0.7399   Mean   :0.3175   Mean   :0.558   Mean   :0.6545  
			 3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:1.0000  
			 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000   Max.   :1.0000  

			Call:
			imcdiag(x = x, y = y, method = method, corr = FALSE, vif = vif, 
				tol = tol, conf = conf, cvif = cvif, leamer = leamer, all = all)


			 VIF Multicollinearity Diagnostics

					 VIF detection
			V8514 1.7782         0
			V8502 1.3797         0
			V8530 1.1458         0
			V8512 1.6401         0

			NOTE:  VIF Method Failed to detect multicollinearity


			0 --> COLLINEARITY is not detected by the test

			===================================
			[1] "MC summary"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.7659  -0.9757   0.6872   1.0092   1.3936  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept)   1.3232     0.1800   7.351 1.96e-13 ***
			V8514        -0.9138     0.1974  -4.630 3.66e-06 ***
			V8530        -0.9044     0.1654  -5.469 4.52e-08 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 988.85  on 714  degrees of freedom
			Residual deviance: 913.29  on 712  degrees of freedom
			AIC: 919.29

			Number of Fisher Scoring iterations: 4

			[1] "optCutOff_sens:" "0"              
			[1] "error rate sens: 0.4713"
			[1] "yhat.transformed_sens matrix"
			[1] "n:"  "715"
				   1
			0 0.4713
			1 0.5287
			[1] "optCutOff_center" "0.6"             
			[1] "error rate c1: 0.351"
			[1] "yhat.transformed_center matrix"
				   0      1
			0 0.3021 0.1692
			1 0.1818 0.3469
			[1] "optCutOff_cen" "0.5"          
			[1] "error rate c2: 0.351"
			[1] "yhat.transformed_cen matrix"
				   0      1
			0 0.3021 0.1692
			1 0.1818 0.3469
			[1] "optCutOff_top" "0.99"         
			[1] "error rate top: 0.3524"
			[1] "yhat.transformed_top matrix"
				   0      1
			0 0.3021 0.1692
			1 0.1832 0.3455
			[1] "optCutOff_spec" "0.01"          
			[1] "error rate spec: 0.351"
			[1] "yhat.transformed_spec matrix"
				   0      1
			0 0.3021 0.1692
			1 0.1818 0.3469
			[1] "MC model applied to pop:" "0.653"                   
			[1] "Conf matrix: MC CV (robust) model applied to pop data"
				   0      1
			0 0.5568 0.4168
			1 0.0097 0.0168
			[1] "Pop model applied to pop:" "0.1628"                   
			[1] "Conf Matrix: Pop CV (overfitted) model applied to pop data"
				   0
			0 0.9735
			1 0.0265
			[1] "Summary: CV Model applied to population"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-0.3552  -0.2490  -0.1843  -0.1843   2.8581  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -2.73147    0.05010  -54.52   <2e-16 ***
			V8514       -0.72655    0.06045  -12.02   <2e-16 ***
			V8530       -0.60925    0.05812  -10.48   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 12117  on 49525  degrees of freedom
			Residual deviance: 11796  on 49523  degrees of freedom
			AIC: 11802

			Number of Fisher Scoring iterations: 6

				 V7118             V8514            V8530      
			 Min.   :0.00000   Min.   :0.0000   Min.   :0.000  
			 1st Qu.:0.00000   1st Qu.:1.0000   1st Qu.:0.000  
			 Median :0.00000   Median :1.0000   Median :1.000  
			 Mean   :0.02649   Mean   :0.8023   Mean   :0.653  
			 3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.000  
			 Max.   :1.00000   Max.   :1.0000   Max.   :1.000  
			[1] "C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577//output/V7221-greaterEqual-10-final.csv"
			[1] "y:"    "V7221"
			[[1]]
			[1] "final: "

			$tabulatedCrossValidated
			 [1] <NA>  V7215 V8526 V8512 V8527 V7206 V7202 V8502 V8509 V8528 V8514 V8530 V8529 V7562 V8565 V7552 V8505 V7553 V8536 V7551 V7563 V8531
			Levels: V7202 V7206 V7215 V7551 V7552 V7553 V7562 V7563 V8502 V8505 V8509 V8512 V8514 V8526 V8527 V8528 V8529 V8530 V8531 V8536 V8565

			$Freq
			 [1] 1.717 0.667 0.667 0.583 0.583 0.533 0.467 0.433 0.433 0.300 0.267 0.217 0.200 0.183 0.167 0.133 0.133 0.067 0.050 0.033 0.033 0.017

			   tabulatedCrossValidated  Freq
			2                    V7215 0.667
			3                    V8526 0.667
			4                    V8512 0.583
			5                    V8527 0.583
			6                    V7206 0.533
			7                    V7202 0.467
			8                    V8502 0.433
			9                    V8509 0.433
			10                   V8528 0.300
			11                   V8514 0.267
			12                   V8530 0.217
			13                   V8529 0.200
			14                   V7562 0.183
			15                   V8565 0.167
			16                   V7552 0.133
			17                   V8505 0.133
			18                   V7553 0.067
			19                   V8536 0.050
			20                   V7551 0.033
			21                   V7563 0.033
			22                   V8531 0.017
			 [1] "keep: > " "0.35"     "8"        "V7215"    "V8526"    "V8512"    "V8527"    "V7206"    "V7202"    "V8502"    "V8509"   
				 V7221            V7215            V8526            V8512            V8527            V7206            V7202            V8502            V8509       
			 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
			 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
			 Median :0.0000   Median :1.0000   Median :1.0000   Median :1.0000   Median :1.0000   Median :1.0000   Median :1.0000   Median :0.0000   Median :0.0000  
			 Mean   :0.4927   Mean   :0.5309   Mean   :0.5747   Mean   :0.6905   Mean   :0.6499   Mean   :0.7793   Mean   :0.5015   Mean   :0.2204   Mean   :0.1685  
			 3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
			 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  

			Call:
			imcdiag(x = x, y = y, method = method, corr = FALSE, vif = vif, 
				tol = tol, conf = conf, cvif = cvif, leamer = leamer, all = all)


			 VIF Multicollinearity Diagnostics

					 VIF detection
			V7215 1.0103         0
			V8526 1.1592         0
			V8512 1.2080         0
			V8527 1.1068         0
			V7206 1.0146         0
			V7202 1.0327         0
			V8502 1.4466         0
			V8509 1.3873         0

			NOTE:  VIF Method Failed to detect multicollinearity


			0 --> COLLINEARITY is not detected by the test

			===================================
			[1] "MC summary"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.5954  -1.1085  -0.5938   1.0875   1.9092  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.91729    0.05117  -17.93   <2e-16 ***
			V7215        0.50717    0.04418   11.48   <2e-16 ***
			V8526        0.75312    0.04595   16.39   <2e-16 ***
			V7202        0.60106    0.04525   13.28   <2e-16 ***
			V8509       -0.72897    0.06178  -11.80   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 12437  on 8972  degrees of freedom
			Residual deviance: 11697  on 8968  degrees of freedom
			AIC: 11707

			Number of Fisher Scoring iterations: 4

			[1] "optCutOff_sens:" "0"              
			[1] "error rate sens: 0.5073"
			[1] "yhat.transformed_sens matrix"
			[1] "n:"   "8973"
				   1
			0 0.5073
			1 0.4927
			[1] "optCutOff_center" "0.54"            
			[1] "error rate c1: 0.3772"
			[1] "yhat.transformed_center matrix"
				   0      1
			0 0.3212 0.1861
			1 0.1911 0.3016
			[1] "optCutOff_cen" "0.5"          
			[1] "error rate c2: 0.3772"
			[1] "yhat.transformed_cen matrix"
				   0      1
			0 0.3212 0.1861
			1 0.1911 0.3016
			[1] "optCutOff_top" "0.99"         
			[1] "error rate top: 0.3786"
			[1] "yhat.transformed_top matrix"
				   0      1
			0 0.3255 0.1818
			1 0.1968 0.2959
			[1] "optCutOff_spec" "0.72"          
			[1] "error rate spec: 0.3786"
			[1] "yhat.transformed_spec matrix"
				   0      1
			0 0.3255 0.1818
			1 0.1968 0.2959
			[1] "MC model applied to pop:" "0.6215"                  
			[1] "Conf matrix: MC CV (robust) model applied to pop data"
				   0      1
			0 0.2736 0.1659
			1 0.2204 0.3402
			[1] "Pop model applied to pop:" "0.6222"                   
			[1] "Conf Matrix: Pop CV (overfitted) model applied to pop data"
				   0      1
			0 0.1559 0.2835
			1 0.1036 0.4570
			[1] "Summary: CV Model applied to population"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.6811  -1.1836   0.7469   0.9732   1.7186  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.57664    0.02157  -26.73   <2e-16 ***
			V7215        0.48683    0.01896   25.67   <2e-16 ***
			V8526        0.63268    0.01958   32.31   <2e-16 ***
			V7202        0.59120    0.01932   30.61   <2e-16 ***
			V8509       -0.64080    0.02578  -24.86   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 66786  on 48692  degrees of freedom
			Residual deviance: 63478  on 48688  degrees of freedom
			AIC: 63488

			Number of Fisher Scoring iterations: 4

				 V7221            V7215            V8526            V7202            V8509       
			 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
			 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
			 Median :1.0000   Median :1.0000   Median :1.0000   Median :1.0000   Median :0.0000  
			 Mean   :0.5606   Mean   :0.5366   Mean   :0.5901   Mean   :0.5141   Mean   :0.1627  
			 3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
			 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
			[1] "C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577//output/V8517-greaterEqual-10-final.csv"
			[1] "y:"    "V8517"
			[[1]]
			[1] "final: "

			$tabulatedCrossValidated
			 [1] <NA>  V8502 V8509 V8527 V7552 V8512 V8526 V8530 V8514 V8536 V7206 V8505 V7215 V8528 V8565 V8531 V7202
			Levels: V7202 V7206 V7215 V7552 V8502 V8505 V8509 V8512 V8514 V8526 V8527 V8528 V8530 V8531 V8536 V8565

			$Freq
			 [1] 1.683 0.717 0.600 0.533 0.417 0.400 0.400 0.400 0.333 0.283 0.267 0.233 0.217 0.150 0.083 0.067 0.033

			   tabulatedCrossValidated  Freq
			2                    V8502 0.717
			3                    V8509 0.600
			4                    V8527 0.533
			5                    V7552 0.417
			6                    V8512 0.400
			7                    V8526 0.400
			8                    V8530 0.400
			9                    V8514 0.333
			10                   V8536 0.283
			11                   V7206 0.267
			12                   V8505 0.233
			13                   V7215 0.217
			14                   V8528 0.150
			15                   V8565 0.083
			16                   V8531 0.067
			17                   V7202 0.033
			 [1] "keep: > " "0.35"     "7"        "V8502"    "V8509"    "V8527"    "V7552"    "V8512"    "V8526"    "V8530"   
				 V8517            V8502            V8509            V8527            V7552            V8512           V8526            V8530       
			 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000  
			 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000  
			 Median :0.0000   Median :0.0000   Median :0.0000   Median :1.0000   Median :1.0000   Median :1.000   Median :1.0000   Median :1.0000  
			 Mean   :0.4845   Mean   :0.2624   Mean   :0.2111   Mean   :0.6334   Mean   :0.7676   Mean   :0.669   Mean   :0.5575   Mean   :0.6088  
			 3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:1.0000   3rd Qu.:1.0000  
			 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000  

			Call:
			imcdiag(x = x, y = y, method = method, corr = FALSE, vif = vif, 
				tol = tol, conf = conf, cvif = cvif, leamer = leamer, all = all)


			 VIF Multicollinearity Diagnostics

					 VIF detection
			V8502 1.4496         0
			V8509 1.3904         0
			V8527 1.1311         0
			V7552 1.0062         0
			V8512 1.1961         0
			V8526 1.2297         0
			V8530 1.1676         0

			NOTE:  VIF Method Failed to detect multicollinearity


			0 --> COLLINEARITY is not detected by the test

			===================================
			[1] "MC summary"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-1.6179  -1.0328  -0.9052   1.1497   1.4767  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -0.26491    0.08363  -3.168  0.00154 ** 
			V8502        0.36958    0.08541   4.327 1.51e-05 ***
			V8509        0.55875    0.09261   6.033 1.61e-09 ***
			V8527       -0.41574    0.06710  -6.196 5.79e-10 ***
			V7552        0.33046    0.07661   4.313 1.61e-05 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 5638.3  on 4069  degrees of freedom
			Residual deviance: 5446.9  on 4065  degrees of freedom
			AIC: 5456.9

			Number of Fisher Scoring iterations: 4

			[1] "optCutOff_sens:" "0"              
			[1] "error rate sens: 0.5155"
			[1] "yhat.transformed_sens matrix"
			[1] "n:"   "4070"
				   1
			0 0.5155
			1 0.4845
			[1] "optCutOff_center" "0.52"            
			[1] "error rate c1: 0.4027"
			[1] "yhat.transformed_center matrix"
				   0      1
			0 0.3248 0.1907
			1 0.2120 0.2725
			[1] "optCutOff_cen" "0.5"          
			[1] "error rate c2: 0.4034"
			[1] "yhat.transformed_cen matrix"
				   0      1
			0 0.3226 0.1929
			1 0.2106 0.2740
			[1] "optCutOff_top" "0.99"         
			[1] "error rate top: 0.4047"
			[1] "yhat.transformed_top matrix"
				   0      1
			0 0.3253 0.1902
			1 0.2145 0.2700
			[1] "optCutOff_spec" "0.73"          
			[1] "error rate spec: 0.4047"
			[1] "yhat.transformed_spec matrix"
				   0      1
			0 0.3253 0.1902
			1 0.2145 0.2700
			[1] "MC model applied to pop:" "0.6275"                  
			[1] "Conf matrix: MC CV (robust) model applied to pop data"
				   0      1
			0 0.5207 0.3294
			1 0.0644 0.0855
			[1] "Pop model applied to pop:" "0.3872"                   
			[1] "Conf Matrix: Pop CV (overfitted) model applied to pop data"
				   0
			0 0.8501
			1 0.1499
			[1] "Summary: CV Model applied to population"

			Call:
			NULL

			Deviance Residuals: 
				Min       1Q   Median       3Q      Max  
			-0.8665  -0.6103  -0.5115  -0.4172   2.2292  

			Coefficients:
						Estimate Std. Error z value Pr(>|z|)    
			(Intercept) -2.01608    0.03397 -59.353   <2e-16 ***
			V8502        0.45438    0.03310  13.729   <2e-16 ***
			V8509        0.34564    0.03591   9.626   <2e-16 ***
			V8527       -0.38156    0.02586 -14.756   <2e-16 ***
			V7552        0.42976    0.03183  13.502   <2e-16 ***
			---
			Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			(Dispersion parameter for binomial family taken to be 1)

				Null deviance: 42400  on 50171  degrees of freedom
			Residual deviance: 41286  on 50167  degrees of freedom
			AIC: 41296

			Number of Fisher Scoring iterations: 4

				 V8517            V8502            V8509            V8527            V7552      
			 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
			 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000  
			 Median :0.0000   Median :0.0000   Median :0.0000   Median :1.0000   Median :1.000  
			 Mean   :0.1499   Mean   :0.2151   Mean   :0.1639   Mean   :0.6571   Mean   :0.744  
			 3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.000  
			 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000
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

	GLM and PCA
		https://stats.stackexchange.com/questions/22665/how-to-use-principal-components-as-predictors-in-glm

	http://www.et.bs.ehu.es/cran/web/packages/bestglm/vignettes/bestglm.pdf
	
	CV (2009?)
	
	Measures of central tendency (Median)
	
	CI's 
		all these assume different center's... yet... I'm using a median... I should be using mean and testing if the mean is different (to test if population is statistically different)
		
		https://www.apastyle.org/manual/related/cumming-and-finch.pdf
		
		https://towardsdatascience.com/why-overlapping-confidence-intervals-mean-nothing-about-statistical-significance-48360559900a
			all our median's are 0 difference, so according to this, our populations are the same (maybe we should test for this with a t-test or anova?)
	
		https://www.cscu.cornell.edu/news/statnews/stnews73.pdf
		
		https://blog.minitab.com/blog/real-world-quality-improvement/common-statistical-mistakes-you-should-avoid
	
	Resampling / simulation methods: monte carlo, bootstrapping, jackknifing, cross-validation, randomization tests, and permutation tests
	
		https://stats.stackexchange.com/questions/104040/resampling-simulation-methods-monte-carlo-bootstrapping-jackknifing-cross
		https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
	
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
				#https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/
				goal is a model with a high sensitivity
				
				which plots the false positive rate (FPR) on the x-axis and the true positive rate (TPR) on the y-axis:
				
		
		Lift charts
			p 125-126			
		
				case can be assigned to the class with the highest probability.  In many cases, a single class is of special interest, so we will focus on that particular lcass and compare the propensity of belonging to that class to ac utoff value set by the analyst.... If hte probability of belonging to the lcass of interest is above the cutoff, the case is assigned to that class.

				i.e. if holdout/test value is above a preset threshold (based on prior model result propensity threshold set to achieve optimal class specification) assign 1, else 0.		
			
		Decile charts
			p 127			
			
		Oversampling (nice to have)
		
			p 129-134	
				adjust confusion matrix for oversampling.
					I need to base my matrix, and possibly formula on the oversampled version.  Well, the matrix is based on the oversample but the proportions are based on the population.
			BIAS
			sampling at a greater proportion than it exists in the population
			http://r-statistics.co/Logistic-Regression-With-R.html

			
			to deal with na's, we could have oversampled cases...
	
	Assessment
		https://datascienceplus.com/perform-logistic-regression-in-r/
		While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
	
	Applied Regression Modelling
		104-109
			nested model tests (anova) similar to t-tests. basically testing if there is a need to include the coefficient term
			we don't need to do this
	
		p 150
			natural log for response (using a quantitative example)
			
		p 206
			check collinearity
			cross validation kind of takes care of this
			"if any of the (absolute) correlations between each pair of predictors is greater than the highest (absolute) correlation between Y and each of the predictors."
			
		Page 268-273 binary logistic regression
		
			p 269
				Model Assumptions
				https://www.statisticssolutions.com/assumptions-of-logistic-regression/
				
				First, binary logistic regression requires the dependent variable to be binary and ordinal logistic regression requires the dependent variable to be ordinal.

				Second, logistic regression requires the observations to be independent of each other.  In other words, the observations should not come from repeated measurements or matched data.

				Third, logistic regression requires there to be little or no multicollinearity among the independent variables.  This means that the independent variables should not be too highly correlated with each other.

				Fourth, logistic regression assumes linearity of independent variables and log odds.  although this analysis does not require the dependent and independent variables to be related linearly, it requires that the independent variables are linearly related to the log odds.

				Finally, logistic regression typically requires a large sample size.  A general guideline is that you need at minimum of 10 cases with the least frequent outcome for each independent variable in your model. For example, if you have 5 independent variables and the expected probability of your least frequent outcome is .10, then you would need a minimum sample size of 500 (10*5 / .10).
			
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
						
						Chapter 6 of Dr Zerom's 526 class text on "Time Series Forecasting with Regressive Models" p 19
							CHIINV(sig,df)
							
							Related to Ljung-Box statistic apparently via Chi Squared distribution
							i.e. statistic is Chi-Squared distributed with the delta in k as the degrees of freedom
					
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
	
	Oversampling
		Due to bias in class, we did not oversample.  However, we did not build models based on oversampling.  We derived terms from CV models that worked best in randomized datasets.  The system would improve with equalized models, but arguably not by much because we did not use a model based approach.
	
Appendix
	