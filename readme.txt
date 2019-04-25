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
		10
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
				
		5
			[1] "Y: V7221"
				V7563 V8514 
				 1.00  0.56 
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
				
			[1] "Y: V8517"
				V7563 V8514 
				 1.00  0.68 
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

			[1] "Y: V7118"
				V7563 V8514 
				 1.00  0.68 
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
				
		3
		[1] "Y: V7221"
			V7507     V7563     V8502     V8514     V8530 
			0.2222222 1.0000000 0.1111111 0.4444444 0.1111111 
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
			
		[1] "Y: V8517"
			V7507     V7563     V8514 
			0.2222222 1.0000000 0.7777778 
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
			
		[1] "Y: V7118"
			V7507     V7563     V8514 
			0.1111111 1.0000000 0.6666667 
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
			p 127-134	
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
	