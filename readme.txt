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
		
Hypothesis
		at the 5% sample level using the aggregate data of the subset of the holdout.  We will derive a regression equation there.  Then we will check it against the population and see if it was within range.  If so, we will confirm our hypothesis.  As an addendum, we can derive a population level... this part I think I'd like to ask the professor about.
		
		V7118profile <- c("V7118","V7202","V7551","V8502")   
		eventually reduced to ("V7118, "V7202") (after holdout analysis)

		so... I can derive a regression equation at the holdout analysis juncture, and then see if it holds when I derive a newdataset of the population and derive it's new coefficient terms fitting a model.  See if it's terms fit within the range predicted at the holdout analysis level.  At these junctures, the datasets are aggregated independently (different sampling due to inclusion of different factors, i.e. na drops records based on what columns are included)
		
		Feature extractor will extract features that should display in random samples as significant.
		
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

		Violence (4) (always excluded from substances cv results)
			V8517,1,"FRQ GANG FIGHT",0

		Father1 (5) (always excluded from substances cv results)
			V7206,0,"R'S HSHLD FATHER",0

		Father2 (6  (always excluded from substances cv results), Median is 5, college graduate
			V7215,2,"FATHR EDUC LEVEL",0
					
		Habits (7) (always excluded from substances cv results)
			V7551,2,"#HR/W INTERNET S",0
			V7552,2,"DALY WEB FACEBK",0
			V7553,2,"#HR GAMING",0
			V7562,2,"#HR TEXT",0 
			V7563,2,"#HR TALK CELL",0

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
			
I realized I never did a proper holdout analysis.  So I revisited the drawing board and basically

I would higlight each subdomain using bestglm, example
#training, bestglm would result in each category
	V7118profile <- c("V7118",
	#gender
	"V7202",
	#habits
	"V7551","V7552",
	#health
	"V8526","V8527",
	#psych
	"V8502","V7507")

	upon a new bstglm would reduce to 
	V7118profile <- c("V7118","V7202","V7551","V8502")  

	however upon holdout analyisis, only V7202 was significant.  
	
	So... I revisited my holdout analysis approach and fed V7118profile <- c("V7118","V7202","V7551","V7552","V8526","V8527","V8502","V7507") into 
	
	bestglm and only 
	
	V7202 came out.  
	
	I then fed this into the population and V7202 accounts for a classification matrix increase of +12.3% (from 50%).  With the inclusion of V7551 and V8502, the increase is +20%.  Which tells me the inclusion of V7551 and V8502 account for 8% of the deviation however, they are not present in the holdout sample (in this).  Which tells me that each sample should highlight difference presences of features that exist in the population apart from the bigger elements like gender (V7202).  Something to that affect.

V8517profile <- c("V8517","V7551","V8530","V8531","V8514","V8505")
	reduced after holdout to the same as before
	"V8517" ~ "V8530" "V8531"  

V7221profile <- c("V7221","V7552","V7562","V7563","V8527","V8502","V8509","V8512")
	reduced to
	"V7221" ~ "V7552" "V7562" "V7563" "V8527" "V8509"
	(dropped V8512)

	I believe this gives me better models for the population and highlights what exists across the population more.  However.  I don't see how it really tells me which one will do better for the population.  As I can see that I can still measure that after the fact.  I think this is truly highlighting the least common denominator's though.

So now our final model's are (7a99c61)
	"V7118", ~ "V7202"
	"V8517" ~ "V8530" "V8531"   
	"V7221" ~ "V7552" "V7562" "V7563" "V8527" "V8509"
	
Milestone 4.2 59eff5e

V7221 R HS GRADE/D=1
	V7202 R'S SEX
	V7206 R'S HSHLD FATHER
	
V8517 Gang Fight
	V8530 OFTN 7HRS SLEEP
	V8565 I AM OFTEN BORED
	
V7118 #X PSYD/LIFETIME
	V7202 R'S SEX

I know without a doubt these will show true to the population.

			

seed 5 factor reduced results commit.txt			

	Milestone 1
		Identified relationships as of commit 3063ef8

	Milestone 2
		Cross validation
		Identified relationships using colsums over a subsample of entire database
		
	Milestone 3
		Implemented Cross Validation and stopped using stepAIC and opted for bestglm which allows for the smallest model to converge based on lowest cv error.  Bestglm has built in CV, which elinates the need for an external loop.
		By aggregating these terms from each subdomain up, we construct a terms list that we feed into a new bestglm formula which spits out a new list.  This list when applied to the population shows extermely significant on all terms.  We tried monte carlo with a manual CV loop but find the cross validation does all the work of comprehensively testing our sample.
		
		We get the following
		
		"V7118" "V7202" "V7551" "V8502"
		
		"V8517" "V8530" "V8531"
		
		"V7221" "V7552" "V7562" "V7563" "V8527" "V8509" "V8512"
		
	Milestone 4
		Implemented cross validation, monte carlo resampling of sub sample, and resampling of holdout/non holdout data in 2 distinct loops.  I use a width factor that controls all the loops to include # of k folds and cv iterations.  Set to 2 atm.  Default will be 3.
		
		So far
		
		8517 ~ 8530 , 8531
		
		7221 ~ 8512
		
		[1] "V8517"
[1] "holdoutReset:  1"
[1] "resample:  1"
[1] "1st pass"
[1] "V8505" "V8509" "V8514" "V8536" "V7507" "V8565"
[1] "holdout pass: "
[1] "V8514" "V8536" "V8565"
[1] "V8517"
[1] "holdoutReset:  1"
[1] "resample:  2"
[1] "1st pass"
[1] "V8514" "V8536" "V8565"
[1] "holdout pass: "
[1] "V8514" "V8565"
[1] "V8517"
[1] "holdoutReset:  2"
[1] "resample:  1"
[1] "1st pass"
[1] "V8505" "V8509" "V8514" "V8536" "V7507" "V8565"
[1] "holdout pass: "
[1] "V8514" "V8536" "V8565"
[1] "V8517"
[1] "holdoutReset:  2"
[1] "resample:  2"
[1] "1st pass"
[1] "V8514" "V8536" "V8565"
[1] "holdout pass: "
[1] "V8514" "V8565"
		

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
	
	If sample size is too small and nrFolds value is set too high... will get errors when there is no values in data[-folds]

	folds <- rep_len(1:nrFolds, nrow(data))

	> folds
	[1] 8 9 1 5 7 6 2 3 4
	> nrow(data)
	[1] 9

	full.model.train <- glm(data.train[,1]~., data=data.train)	
	
	Error in glm.fit(x = numeric(0), y = numeric(0), weights = NULL, start = NULL,  : 
  object 'fit' not found
  

Commit 5847293 2019-04-09
	v7118 
	reducedFilterList.txt	
	Seed 5 100 nrFolds

	[1] "V7118"
	V507NE V507NC  V507W 
		 0     14     43 
	V7551 V7552 V7553 V7562 V7563 
		0    51    59    68    39 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0    48    43    67    84    52 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0    30    58    73    63    33    40    18    53 
		
	Seed 6 100 nrFolds
	V507NE V507NC  V507W 
		 0     84     71 
	V7551 V7552 V7553 V7562 V7563 
		0    49    63    62    47 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0    69    73    72    57    59 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0    33    31    37    47    30    31    40    60 
	
	
	
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
	
	
Seed5CV25PctFilterList.txt		
	[1] "V7101"
	V507NE V507NC  V507W 
		 0      1      0 
	V7551 V7552 V7553 V7562 V7563 
		0     3     4     6     0 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     0     0     6     6     9 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		1     4     3     6     3     2     4     1     4 
	V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     9     9     9     9     9     9     9     9     9     9     9     9     8     9     9     8     9     7     6     6     6 
	[1] "V7104"
	V507NE V507NC  V507W 
		 0      1      2 
	V7551 V7552 V7553 V7562 V7563 
		0     2     1     3     2 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     4     6     7     3     6 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     1     1     4     4     1     1     2     4 
	V7101 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     8    10     7     9     8     8     9     8     8     9     9     8     7     7     7     5     6     4     3     3     3 
	[1] "V7112"
	V507NE V507NC  V507W 
		 0      4      0 
	V7551 V7552 V7553 V7562 V7563 
		0     3     7     7     4 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     1     3     1     6 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		1     2     1     2     5     1     3     2     1 
	V7101 V7104 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     9     6     8     8     9     9     9     9     8    10    10     9     7     8     5     5     6     2     2     2 
	[1] "V7115"
	V507NE V507NC  V507W 
		 0      1      5 
	V7551 V7552 V7553 V7562 V7563 
		0     2     3     4     4 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     2     2     4     3     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     2     3     4     4     0     4     0     4 
	V7101 V7104 V7112 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     9     7     9     9     8     9     7     8     8     7     9     9     8     6     5     5     5     3     3     3 
	[1] "V7118"
	V507NE V507NC  V507W 
		 0      0      7 
	V7551 V7552 V7553 V7562 V7563 
		0     3     7     6     3 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     4     5     6     8     3 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		1     2     1     5     5     2     2     1     5 
	V7101 V7104 V7112 V7115 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     9     8    10    10     9    10    10    10    10    10     9     9     8     6     6     6     6     3     2     2     2 
	[1] "V7127"
	V507NE V507NC  V507W 
		 0      1      2 
	V7551 V7552 V7553 V7562 V7563 
		0     2     3     6     2 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     0     1     2     0     2 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     4     6     6     5     6     2     3     9 
	V7101 V7104 V7112 V7115 V7118 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     6     9     9     8    10    10     9     9     8     9    10     9    10     6     6     5     5     3     4     4     3 
	[1] "V7097"
	V507NE V507NC  V507W 
		 0      3      8 
	V7551 V7552 V7553 V7562 V7563 
		0     2     3     5     3 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     4     9     7     6     5 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		1     3     3     8     2     6     0     1     4 
	V7101 V7104 V7112 V7115 V7118 V7127 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     8     7     9     7     9     8     9     9     8     9     8     8     9     4     7     6     6     4     3     2     2 
	[1] "V7133"
	V507NE V507NC  V507W 
		 0      1      4 
	V7551 V7552 V7553 V7562 V7563 
		0     2     4     4     2 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     4     6     6     8     7 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     5     9     5     7     6     1     1     7 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     7     7     8    10     7     8     9     9     9     8     9     8     8     6     4     5     3     3     3     3 
	[1] "V7139"
	V507NE V507NC  V507W 
		 0      7      4 
	V7551 V7552 V7553 V7562 V7563 
		0     6     5     5     9 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     2     1     5     5     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     0     0     0     0     0     1     2     2 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     6     7     7     8     6     7     9     7     9     6     8     6     7     6     5     4     5     4     3     3 
	[1] "V7142"
	V507NE V507NC  V507W 
		 0      5      4 
	V7551 V7552 V7553 V7562 V7563 
		0     3     1     6     3 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     7     5     6     7 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     4     5     5     8     3     3     1     6 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     8     7    10     8     9     9    10    10     9    10     8     8     9     7     5     6     6     1     3     1     1 
	[1] "V8451"
	V507NE V507NC  V507W 
		 0      0      3 
	V7551 V7552 V7553 V7562 V7563 
		0     0     7     5     5 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     3     7     4     5     3 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		1     4     5     7     1     0     6     9     0 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     9     9     9     9     9     9     9     9    10     9    10    10     9     9     9     9     9     8     8     8     8 
	[1] "V7426"
	V507NE V507NC  V507W 
		 0      0      6 
	V7551 V7552 V7553 V7562 V7563 
		0     1     4     6     5 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     2     2     2     2     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     0     2     2     3     0     0     1     1 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     5     4     6     4    10     7     8     9     8     6     9     7     7     7     5     5     4     1     0     1     0 
	[1] "V7121"
	V507NE V507NC  V507W 
		 0      0      4 
	V7551 V7552 V7553 V7562 V7563 
		0     3     4     3     5 
	V8526 V8527 V8528 V8529 V8530 V8531 
		1     3     3     5     4     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     5     3     4     6     1     3     1     2 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     6     7     8     5     9     7     9     8     8     9     9     8     9     9     5     4     3     1     2     2     2 
	[1] "V7124"
	V507NE V507NC  V507W 
		 0      3      7 
	V7551 V7552 V7553 V7562 V7563 
		0     2     2     3     3 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     1     2     1     3 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		1     2     3     3     4     2     0     0     4 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     8     9    10     9    10     9    10    10    10     8     9     8     8     8     6     7     6     2     2     2     1 
	[1] "V7164"
	V507NE V507NC  V507W 
		 0      5      3 
	V7551 V7552 V7553 V7562 V7563 
		0     4     1     6     6 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     2     2     5     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     4     3     1     3     0     1     2     3 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     7     7     8     7     8     7     7     7     8     7     7     7     9     7     7     7     6     6     6     6 
	[1] "V7145"
	V507NE V507NC  V507W 
		 0      2      2 
	V7551 V7552 V7553 V7562 V7563 
		0     2     6     5     2 
	V8526 V8527 V8528 V8529 V8530 V8531 
		1     2     1     1     3     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     3     2     4     4     2     0     0     4 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     8     8     9    10     9     9     9    10     7    10     9     9     9     7     6     5     5     3     4     3 
	[1] "V7109"
	V507NE V507NC  V507W 
		 0      0      2 
	V7551 V7552 V7553 V7562 V7563 
		0     3     5     5     3 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     2     4     5     2 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     2     1     7     8     2     1     3     4 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     6     7     4     8     7     7     7     8     8     7     8     8     5     2     2     3     3     2     1     1 
	[1] "V7152"
	V507NE V507NC  V507W 
		 0      3      5 
	V7551 V7552 V7553 V7562 V7563 
		0     4     6     5     7 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     4     4     4     7     4 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     4     6     7     5     2     2     2     4 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7155 V7158 V7161 V7601 V8480 
		0    10    10    10    10    10    10    10    10    10    10    10     9    10    10     8     4     6     4     4     2     2 
	[1] "V7155"
	V507NE V507NC  V507W 
		 0      1      5 
	V7551 V7552 V7553 V7562 V7563 
		0     1     2     3     4 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     0     4     6     4 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     1     3     4     3     0     0     0     6 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7158 V7161 V7601 V8480 
		0     9     9     9     9     9     9     9     9     8     9     9     9     8     8     8     5     6     2     2     2     3 
	[1] "V7158"
	V507NE V507NC  V507W 
		 0      0      4 
	V7551 V7552 V7553 V7562 V7563 
		0     2     3     3     5 
	V8526 V8527 V8528 V8529 V8530 V8531 
		1     3     2     3     6     2 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     1     2     5     5     1     1     0     5 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7161 V7601 V8480 
		0     7     7     8     5     8     6     9     9     9     8     9     8     9     7     6     5     2     4     2     3     2 
	[1] "V7161"
	V507NE V507NC  V507W 
		 0      0      1 
	V7551 V7552 V7553 V7562 V7563 
		0     1     2     0     0 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     0     0     1     0     0 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     1     0     3     2     0     2     0     4 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7601 V8480 
		0     8     8     8     8     9     8     9     9     8     9     9     8     8     8     7     7     7     8     8     7     7 
	[1] "V7601"
	V507NE V507NC  V507W 
		 0      0      1 
	V7551 V7552 V7553 V7562 V7563 
		0     3     5     5     3 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     0     3     1     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     4     3     5     6     2     6     4     6 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V8480 
		0    10    10    10     8    10    10    10    10    10    10    10     8     9     9     7    10     6     6     6     6     6 
	[1] "V8480"
	V507NE V507NC  V507W 
		 0      0      5 
	V7551 V7552 V7553 V7562 V7563 
		0     4     7     6     4 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     3     3     7     5     5 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     2     4     4     9     0     8     8     1 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 
		0     8     9     8     8     9     8     8     8     9     8     8     8     8     7     9     7     7     8     7     7     7

Seed5CV25PctGanf.txt		
[1] "V8517"
	V507NE V507NC  V507W 
		 0      0      1 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     7     7     9     9    10     7     9     9     9     9    10     7     9     8     7     7     8     5     5     5     5     5 
	V7551 V7552 V7553 V7562 V7563 
		0     2     6     5     5 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     1     2     2     3     1 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     3     3     3     4     1     5     6     1 
		
Seed5CV25PctGPA.txt
	[1] "V7221"
	V507NE V507NC  V507W 
		 0      2      3 
	V7101 V7104 V7112 V7115 V7118 V7127 V7097 V7133 V7139 V7142 V8451 V7426 V7121 V7124 V7164 V7145 V7109 V7152 V7155 V7158 V7161 V7601 V8480 
		0     5     8     9     7     9     8     8     8     8     8     7     8     7     6     6     6     6     4     2     2     2     2 
	V7551 V7552 V7553 V7562 V7563 
		0     3     6     6     2 
	V8526 V8527 V8528 V8529 V8530 V8531 
		0     0     1     4     6     9 
	V8502 V8505 V8509 V8512 V8514 V8536 V7501 V7507 V8565 
		0     3     4     3     4     2     7     4     7 		
		
Milestone 4.2
Seed 5

	V7221 
		V7202 and V7206
		
	V8517
		V8530 V8565
		
	V7118
		V7202

Seed 6

	V7221 
		V7202, V7206, V7215
		
	V8517
		V8528 V8565
		
	V7118
		V7202, V7552

Seed 7

	V7221 
		V7202, V7206
		
	V8517
		V8528 V8530 8531
		
	V7118
		V7202


Seed 5 results
	
[1] "Y: V7221"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7206" "V7215" "V7552" "V7562" "V7563" "V8527" "V8509" "V8512"
	[1] "holdout pass: "
	[1] "V7202" "V7206"
	[1] "Y: V7221"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	 [1] "V7202" "V8517" "V7206" "V7215" "V7552" "V7562" "V7563" "V8527" "V8502" "V8512" "V8536"
	[1] "holdout pass: "
	[1] "V7202" "V8517" "V7206" "V7215" "V7562" "V7563" "V8536"
	[1] "Y: V7221"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7206" "V7215" "V7552" "V7562" "V7563" "V8527" "V8509" "V8512"
	[1] "holdout pass: "
	[1] "V7202" "V7206"
	[1] "Y: V7221"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	 [1] "V7202" "V8517" "V7206" "V7215" "V7552" "V7562" "V7563" "V8527" "V8502" "V8512" "V8536"
	[1] "holdout pass: "
	[1] "V7202" "V8517" "V7206" "V7215" "V7562" "V7563" "V8536"

[1] "Y: V8517"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7551" "V8530" "V8531" "V8514" "V8565"
	[1] "holdout pass: "
	[1] "V8530" "V8531" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7551" "V8528" "V8530" "V8514" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8530" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7551" "V8530" "V8531" "V8514" "V8565"
	[1] "holdout pass: "
	[1] "V8530" "V8531" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7551" "V8528" "V8530" "V8514" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8530" "V8565"

[1] "Y: V7118"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7551" "V8502"
	[1] "holdout pass: "
	[1] "V7202"
	[1] "Y: V7118"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7202" "V7551" "V7552" "V8526" "V8527" "V8529" "V8531" "V8502" "V7501"
	[1] "holdout pass: "
	[1] "V7202"
	[1] "Y: V7118"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7551" "V8502"
	[1] "holdout pass: "
	[1] "V7202"
	[1] "Y: V7118"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7202" "V7551" "V7552" "V8526" "V8527" "V8529" "V8531" "V8502" "V7501"
	[1] "holdout pass: "
	[1] "V7202"
	
Seed 6 results

+ }
[1] ""
[1] "Y: V7221"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V8517" "V7206" "V7215" "V7562" "V7563" "V8526" "V8512"
	[1] "holdout pass: "
	[1] "V7202" "V7206" "V7215"
	[1] "Y: V7221"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	 [1] "V7202" "V8517" "V7206" "V7215" "V7562" "V7563" "V8527" "V8502" "V8509" "V8512" "V7501" "V7507"
	[1] "holdout pass: "
	[1] "V7202" "V7206" "V7215" "V7562" "V7563"
	[1] "Y: V7221"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V8517" "V7206" "V7215" "V7562" "V7563" "V8526" "V8512"
	[1] "holdout pass: "
	[1] "V7202" "V7206" "V7215"
	[1] "Y: V7221"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	 [1] "V7202" "V8517" "V7206" "V7215" "V7562" "V7563" "V8527" "V8502" "V8509" "V8512" "V7501" "V7507"
	[1] "holdout pass: "
	[1] "V7202" "V7206" "V7215" "V7562" "V7563"
[1] ""
[1] "Y: V8517"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7551" "V8528" "V8530" "V8531" "V8505" "V8509" "V8536" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8531" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7551" "V8528" "V8529" "V8531" "V8514" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7551" "V8528" "V8530" "V8531" "V8505" "V8509" "V8536" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8531" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7551" "V8528" "V8529" "V8531" "V8514" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8565"
[1] ""
[1] "Y: V7118"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7206" "V7551" "V7552" "V7553" "V8529" "V8502"
	[1] "holdout pass: "
	[1] "V7202" "V7552"
	[1] "Y: V7118"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7202" "V7221" "V7206" "V7552" "V7563" "V8509"
	[1] "holdout pass: "
	[1] "V7202" "V7552" "V7563"
	[1] "Y: V7118"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7206" "V7551" "V7552" "V7553" "V8529" "V8502"
	[1] "holdout pass: "
	[1] "V7202" "V7552"
	[1] "Y: V7118"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7202" "V7221" "V7206" "V7552" "V7563" "V8509"
	[1] "holdout pass: "
	[1] "V7202" "V7552" "V7563"
	
[1] "seed 7"
	[1] "Y: V7221"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7206" "V7215" "V7552" "V7553" "V7563" "V8526" "V8502" "V8512"
	[1] "holdout pass: "
	[1] "V7202" "V7206"
	[1] "Y: V7221"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	 [1] "V7202" "V7206" "V7215" "V7552" "V7553" "V7562" "V7563" "V8526" "V8502" "V8514"
	[1] "holdout pass: "
	[1] "V7202" "V7206" "V7215" "V8526" "V8502"
	[1] "Y: V7221"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7206" "V7215" "V7552" "V7553" "V7563" "V8526" "V8502" "V8512"
	[1] "holdout pass: "
	[1] "V7202" "V7206"
	[1] "Y: V7221"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	 [1] "V7202" "V7206" "V7215" "V7552" "V7553" "V7562" "V7563" "V8526" "V8502" "V8514"
	[1] "holdout pass: "
	[1] "V7202" "V7206" "V7215" "V8526" "V8502"
	[1] "seed 7"
	[1] "Y: V8517"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V8528" "V8529" "V8530" "V8531" "V8514" "V8536" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8529" "V8530" "V8531" "V8514" "V8536" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V8528" "V8530" "V8531" "V8505" "V8536" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8530" "V8531"
	[1] "Y: V8517"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V8528" "V8529" "V8530" "V8531" "V8514" "V8536" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8529" "V8530" "V8531" "V8514" "V8536" "V8565"
	[1] "Y: V8517"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V8528" "V8530" "V8531" "V8505" "V8536" "V8565"
	[1] "holdout pass: "
	[1] "V8528" "V8530" "V8531"
	[1] "seed 7"
	[1] "Y: V7118"
	[1] "holdoutReset:  1"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7562" "V8505" "V8509"
	[1] "holdout pass: "
	[1] "V7202"
	[1] "Y: V7118"
	[1] "holdoutReset:  1"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7202" "V7221" "V8528" "V8502" "V8512" "V8536" "V7501" "V8565"
	[1] "holdout pass: "
	[1] "V7202"
	[1] "Y: V7118"
	[1] "holdoutReset:  2"
	[1] "resample:  1"
	[1] "1st pass"
	[1] "V7202" "V7562" "V8505" "V8509"
	[1] "holdout pass: "
	[1] "V7202"
	[1] "Y: V7118"
	[1] "holdoutReset:  2"
	[1] "resample:  2"
	[1] "1st pass"
	[1] "V7202" "V7221" "V8528" "V8502" "V8512" "V8536" "V7501" "V8565"
	[1] "holdout pass: "
	[1] "V7202"	