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

seed 5 factor reduced results commit.txt			

	Milestone 1
		Identified relationships as of commit 3063ef8

	Milestone 2
		Identified relationships using colsums over a subsample of entire database

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