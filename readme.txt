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
	
	seed 5 factor reduced results commit.txt
	
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
		
		#bug:
		
			report bug as inclusion of factors as opposed to breaking out of loop
			also breaking out of loop might result in the rest of the variables not getting reported.
			
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

		GPA (3)
			V7221,2,"R HS GRADE/D=1",0	

		Violence (4)
			V8517,1,"FRQ GANG FIGHT",0

		Father1 (5)
			V7206,0,"R'S HSHLD FATHER",0

		Father2 (6)
			V7215,2,"FATHR EDUC LEVEL",0
			
			"Error in stepAIC(full.model.train, direction = "both", trace = FALSE) :"
					
		Habits (7)
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
			
Identified relationships as of commit 3063ef8

	[1] "V7133"

		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8531"
		[1] "V8527" "V8529" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8529" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8529" "V8531"
		
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V8565"
		[1] "V8505" "V8509" "V8512" "V8514"
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V7501" "V7507" "V8565"
		[1] "V8505" "V8514"
		[1] "V8505" "V8509" "V8514"
		[1] "V8505" "V8509" "V8512" "V8514" "V8536" "V8565"
		[1] "V8505" "V8509" "V8512" "V8514" "V8565"
		[1] "V8505" "V8514"
		[1] "V8505" "V8509" "V8512" "V8514"
		[1] "V8505" "V8509" "V8512" "V8514" "V8536" "V8565"
			
	[1] "V7139"
	
		[1] "V7552" "V7553"
		[1] "V7551" "V7552" "V7553" "V7562" "V7563"
		[1] "V7552" "V7553"
		[1] "V7551" "V7552" "V7553" "V7562" "V7563"
		[1] "V7552" "V7553"
		[1] "V7552" "V7562"
		[1] "V7552" "V7553"
		[1] "V7552" "V7553" "V7562"
		[1] "V7552" "V7553" "V7562"
		[1] "V7552" "V7553"		

	[1] "V7142"		
	
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
	
		[1] "V8502" "V8565"
		[1] "V8502" "V8509" "V8514" "V8536" "V8565"
		[1] "V8502" "V8512" "V8565"
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V8565"
		[1] "V8502" "V8512" "V8565"
		[1] "V8502" "V8509" "V8512" "V8565"
		[1] "V8502" "V8512" "V8565"
		[1] "V8502" "V8509" "V8514" "V8536" "V8565"
		[1] "V8502" "V8512" "V8565"
		[1] "V8502" "V8512" "V8565"	

	[1] "V8451"

		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		
		[1] "V8502" "V8565"
		[1] "V8502" "V8509" "V8514" "V8536" "V8565"
		[1] "V8502" "V8512" "V8565"
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V8565"
		[1] "V8502" "V8512" "V8565"
		[1] "V8502" "V8509" "V8512" "V8565"
		[1] "V8502" "V8512" "V8565"
		[1] "V8502" "V8509" "V8514" "V8536" "V8565"
		[1] "V8502" "V8512" "V8565"

	[1] "V7145"	

		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V7501" "V7507" "V8565"
		[1] "V8502" "V8565"
		[1] "V8502" "V8505" "V8509" "V8512" "V8536" "V8565"
		[1] "V8502" "V8509" "V8512" "V8536" "V8565"
		[1] "V8502" "V8505" "V8514" "V7501" "V7507"
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V7501" "V7507" "V8565"
		[1] "V8502" "V8565"
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V7501" "V7507" "V8565"
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V7501" "V7507" "V8565"
		[1] "V8502" "V8505" "V8509" "V8512" "V8514" "V8536" "V7507" "V8565"
		[1] "V8502" "V8512" "V8565"

	[1] "V7158"
		
		[1] "V8526" "V8528" "V8530"
		[1] "V8526" "V8528" "V8530"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8527" "V8528" "V8530"
		[1] "V8526" "V8528" "V8530"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"
		[1] "V8526" "V8528" "V8530"
		[1] "V8526" "V8528" "V8530"
		[1] "V8526" "V8528"
		[1] "V8526" "V8527" "V8528" "V8529" "V8530" "V8531"	

	