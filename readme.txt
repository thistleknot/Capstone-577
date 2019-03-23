filterlist.txt
	columns
	factor id, description, conversion profile, y variable flag

Prerequisites
	PostgreSQL-10.4-1-win64-bigsql
		set pw to Read1234

	pgadmin4-4.3-x86


Update 
	in cleandatacode.r
		sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
		
	put following files in folder
	
		34574-0001-Data.csv
		34574-0001-Data.csv
		36149-0001-Data.csv
		36407-0001-Data.csv
		36799-0001-Data.csv
		37183-0001-Data.csv
	
Steps
	createdb.bat
		creates database
		
	then run 
	cleandatacode.r
