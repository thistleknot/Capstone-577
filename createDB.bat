setlocal enableextensions enabledelayedexpansion

FOR /F "tokens=*" %%a in ('returnNumLines.bat apiKey.txt') do SET numKeys=%%a
FOR /F "tokens=*" %%a in ('returnLine.bat 1 psqlPW.txt') do SET PGPASSWORD=%%a
FOR /F "tokens=*" %%a in ('returnNumLines.bat c:\test\nasdaqSymbolsNoHeader.csv') do SET numNasdaqSymbols=%%a
set waitPeriod=12
echo %waitPeriod%
set host=127.0.0.1
set PGPASSWORD=Read1234
set fullFlag=1

set dbName=readyloop
set tableName=nasdaq_facts

REM needed for null string for copy statements
set NULL="null"

REM %1 = drop flag, assume 0 (not 1)

REM download NASDAQ & Other (DOW and NYSE)
curl --silent "ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqlisted.txt" --stderr -> nasdaqlisted.txt
curl --silent "ftp://ftp.nasdaqtrader.com/SymbolDirectory/otherlisted.txt" --stderr -> otherlisted.txt

REM ETF Bonds
curl --silent "https://www.nasdaq.com/investing/etfs/etf-finder-results.aspx?download=Yes" --stderr -> ETFList.csv

	REM encapsulate q quotes
	cut -f 1,2 -d , ETFList.csv > c:\test\ETFListwQuotes.csv
