setlocal enableextensions enabledelayedexpansion

set host=127.0.0.1
set PGPASSWORD=Read1234
set fullFlag=1

REM only need to run once, else errors out, prompts for admin pw.

REM https://www.a2hosting.com/kb/developer-corner/postgresql/managing-postgresql-databases-and-users-from-the-command-line

REM creates user openpg, without "openpg" will create default "user" account
REM createuser.exe --createdb --username postgres --no-createrole --pwprompt
REM prompts for pw for user

REM needed for null string for copy statements
REM set NULL="null"
REM %1 = drop flag, assume 0 (not 1)

REM set dbName=analyticPlatform

createuser.exe --createdb --username postgres --no-createrole --pwprompt openpg

echo drop database analyticplatform;  psql -U postgres -h %host%
echo create database analyticplatform;| psql -U postgres -h %host%

echo drop table if exists temp_table_data | psql -U postgres analyticplatform


