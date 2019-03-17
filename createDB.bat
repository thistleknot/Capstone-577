setlocal enableextensions enabledelayedexpansion

set host=127.0.0.1
set PGPASSWORD=Read1234
set fullFlag=1

REM only need to run once, else errors out, prompts for admin pw.

#https://www.a2hosting.com/kb/developer-corner/postgresql/managing-postgresql-databases-and-users-from-the-command-line

REM creates user openpg, without "openpg" will create default "user" account
REM createuser.exe --createdb --username postgres --no-createrole --pwprompt
#prompts for pw for user
createuser.exe --createdb --username postgres --no-createrole --pwprompt openpg

echo drop database analyticplatform; create database analyticplatform;| psql -U postgres -h %host%

set dbName=analyticPlatform
REM set tableName=nasdaq_facts

REM needed for null string for copy statements
set NULL="null"

REM %1 = drop flag, assume 0 (not 1)

