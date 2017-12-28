REM Switch to master
git submodule foreach git checkout master

REM Pull from master
git submodule foreach git pull origin master

pause