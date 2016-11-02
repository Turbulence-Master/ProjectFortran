# Project Turbulence Tools

This project started as a part of for International Master on Turbulence 2016, Fortran class.

# What is TurbTools

Set of mathematical tools to help master students projects, homeworks developped by students themself.

# Status

# Compiling
pFUnit http://pfunit.sourceforge.net/

# Installation
1.) Inside the folder src type 'make' 
2.) Execute the math binary file in the newly created folder /bin 

# Documentation

# License

# Colaborating with the code
! get the copy of the project <br />
git clone https://github.com/Turbulence-Master/ProjectFortran.git <br /> 

! update the project with the most recent changes <br /> 
git pull <br />

! change to your own branch (do not make changes directly to master) <br /> 
git checkout -b NAME_OF_BRANCH <br />

! add files to be inserted or changed in the project <br />
git add FILE_HERE <br />

! commit your changes to your LOCAL repository <br />
git commit -m "message here" <br />

! send your LOCAL changes to the REMOTE (upstream) repository <br />
git push -u origin NAME_OF_BRANCH<br />
or
git push (if commiting directly to master branch) <br />

! merge changes from your branch to master branch <br />
git checkout master <br />
git merge NAME_OF_BRANCH <br />

! clears the files created by you, start over, discard changes (be careful) <br /> 
git clean -d -x -f <br />
