# Project Turbulence Tools

This project started as a part of for International Master on Turbulence 2016, Fortran class.

# What is TurbTools

Set of mathematical tools to help master students projects, homeworks developped by students themself.

# Status

# Compiling
pFUnit http://pfunit.sourceforge.net/

# Installation
You need the following packages: git, gfortran and liblapack-dev

_sudo apt-get install git gfortran liblapack-dev_

To test the lapack modules, you can go to the src folder and type:

_gfortran -c tools/precision.f90_

_gfortran -llapack precision.o lapackTesting.f90 -o lapackTesting_

_./lapackTesting_


# Documentation

You need the following packages: doxygen

_sudo apt-get install doxygen_

To generate the documentation, go to the doc folder and simply type "doxygen".
A folder named html will be created and you can open the file index.html to see the documentation.

The Doxygen documentation will read special syntax from your code, so if you want your code to appear, __you have to follow some rules__. Quick explanation: http://fortranwiki.org/fortran/show/Doxygen

The module [class_io](src/class_io/class_io.f90) is the best example for doxygen Documentation, using the most important notations from doxygen.

# License

# Colaborating with the code

## Cheat Sheet
|Command      |Function                                              |
|-------------|------------------------------------------------------|
|git clone https://github.com/Turbulence-Master/ProjectFortran.git |Get the copy of the project|
|git pull |Update the project with the most recent changes|
|git checkout -b NAME_OF_BRANCH |Change to your own branch (do not make changes directly to master)|
|git add FILE_HERE |Add files to be inserted or changed in the project|
|git commit -m "message here" |Commit your changes to your LOCAL repository|
|git push -u origin NAME_OF_BRANCH |Send your LOCAL changes to the REMOTE (upstream) repository|
|git push |If commiting directly to master branch|
|git checkout master |Change to master branch|
|git merge NAME_OF_BRANCH |Merge changes from your branch to master branch|
|git branch -d BRANCH_NAME |Delete your branch|
|git push origin --delete BRANCH_NAME  |Send the information of the branch deletion upstream|
|git clean -d -x -f |Clears the files created by you, start over, discard changes (BE CAREFUL)|


