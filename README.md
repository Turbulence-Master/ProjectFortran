# Project Turbulence Tools

This project started as a part of for International Master on Turbulence 2016, Fortran class.

# What is TurbTools

Set of mathematical tools to help master students projects, homeworks developped by students themself.

# Status

# Compiling
pFUnit http://pfunit.sourceforge.net/

# Installation
1.) Inside the folder src type 'make' <br />
2.) Execute the math binary file in the newly created folder /bin 

# Documentation
The documentation of this project is handled by Doxygen. Please make the comments on your code according to the following example: <br />
|Code  style | explanation|
|------------|------------|
|!> Subroutine for using the midpoint method| here you describe your subroutine|
|    !! method| saying that this is a method|
|    !! @param a, b Lower and Upper range| the input parameter, with description|
|    !! @param n Number of domain division| more input parameters
|    !! @todo more test cases| this creates a "to do" section on the documentation|
|    subroutine midpoint(a, b, n)| sample code|
|    implicit none| sample code|
|    integer :: i,j,n| sample code|
|    real :: a,b,h,temp,xi,res,error| sample code|

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


