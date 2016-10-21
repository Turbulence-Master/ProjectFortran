# makefile: makes mathematical tools program


math: tools.o
	gfortran tools.o main.f90 -o math

tools.o: tools.f90
	gfortran -c tools.f90

clean:
	rm -f tools.o tools.mod math
