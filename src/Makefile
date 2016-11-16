# makefile: makes mathematical tools program

#INC_PATH=../includes
LIB_PATH=../lib
BIN_PATH=../bin

FLAGS = -llapack -Wall -fcheck=bounds
OBJECTS = class_field.o precision.o

$(BIN_PATH)/generalTest: $(OBJECTS)
	mkdir -p $(BIN_PATH)
	gfortran $(OBJECTS) generalTest.f90 -o $(BIN_PATH)/generalTest $(FLAGS)

class_field.o: precision.o
	gfortran -c class_field/class_field.f90

precision.o: tools/precision.f90
	gfortran -c tools/precision.f90

clean:
	rm -f *.o *.mod
	rm -Rf $(BIN_PATH)
#	rm -Rf $(LIB_PATH)
#	rm -Rf $(INC_PATH)
