# makefile: makes mathematical tools program

#INC_PATH=../includes
LIB_PATH=../libs
BIN_PATH=../bin

FLAGS = -Wall -fcheck=bounds
OBJECTS = tools.o

$(BIN_PATH)/math: $(OBJECTS)
	mkdir $(BIN_PATH)
	gfortran $(OBJECTS) main.f90 -o $(BIN_PATH)/math $(FLAGS)

tools.o: tools.f90
	gfortran -c tools.f90

clean:
	rm -f $(OBJECTS) tools.mod
	rm -Rf $(BIN_PATH)
