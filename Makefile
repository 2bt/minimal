LIB = -lSDL
SRC = $(wildcard src/*.d)
BIN = minimal
CC = gdc

all: $(BIN)

$(BIN): $(SRC)
	$(CC) -O2 -o $(BIN) $(SRC) $(LIB) -g


.PHONY: clean
clean:
	rm -f $(BIN)
