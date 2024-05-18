SOURCES = basic-e.c

BASIC = basic

CFLAGS = -std=gnu17 -gdwarf-2 -Wall
# -fprofile-arcs -ftest-coverage

all: $(BASIC)

$(BASIC): $(SOURCES)
	gcc $(CFLAGS) -o $(BASIC) $(SOURCES)

clean:
	$(RM) $(BASIC)
