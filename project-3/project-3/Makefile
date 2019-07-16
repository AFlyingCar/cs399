IDRIS=idris

PACKAGES=-p contrib -p gl -p glfw
SOURCES=$(wildcard *.idr)
OUT=pong
IFLAGS=#--total

IBCS=$(wildcard *.ibc)

all:
	$(IDRIS) $(PACKAGES) $(SOURCES) $(IFLAGS) -o $(OUT)

clean:
	$(RM) $(IBCS) $(OUT)

