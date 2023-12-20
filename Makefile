CXX = g++
CXXFLAGS = -Wall -Iinclude

SRCDIR = src
BINDIR = bin
INCDIR = include
OBJDIR = obj

SOURCES := $(filter-out $(SRCDIR)/aoc.cpp, $(wildcard $(SRCDIR)/*.cpp))
AOC_OBJ := $(OBJDIR)/aoc.o
OBJS := $(SOURCES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)

BINS := $(OBJS:$(OBJDIR)/%.o=$(BINDIR)/%)

all: $(BINS)

$(BINDIR)/%: $(OBJDIR)/%.o $(AOC_OBJ) | $(BINDIR)
	$(CXX) $(CXXFLAGS) -o $@ $^

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp | $(OBJDIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(AOC_OBJ): $(SRCDIR)/aoc.cpp | $(OBJDIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(BINDIR) $(OBJDIR):
	mkdir -p $@

clean:
	rm -f $(BINDIR)/* $(OBJDIR)/*

.PHONY: all clean
