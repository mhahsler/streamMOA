SOURCES = StreamMOA.java StreamMOA_MCODResult.java StreamMOA_MCOD.java
OBJECTS = $(SOURCES:.java=.class)
JARS = StreamMOA.jar

JAVAC = javac
JAR = jar
LIBS = ../inst/java/moa.jar:./

.SUFFIXES: .class .java

.java.class:
	$(JAVAC) --release 8 -cp $(LIBS) $<

all: compile install clean

$(JARS): $(OBJECTS)
	$(JAR) cvf $@ $(OBJECTS)

compile: $(JARS)

install:
	mv $(JARS) ../inst/java/

clean:
	-@rm -f $(OBJECTS) $(JARS)
