SOURCES = \
BinGenI.ml \
BinI.ml \
Direction.ml \
Event51.ml \
GeneratorI.ml \
Helpers.ml \
Main.ml \
MatrixI.ml \
PackerI.ml \
PobjI.ml \
UI.ml \
World.ml \
WorldObjectI.ml

all: $(SOURCES)
	corebuild -quiet -lib graphics Main.native

check: $(SOURCES)
	@chmod u+x ../check_width
	@../check_width BinAverage.ml; \
	../check_width BinGenI.ml; \
	../check_width BinI.ml; \
	../check_width Direction.ml; \
	../check_width Event51.ml; \
	../check_width GeneratorI.ml; \
	../check_width Helpers.ml; \
	../check_width Main.ml; \
	../check_width MatrixI.ml; \
	../check_width PackerI.ml; \
	../check_width PobjI.ml; \
	../check_width UI.ml; \
	../check_width Test.ml; \
	../check_width World.ml; \
	../check_width WorldBins.ml; \
	../check_width WorldObjectI.ml; \
	../check_width WorldObjectBin.ml; \
	../check_width WorldObjectBlock.ml; \
	../check_width WorldObjectText.ml; 

clean:
	rm -rf _build Main.native
