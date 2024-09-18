BIN_PATH=./bin
ISA_PATH=./isa
CPU_PATH=./cpu
TST_PATH=./test
LOG_PATH=./log

ISA_FILES=$(shell find $(ISA_PATH)/ -name "*.vhdl")
CPU_FILES=$(shell find $(CPU_PATH)/ -name "*.vhdl")
TST_FILES=$(shell find $(TST_PATH)/ -name "*.vhdl")

CPU_OBJECTS=$(addprefix $(BIN_PATH)/, $(patsubst %.vhdl,%.o,$(notdir $(CPU_FILES))))
ISA_OBJECTS=$(addprefix $(BIN_PATH)/, $(patsubst %.vhdl,%.o,$(notdir $(ISA_FILES))))
TST_OBJECTS=$(addprefix $(BIN_PATH)/, $(patsubst %.vhdl,%.o,$(notdir $(TST_FILES))))

CPU_BINARIES=$(patsubst %.o,%,$(CPU_OBJECTS))
TST_BINARIES=$(patsubst %.o,%,$(TST_OBJECTS))

.PHONY: test
.PHONY: clean
.PHONY: all

all: $(ISA_OBJECTS) $(CPU_BINARIES) $(TST_BINARIES)

$(BIN_PATH)/%.o: */%.vhdl
	cd $(BIN_PATH) && ghdl -a ../$<

$(BIN_PATH)/%.o: */*/%.vhdl
	cd $(BIN_PATH) && ghdl -a ../$<

$(BIN_PATH)/%: $(BIN_PATH)/%.o
	cd $(BIN_PATH) && ghdl -e $(patsubst %.o,%,$(notdir $@)) && rm e~*

merge: $(ISA_FILES) $(CPU_FILES)
	cat $^ >> $(BIN_PATH)/cpu.vhdl

test: all
	@{  for test in $(TST_BINARIES); do \
			log_path="${LOG_PATH}/$$(basename $$test).log"; \
			printf "Running $$test: "; \
			./$$test > $$log_path 2>&1; \
			errors=`cat $$log_path | grep -c error`; \
			if [ "$$errors" -gt 0 ]; then \
				echo "FAILED ($$errors errors found)"; \
			else \
				echo "PASSED"; \
			fi; \
		done \
	}

clean:
	rm $(BIN_PATH)/* $(LOG_PATH)/*
