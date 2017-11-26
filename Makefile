OSNAME :=$(shell grep -o '^NAME=.*$$' /etc/os-release | cut -d'=' -f2 )
ifeq ($(OSNAME),NixOS)
       STACK=stack --nix
else
       STACK=stack
endif

TARGET ?= /dev/odrive
#TARGET ?= /dev/f4gdb
IVORYFLAGS ?= --const-fold --verbose
TESTS      := \
	cansendrecv-test \
	blink-test \
	spi-test \
	pwm-test \
	encoder-test \
	adc-test \
	adcmulti-test \
	spin-test \
	calib-test \
	focvoltage-test \
	foccurrent-test \
	canopen-test
AADL_TESTS := 
CLEANS     := $(foreach test,$(TESTS),$(test)-clean) \
	            $(foreach test,$(AADL_TESTS),$(test)_clean)
GDB := arm-none-eabi-gdb \
		--ex 'target extended-remote $(TARGET)' \
		--ex 'monitor connect_srst disable' \
		--ex 'monitor swdp_scan' \
		--ex 'set mem inaccessible-by-default off' \
		--ex 'attach 1'

all: schema $(TESTS)

.PHONY: test clean $(TESTS) $(AADL_TESTS) $(CLEANS)
test: $(TESTS) $(AADL_TESTS)
clean: $(CLEANS)
	make -C canopen-schema clean

# This target bootstraps Gidl if the generated packages do not exist
# yet, then calls the default target to generate them.
.PHONY: cidl-lambdadrive-bootstrap
cidl-lambdadrive-bootstrap:
	$(STACK) --stack-yaml=cidl-bootstrap.yaml install cidl-lambdadrive

schema: cidl-lambdadrive-bootstrap
	make -C canopen-schema

define MKTEST
$(1): schema
	# ideally we would build only the target executable
	# needs fixing in stack
	# https://github.com/commercialhaskell/stack/issues/1406
	$(STACK) build . --exec '$(1)-gen --src-dir=build/$(1) $(IVORYFLAGS)'
	make -C build/$(1)
$(1)-clean:
	rm -rf build/$(1)
$(1)-gdb: $(1)
	$(GDB) build/$(1)/image
$(1)-gdbtui: $(1)
	$(GDB) -tui build/$(1)/image
$(1)-load: $(1)
	$(GDB) --ex 'load' build/$(1)/image
$(1)-run: $(1)
	$(GDB) --ex 'load' --ex 'run' build/$(1)/image
endef

define MK_AADL_TEST
$(1):
	$(STACK) build . --exec '$(1)_gen --src-dir=build_aadl/$(1) $(IVORYFLAGS)'
$(1)_clean:
	rm -rf build_aadl/$(1)
endef

$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
$(foreach test,$(AADL_TESTS),$(eval $(call MK_AADL_TEST,$(test))))
