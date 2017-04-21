IVORY_REPO             ?= ../../ivory
TOWER_REPO             ?= ../../tower
IVORY_TOWER_STM32_REPO ?= ../../ivory-tower-stm32

GIDL ?= gidl
NAME ?= ODrive # name prefix

default: odrive-schema-native
default: odrive-schema-tower
default: odrive-schema-elm

test: odrive-schema-native-test
test: odrive-schema-tower-test
test: odrive-schema-elm-test

.PHONY: odrive-schema-native
odrive-schema-native: odrive-schema.gidl
	$(GIDL) -b haskell-rpc \
	     -i odrive-schema.gidl \
	     -o odrive-schema-native \
	     -p odrive-schema-native \
	     -n $(NAME)

.PHONY: odrive-schema-tower
odrive-schema-tower: odrive-schema.gidl
	$(GIDL) -b tower \
	     -i odrive-schema.gidl \
	     -o odrive-schema-tower \
	     -p odrive-schema-tower \
	     -n $(NAME) \
	     --ivory-repo=$(IVORY_REPO) \
	     --tower-repo=$(TOWER_REPO) \
	     --ivory-tower-stm32-repo=$(IVORY_TOWER_STM32_REPO)

.PHONY: odrive-schema-elm
odrive-schema-elm: odrive-schema.gidl
	$(GIDL) -b elm \
	     -i odrive-schema.gidl \
	     -o odrive-schema-elm \
	     -p odrive-schema-elm \
	     -n $(NAME)

odrive-schema-native-test: odrive-schema-native
	make -C odrive-schema-native test

odrive-schema-tower-test: odrive-schema-tower
	make -C odrive-schema-tower test

odrive-schema-elm-test: odrive-schema-elm
	# no tests yet

clean:
	-rm -rf odrive-schema-native
	-rm -rf odrive-schema-tower
	-rm -rf odrive-schema-elm
