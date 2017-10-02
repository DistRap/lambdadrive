IVORY_REPO             ?= ../../ivory
TOWER_REPO             ?= ../../tower
IVORY_TOWER_STM32_REPO ?= ../../ivory-tower-stm32
IVORY_TOWER_CANOPEN    ?= ../../ivory-tower-canopen

GIDL ?= cidl-lambdadrive
NAME ?= LDrive # name prefix

#default: lambdadrive-schema-native
default: lambdadrive-schema-tower
#default: lambdadrive-schema-elm

test: lambdadrive-schema-native-test
test: lambdadrive-schema-tower-test
test: lambdadrive-schema-elm-test

.PHONY: lambdadrive-schema-native
lambdadrive-schema-native:
	$(GIDL) -b haskell-rpc \
	     -o lambdadrive-schema-native \
	     -p lambdadrive-schema-native \
	     -n $(NAME)

.PHONY: lambdadrive-schema-tower
lambdadrive-schema-tower:
	$(GIDL) -b tower \
	     -o lambdadrive-schema-tower \
	     -p lambdadrive-schema-tower \
	     -n $(NAME) \
	     --ivory-repo=$(IVORY_REPO) \
	     --tower-repo=$(TOWER_REPO) \
	     --ivory-tower-stm32-repo=$(IVORY_TOWER_STM32_REPO) \
	     --canopen-repo=$(IVORY_TOWER_CANOPEN)

.PHONY: lambdadrive-schema-elm
lambdadrive-schema-elm:
	$(GIDL) -b elm \
	     -o lambdadrive-schema-elm \
	     -p lambdadrive-schema-elm \
	     -n $(NAME)

lambdadrive-schema-native-test: lambdadrive-schema-native
	make -C lambdadrive-schema-native test

lambdadrive-schema-tower-test: lambdadrive-schema-tower
	make -C lambdadrive-schema-tower test

lambdadrive-schema-elm-test: lambdadrive-schema-elm
	# no tests yet

clean:
	-rm -rf lambdadrive-schema-native
	-rm -rf lambdadrive-schema-tower
	-rm -rf lambdadrive-schema-elm
