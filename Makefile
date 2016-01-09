
HOSTS             = galois

config-file       = host/$1/config
config-sources    = $(sort $(wildcard config.d/*.conf) $(wildcard host/$1/config.d/*.conf))


CONFIG_FILES = $(foreach h,$(HOSTS),$(call config-file,$(h)))

$(CONFIG_FILES): host/%/config: $(call config-sources,%)
	cat $< > $@


# shell settings
SHELL       := /usr/bin/bash
.SHELLFLAGS := -e -u -c

.ONESHELL:

# So we can use $$(variable) on the prerequisites, that expand at matching time.
.SECONDEXPANSION:

.PHONY: config

all: config

config: $(CONFIG_FILES)
