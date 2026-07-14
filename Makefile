SHELL := /bin/bash

PYTHON ?= python3
CONFIG_FILE := reproduced/config/thesis.yml

# Shared helper to extract configuration values from reproduced/config/thesis.yml.
define CONFIG_VALUE
$(shell printf '%s\n' \
"from pathlib import Path" \
"import json" \
"import runpy" \
"import sys" \
"" \
"config_path = Path(\"$(CONFIG_FILE)\").resolve()" \
"module = runpy.run_path(str(Path(\"reproduced/scripts/utils/yaml_loader.py\")))" \
"load_yaml = module.get(\"load_yaml_minimal\")" \
"if load_yaml is None:" \
"    print(\"yaml_loader.load_yaml_minimal is unavailable\", file=sys.stderr)" \
"    sys.exit(1)" \
"" \
"config = load_yaml(config_path)" \
"value = config" \
"for part in \"$(1)\".split('.'):" \
"    if isinstance(value, dict) and part in value:" \
"        value = value[part]" \
"    else:" \
"        print(f\"Missing configuration key: $(1)\", file=sys.stderr)" \
"        sys.exit(1)" \
"" \
"if isinstance(value, (dict, list)):" \
"    print(json.dumps(value), end=\"\")" \
"elif isinstance(value, bool):" \
"    print(\"true\" if value else \"false\", end=\"\")" \
"elif value is None:" \
"    print(\"\", end=\"\")" \
"else:" \
"    print(str(value), end=\"\")" | $(PYTHON) -)
endef

DOCKER_IMAGE := $(call CONFIG_VALUE,project.environment.docker_image)
DOCKER_BUILD_CONTEXT ?= .

SUB_MAKE := $(MAKE) -C reproduced

.PHONY: help env env-conda env-renv validate-config check check-r-syntax proxy-data export-raw-csvs chapter4 chapter5 chapter6 chapter7 chapter8 chapter8-dry verify verify-proxy verify-proxy-quick verify-real clean all docker-build rsiena run-all visualise portfolio portfolio-test privacy-check

help:
	@$(SUB_MAKE) help
	@echo "  make docker-build     # Build the thesis reproduction Docker image"

env:
	@$(SUB_MAKE) env

env-conda:
	@$(SUB_MAKE) env-conda

env-renv:
	@$(SUB_MAKE) env-renv

validate-config:
	@$(SUB_MAKE) validate-config

check:
	@$(SUB_MAKE) check

proxy-data:
	@$(SUB_MAKE) proxy-data

export-raw-csvs:
	@$(SUB_MAKE) export-raw-csvs

check-r-syntax:
	@$(SUB_MAKE) check-r-syntax FILES="$(patsubst reproduced/%,%,$(FILES))"

chapter4:
	@$(SUB_MAKE) chapter4

chapter5:
	@$(SUB_MAKE) chapter5

chapter6:
	@$(SUB_MAKE) chapter6

chapter7:
	@$(SUB_MAKE) chapter7

chapter8:
	@$(SUB_MAKE) chapter8

chapter8-dry:
	@$(SUB_MAKE) chapter8-dry

verify:
	@$(SUB_MAKE) verify

verify-proxy:
	@$(SUB_MAKE) verify-proxy

verify-proxy-quick:
	@$(SUB_MAKE) verify-proxy-quick

verify-real:
	@$(SUB_MAKE) verify-real

clean:
	@$(SUB_MAKE) clean

all:
	@$(SUB_MAKE) all

rsiena:
	@$(SUB_MAKE) rsiena

run-all:
	@$(SUB_MAKE) run-all

visualise:
	@$(SUB_MAKE) visualise

portfolio:
	@$(SUB_MAKE) portfolio

portfolio-test:
	@$(SUB_MAKE) portfolio-test

privacy-check:
	@$(SUB_MAKE) privacy-check

docker-build:
	@command -v docker >/dev/null 2>&1 || { echo "docker command not found. Install Docker or use a compatible CI runner." >&2; exit 1; }
	@echo "[docker] Building image $(DOCKER_IMAGE) from $(DOCKER_BUILD_CONTEXT)."
	docker build --pull --tag $(DOCKER_IMAGE) $(DOCKER_BUILD_CONTEXT)
