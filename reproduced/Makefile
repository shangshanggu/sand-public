SHELL := /bin/bash
PYTHON ?= python3
PYTHON_BOOT := $(PYTHON)
CONDA ?= conda
CONDA_SSL_VERIFY ?= true

# Some environments have Conda installed but not on PATH (e.g., Miniforge/Miniconda under $HOME).
# If CONDA isn't resolvable as a command, try a few common install locations.
CONDA_BIN := $(shell command -v $(CONDA) 2>/dev/null)
ifeq ($(strip $(CONDA_BIN)),)
CONDA_CANDIDATES := \
	$(HOME)/miniforge3/bin/conda \
	$(HOME)/mambaforge/bin/conda \
	$(HOME)/miniconda3/bin/conda \
	$(HOME)/anaconda3/bin/conda \
	$(HOME)/.conda/bin/conda
CONDA_BIN := $(firstword $(wildcard $(CONDA_CANDIDATES)))
ifneq ($(strip $(CONDA_BIN)),)
CONDA := $(CONDA_BIN)
endif
endif

CONFIG_FILE := config/thesis.yml

# Helper to extract configuration values using the shared YAML loader.
define CONFIG_VALUE
$(shell printf '%s\n' \
"from pathlib import Path" \
"import json" \
"import runpy" \
"import sys" \
"" \
"config_path = Path(\"$(CONFIG_FILE)\").resolve()" \
"module = runpy.run_path(str(Path(\"scripts/utils/yaml_loader.py\")))" \
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
"    print(str(value), end=\"\")" | $(PYTHON_BOOT) -)
endef

# Normalise repository-relative paths to the reproduced/ working directory.
define RELATIVE_TO_REPRO
$(strip $(if $(filter reproduced/%,$(1)),$(patsubst reproduced/%,%,$(1)),$(1)))
endef

CONDA_RUN ?=

CONDA_ENV := $(call CONFIG_VALUE,project.environment.conda_env)

ifneq ($(strip $(CONDA_ENV)),)
ifneq ($(strip $(CONDA_BIN)),)
CONDA_RUN := $(CONDA) run --no-capture-output -n $(CONDA_ENV)
endif
endif

R ?= Rscript

PYTHON_RUNNER := $(PYTHON)
R_RUNNER := $(R)

ifneq ($(strip $(CONDA_RUN)),)
PYTHON_RUNNER := $(CONDA_RUN) $(PYTHON_RUNNER)
R_RUNNER := $(CONDA_RUN) $(R_RUNNER)
PYTHON_BOOT := $(PYTHON_RUNNER)
endif

ENVIRONMENT_FILE := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,project.environment.environment_manifest))
RENV_LOCKFILE := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,project.environment.renv_lock))
DOCKER_IMAGE := $(call CONFIG_VALUE,project.environment.docker_image)

CHAPTER4_ENABLED := $(call CONFIG_VALUE,chapters.chapter4_data_collection.enabled)
CHAPTER5_ENABLED := $(call CONFIG_VALUE,chapters.chapter5_descriptive_norms.enabled)
CHAPTER6_ENABLED := $(call CONFIG_VALUE,chapters.chapter6_injunctive_norms.enabled)
CHAPTER7_ENABLED := $(call CONFIG_VALUE,chapters.chapter7_saom.enabled)
CHAPTER7_PLACEHOLDER_ENABLED := $(call CONFIG_VALUE,chapters.chapter7_saom.placeholder.enabled)
CHAPTER8_ENABLED := $(call CONFIG_VALUE,chapters.chapter8_interventions.enabled)

CHAPTER4_SCRIPTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter4_data_collection.scripts_dir))
CHAPTER5_SCRIPTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter5_descriptive_norms.scripts_dir))
CHAPTER6_SCRIPTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter6_injunctive_norms.scripts_dir))
CHAPTER7_SCRIPTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter7_saom.scripts_dir))
CHAPTER8_OUTPUTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter8_interventions.outputs_dir))
CHAPTER8_MANIFEST := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter8_interventions.scenario_manifest))
CHAPTER8_SCRIPTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter8_interventions.scripts_dir))
CHAPTER8_DATA_DIR := $(CHAPTER8_OUTPUTS_DIR)/data
CHAPTER8_LOGS_DIR := $(CHAPTER8_OUTPUTS_DIR)/logs
CHAPTER8_RESULTS_DIR := $(CHAPTER8_OUTPUTS_DIR)/results
CHAPTER8_SUMMARIES_DIR := $(CHAPTER8_OUTPUTS_DIR)/summaries
CHAPTER8_STRATEGY_OUTPUT := $(CHAPTER8_DATA_DIR)/df_intervention_strategy.csv
CHAPTER8_STAGE_SUMMARY := $(CHAPTER8_LOGS_DIR)/staged_inputs_summary.json
CHAPTER8_STAGE_METADATA := $(CHAPTER8_LOGS_DIR)/staged_inputs_metadata.json
CHAPTER8_SCENARIO_SNAPSHOT := $(CHAPTER8_LOGS_DIR)/manifest_snapshot.json
CHAPTER8_SCENARIO_TABLE := $(CHAPTER8_DATA_DIR)/scenario_manifest.csv
CHAPTER8_RESULTS_OUTPUT := $(CHAPTER8_RESULTS_DIR)/intervention_results.csv
CHAPTER8_RESULTS_LOG := $(CHAPTER8_LOGS_DIR)/intervention_runs.json
CHAPTER8_SUMMARY_JSON := $(CHAPTER8_SUMMARIES_DIR)/intervention_summary.json
CHAPTER8_SUMMARY_TABLE := $(CHAPTER8_SUMMARIES_DIR)/intervention_summary.csv
# Canonical Chapter 8 manifest builder: reproduced/analyses/chapter8_interventions/scripts/00_build_scenario_manifest.R
THESIS_PACK_SCRIPT := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,thesis.packaging.pack_script))
THESIS_RENDER_SCRIPT := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,thesis.build.render_script))
PORTFOLIO_OUTPUTS_DIR := outputs/portfolio
PORTFOLIO_NETWORKS_DIR = $(CHAPTER7_OUTPUTS_DIR)/interactive_networks

CHAPTER4_OUTPUTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter4_data_collection.outputs_dir))
CHAPTER5_OUTPUTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter5_descriptive_norms.outputs_dir))
CHAPTER6_OUTPUTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter6_injunctive_norms.outputs_dir))
CHAPTER7_OUTPUTS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter7_saom.outputs_dir))

CHAPTER7_DEFAULT_MODEL_RAW := $(call CONFIG_VALUE,chapters.chapter7_saom.default_model)
CHAPTER7_DEFAULT_MODEL := $(if $(CHAPTER7_DEFAULT_MODEL_RAW),$(CHAPTER7_DEFAULT_MODEL_RAW),base)

RSIENA_INSTALL_SCRIPT := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,rsiena.installation.script_path))

LOGS_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,project.paths.logs_dir))
DATA_CHECKSUM_LOG := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,data.checksum_log))
DATA_MODE ?= $(call CONFIG_VALUE,data.mode)
PROXY_DATA_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,data.proxy_dir))
# Allow environment overrides: `SAND_DATA_MODE=proxy make chapter4`.
# If DATA_MODE is set explicitly on the make command line, it wins.
ifeq ($(origin DATA_MODE), file)
ifneq ($(strip $(SAND_DATA_MODE)),)
DATA_MODE := $(SAND_DATA_MODE)
endif
endif
export SAND_DATA_MODE = $(DATA_MODE)

CHAPTER4_SCRIPTS := $(sort $(wildcard $(CHAPTER4_SCRIPTS_DIR)/*.R))
CHAPTER4_QA_SCRIPT := $(CHAPTER4_SCRIPTS_DIR)/02_qa_checks_norms.R
CHAPTER4_EXPORT_SCRIPT := $(CHAPTER4_SCRIPTS_DIR)/03_export_norms_longitudinal.R
RAW_DATA_DIR := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,project.paths.raw_data_dir))
DATA_DIR := $(if $(filter proxy,$(DATA_MODE)),$(PROXY_DATA_DIR),$(RAW_DATA_DIR))
LIST_BY_WAVE_FILE := $(DATA_DIR)/list_by_wave.RData
CHAPTER4_SYNTHETIC_SENTINEL := $(DATA_DIR)/.chapter4_synthetic
CHAPTER4_PROXY_MARKER := $(DATA_DIR)/.realistic_proxy_data
CHAPTER4_SCHEMA_FILE := $(DATA_DIR)/list_by_wave_schema.csv
CHAPTER4_SAMPLE_LIST_BY_WAVE_SHA256 := c02dc7b0e652a053acb5db85f3969e122fbecab03b0b4986dbe176cfbd7a8dc9
CHAPTER4_SUMMARY_TABLE := $(CHAPTER4_OUTPUTS_DIR)/tables/norms_longitudinal_summary.csv
CHAPTER5_SCRIPTS := $(sort $(wildcard $(CHAPTER5_SCRIPTS_DIR)/*.R))
CHAPTER6_SCRIPTS := $(sort $(wildcard $(CHAPTER6_SCRIPTS_DIR)/*.R))
CHAPTER7_SCRIPTS := $(sort $(wildcard $(CHAPTER7_SCRIPTS_DIR)/*.R))
CHAPTER7_BASELINE_PATH := $(call RELATIVE_TO_REPRO,$(call CONFIG_VALUE,chapters.chapter7_saom.baseline_model.path))

comma := ,

R_PACKAGE_REPO := https://packagemanager.posit.co/cran/latest,https://cloud.r-project.org
R_PRIMARY_REPO = $(firstword $(subst $(comma), ,$(R_PACKAGE_REPO)))
R_PACKAGE_INSTALLER ?= $(R_RUNNER) scripts/00_setup/install_r_packages.R --check-only true --repos $(R_PACKAGE_REPO) --prefer-binary true --retries 4

CHAPTER4_PACKAGES ?= yaml,cpp11,dplyr,tidyr,purrr,stringr,readr,zoo,rlang,jsonlite,tzdb,vroom
CHAPTER5_PACKAGES ?= jsonlite,yaml,sna,numDeriv
CHAPTER6_PACKAGES ?= jsonlite,yaml,sna,numDeriv
CHAPTER7_PACKAGES ?= jsonlite,yaml
CHAPTER8_PACKAGES ?= cli,jsonlite,yaml,dplyr,tidyr,purrr,stringr,readr,tibble,igraph,abind,ggplot2,gridExtra,scales
THESIS_PACKAGES ?= yaml,jsonlite
PORTFOLIO_PACKAGES ?= yaml,jsonlite,igraph,visNetwork,htmlwidgets,htmltools,testthat

.PHONY: help env env-conda env-renv validate-config proxy-data export-raw-csvs check check-r-syntax chapter4 chapter5 chapter6 chapter7 chapter8 chapter8-dry verify verify-proxy verify-proxy-quick verify-real clean all rsiena run-all thesis visualise portfolio portfolio-test privacy-check

help:
	@echo "Targets (available from repo root or reproduced/):"
	@echo "  make env              # Update Conda environment and restore renv snapshot"
	@echo "  make validate-config  # Validate reproduced/config/thesis.yml"
	@echo "  make proxy-data       # Generate realistic proxy inputs under reproduced/data/proxy/"
	@echo "  make export-raw-csvs  # Derive participants.csv/outcomes.csv from list_by_wave.RData"
	@echo "  make chapter4         # Reproduce Chapter 4 data exports and QA checks"
	@echo "  make chapter5         # Reproduce Chapter 5 descriptive norms"
	@echo "  make chapter6         # Reproduce Chapter 6 injunctive norms"
	@echo "  make chapter7         # Reproduce Chapter 7 SAOM baseline"
	@echo "  make chapter8         # Execute Chapter 8 intervention pipeline"
	@echo "  make chapter8-dry     # Dry-run Chapter 8 intervention staging"
	@echo "  make visualise        # Generate network visualisations from proxy data"
	@echo "  make portfolio        # Build public portfolio artifacts and run privacy checks"
	@echo "  make portfolio-test   # Run portfolio unit and integration tests"
	@echo "  make check            # Run fast configuration and portfolio tests"
	@echo "  make verify           # Dispatch to proxy structural or real empirical verification"
	@echo "  make verify-proxy     # Run Chapters 4–7 on proxy data and check artifact structure"
	@echo "  make verify-real      # Run Chapters 4–7 on protected data and enforce benchmarks"
	@echo "  make clean            # Remove generated outputs and logs"
	@echo "  make thesis           # Package assets and render thesis artefacts"
	@echo "  make all              # Run validate-config then Chapters 4–7"
	@echo "  make run-all          # Execute the R orchestration wrapper"

env: env-conda env-renv
	@echo "Environment bootstrap completed."

env-conda:
	@command -v $(CONDA) >/dev/null 2>&1 || { echo "conda command not found. Install Conda or update your PATH." >&2; exit 1; }
	@if CONDA_SSL_VERIFY=$(CONDA_SSL_VERIFY) $(CONDA) env list | awk '{print $$1}' | grep -Fxq "$(CONDA_ENV)"; then \
			echo "[env] Updating Conda environment '$(CONDA_ENV)' using $(ENVIRONMENT_FILE) via $(CONDA) (CONDA_SSL_VERIFY=$(CONDA_SSL_VERIFY))."; \
			CONDA_SSL_VERIFY=$(CONDA_SSL_VERIFY) $(CONDA) env update --name $(CONDA_ENV) --file $(ENVIRONMENT_FILE); \
	else \
			echo "[env] Conda environment '$(CONDA_ENV)' not found. Creating it from $(ENVIRONMENT_FILE) via $(CONDA) (CONDA_SSL_VERIFY=$(CONDA_SSL_VERIFY))."; \
			CONDA_SSL_VERIFY=$(CONDA_SSL_VERIFY) $(CONDA) env create --name $(CONDA_ENV) --file $(ENVIRONMENT_FILE); \
	fi

env-renv:
	@$(R_RUNNER) --version >/dev/null 2>&1 || { echo "Rscript is unavailable through the configured runner. Install the Conda environment or adjust R_RUNNER." >&2; exit 1; }
	@echo "[env] Restoring renv snapshot defined in $(RENV_LOCKFILE)."
	RENV_CONFIG_REPOS_OVERRIDE=$(R_PRIMARY_REPO) RSPM=$(R_PRIMARY_REPO) $(R_RUNNER) -e "options(repos = c(CRAN='$(R_PRIMARY_REPO)')); if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos = '$(R_PRIMARY_REPO)', dependencies = FALSE); renv::restore(project = '.', library = renv::paths[['library']](project = '.'), prompt = FALSE)"

validate-config:
	@echo "[config] Validating $(CONFIG_FILE)."
	@mkdir -p $(LOGS_DIR)/validate-config
	$(PYTHON_RUNNER) scripts/00_setup/validate_config.py --config $(CONFIG_FILE)

proxy-data:
	@echo "[data] Generating realistic proxy data at $(PROXY_DATA_DIR)."
	@mkdir -p $(PROXY_DATA_DIR)
	$(R_RUNNER) scripts/00_setup/create_realistic_proxy_data.R $(PROXY_DATA_DIR)

export-raw-csvs:
	@echo "[data] Exporting participants/outcomes CSVs from $(LIST_BY_WAVE_FILE)."
	$(R_RUNNER) scripts/00_setup/export_raw_csvs_from_list_by_wave.R

check-r-syntax:
	@if [ -z "$(FILES)" ]; then \
                echo "Usage: make check-r-syntax FILES='path/to/file1.R [path/to/file2.R ...]'" >&2; \
                exit 1; \
	fi
	@$(R_RUNNER) --version >/dev/null 2>&1 || { echo "Rscript is unavailable through the configured runner. Install the Conda environment or adjust R_RUNNER." >&2; exit 1; }
	@echo "[check-r-syntax] Parsing $(FILES)"
	$(R_RUNNER) scripts/utils/check_r_syntax.R $(FILES)

check: validate-config
	$(PYTHON_RUNNER) -m unittest discover -s tests -p 'test_*.py'
	@$(MAKE) DATA_MODE=proxy portfolio-test

chapter4:
	@if [ "$(CHAPTER4_ENABLED)" != "true" ]; then echo "Chapter 4 is disabled in $(CONFIG_FILE)." >&2; exit 1; fi
	@$(R_RUNNER) --version >/dev/null 2>&1 || { echo "Rscript is unavailable through the configured runner. Install the Conda environment or adjust R_RUNNER." >&2; exit 1; }
	@ALLOW_DEGRADED=""; \
	SYNTHETIC_REASON=""; \
	if [ -f "$(CHAPTER4_SYNTHETIC_SENTINEL)" ]; then \
		SYNTHETIC_REASON="synthetic data sentinel detected at $(CHAPTER4_SYNTHETIC_SENTINEL)"; \
	fi; \
	if [ -z "$$SYNTHETIC_REASON" ] && [ -f "$(CHAPTER4_PROXY_MARKER)" ]; then \
		SYNTHETIC_REASON="proxy data marker detected at $(CHAPTER4_PROXY_MARKER)"; \
	fi; \
	if [ -z "$$SYNTHETIC_REASON" ] && [ -f "$(CHAPTER4_SCHEMA_FILE)" ]; then \
		SYNTHETIC_REASON="placeholder schema detected at $(CHAPTER4_SCHEMA_FILE)"; \
	fi; \
	if [ -z "$$SYNTHETIC_REASON" ] && [ -f "$(DATA_DIR)/participants.csv" ]; then \
	        PARTICIPANT_COUNT=$$(awk -F',' 'NR>1 {count++} END {print count+0}' "$(DATA_DIR)/participants.csv"); \
	        FIRST_ID=$$(awk -F',' 'NR==2 {gsub(/"/,"",$$1); print $$1}' "$(DATA_DIR)/participants.csv"); \
		if [ -n "$$PARTICIPANT_COUNT" ] && [ "$$PARTICIPANT_COUNT" -le 24 ] && [ "$$FIRST_ID" = "1001" ]; then \
	        	SYNTHETIC_REASON="sample participant roster detected in $(DATA_DIR)/participants.csv"; \
		fi; \
	fi; \
	if [ -z "$$SYNTHETIC_REASON" ] && [ -f "$(LIST_BY_WAVE_FILE)" ]; then \
	        CURRENT_HASH=$$(LIST_BY_WAVE_PATH="$(LIST_BY_WAVE_FILE)" python3 -c "import hashlib, os; path=os.environ['LIST_BY_WAVE_PATH']; print(hashlib.sha256(open(path, 'rb').read()).hexdigest())"); \
		if [ "$$CURRENT_HASH" = "$(CHAPTER4_SAMPLE_LIST_BY_WAVE_SHA256)" ]; then \
	        	SYNTHETIC_REASON="list_by_wave.RData matches packaged sample bundle"; \
		fi; \
	fi; \
	if [ -n "$$SYNTHETIC_REASON" ]; then \
		ALLOW_DEGRADED="--allow-degraded"; \
		echo "[chapter4] $$SYNTHETIC_REASON; QA will use non-empirical proxy tolerances."; \
	fi; \
	if [ ! -f "$(LIST_BY_WAVE_FILE)" ]; then \
		echo "[chapter4] Required raw data missing: $(LIST_BY_WAVE_FILE)." >&2; \
		if [ "$(DATA_MODE)" = "proxy" ]; then \
			echo "[chapter4] Run 'make proxy-data' to generate proxy inputs, then re-run with DATA_MODE=proxy (or SAND_DATA_MODE=proxy)." >&2; \
		else \
			echo "[chapter4] Stage real data in $(RAW_DATA_DIR) (default), or run 'make proxy-data' and re-run with DATA_MODE=proxy (or SAND_DATA_MODE=proxy)." >&2; \
		fi; \
		exit 1; \
	fi; \
	echo "[chapter4] Ensuring Chapter 4 R package dependencies are installed."; \
	$(R_PACKAGE_INSTALLER) --packages "$(CHAPTER4_PACKAGES)" || exit $$?; \
	mkdir -p $(LOGS_DIR)/chapter4; \
	mkdir -p $(dir $(CHAPTER4_SUMMARY_TABLE)); \
	for script in $(CHAPTER4_SCRIPTS); do \
		echo "[chapter4] Running $$script"; \
		if [ "$$script" = "$(CHAPTER4_QA_SCRIPT)" ] && [ -n "$$ALLOW_DEGRADED" ]; then \
	$(R_RUNNER) $$script $$ALLOW_DEGRADED || exit $$?; \
	        elif [ "$$script" = "$(CHAPTER4_EXPORT_SCRIPT)" ]; then \
	$(R_RUNNER) $$script --summary $(CHAPTER4_SUMMARY_TABLE) || exit $$?; \
	        else \
	$(R_RUNNER) $$script || exit $$?; \
		fi; \
	done
chapter5: chapter4
	@if [ "$(CHAPTER5_ENABLED)" != "true" ]; then echo "Chapter 5 is disabled in $(CONFIG_FILE)." >&2; exit 1; fi
	@$(R_RUNNER) --version >/dev/null 2>&1 || { echo "Rscript is unavailable through the configured runner. Install the Conda environment or adjust R_RUNNER." >&2; exit 1; }
	@echo "[chapter5] Ensuring Chapter 5 R package dependencies are installed."
	@$(R_PACKAGE_INSTALLER) --packages "$(CHAPTER5_PACKAGES)"
	@mkdir -p $(LOGS_DIR)/chapter5
	@for script in $(CHAPTER5_SCRIPTS); do \
		echo "[chapter5] Running $$script"; \
	$(R_RUNNER) $$script || exit $$?; \
	done

chapter6: chapter4
	@if [ "$(CHAPTER6_ENABLED)" != "true" ]; then echo "Chapter 6 is disabled in $(CONFIG_FILE)." >&2; exit 1; fi
	@mkdir -p $(LOGS_DIR)/chapter6
	@echo "[chapter6] Ensuring Chapter 6 R package dependencies are installed."
	@$(R_PACKAGE_INSTALLER) --packages "$(CHAPTER6_PACKAGES)"
	@for script in $(CHAPTER6_SCRIPTS); do \
		echo "[chapter6] Running $$script"; \
	$(R_RUNNER) $$script || exit $$?; \
	done

rsiena:
	@$(R_RUNNER) --version >/dev/null 2>&1 || { echo "Rscript is unavailable through the configured runner. Install the Conda environment or adjust R_RUNNER." >&2; exit 1; }
	@if [ ! -f "$(RSIENA_INSTALL_SCRIPT)" ]; then echo "RSiena installation script missing at $(RSIENA_INSTALL_SCRIPT)." >&2; exit 1; fi
	@echo "[rsiena] Ensuring RSiena package is installed."
	$(R_RUNNER) $(RSIENA_INSTALL_SCRIPT)

chapter7: chapter4 chapter5
	@if [ "$(CHAPTER7_ENABLED)" != "true" ]; then echo "Chapter 7 is disabled in $(CONFIG_FILE)." >&2; exit 1; fi
	@$(MAKE) rsiena
	@mkdir -p $(LOGS_DIR)/chapter7
	@echo "[chapter7] Ensuring Chapter 7 R package dependencies are installed."
	@$(R_PACKAGE_INSTALLER) --packages "$(CHAPTER7_PACKAGES)"
	@if [ -n "$(CHAPTER7_BASELINE_PATH)" ] && [ ! -f "$(CHAPTER7_BASELINE_PATH)" ]; then \
	echo "[chapter7] No cached baseline at $(CHAPTER7_BASELINE_PATH); estimation will run from scratch."; \
	fi
	@for script in $(CHAPTER7_SCRIPTS); do \
	echo "[chapter7] Running $$script"; \
	if [[ $$script == *"02_run_saom_model.R" ]]; then \
	$(R_RUNNER) $$script --model $(CHAPTER7_DEFAULT_MODEL) || exit $$?; \
	else \
	$(R_RUNNER) $$script || exit $$?; \
	fi; \
	done

chapter8: chapter7
	@if [ "$(CHAPTER8_ENABLED)" != "true" ]; then echo "Chapter 8 is disabled in $(CONFIG_FILE)." >&2; exit 1; fi
	@$(R_RUNNER) --version >/dev/null 2>&1 || { echo "Rscript is unavailable through the configured runner. Install the Conda environment or adjust R_RUNNER." >&2; exit 1; }
	@echo "[chapter8] Ensuring Chapter 8 R package dependencies are installed."
	@$(R_PACKAGE_INSTALLER) --packages "$(CHAPTER8_PACKAGES)"
	@echo "[chapter8] Step 00: Building scenario manifest."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/00_build_scenario_manifest.R
	@echo "[chapter8] Step 01: Preparing baseline and strategy grid."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/01_prepare_baseline_and_strategy.R
	@echo "[chapter8] Step 02: Running first-period interventions."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/02_run_interventions.R
	@echo "[chapter8] Step 03: Chaining beyond-period simulations."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/03_chain_beyond_periods.R
	@echo "[chapter8] Step 04: Combining and summarising results."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/04_combine_and_summarize.R
	@echo "[chapter8] Step 05: Plotting intervention effects."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/05_plot_intervention_effects.R
chapter8-dry:
	@echo "[chapter8-dry] Ensuring Chapter 8 R package dependencies are installed."
	@$(R_PACKAGE_INSTALLER) --packages "$(CHAPTER8_PACKAGES)"
	@echo "[chapter8-dry] Step 00: Building scenario manifest."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/00_build_scenario_manifest.R
	@echo "[chapter8-dry] Step 01: Preparing baseline and strategy grid."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/01_prepare_baseline_and_strategy.R
	@echo "[chapter8-dry] Step 02: Running first-period interventions (dry-run)."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/02_run_interventions.R --dry-run
	@echo "[chapter8-dry] Step 03: Chaining beyond-period simulations (dry-run)."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/03_chain_beyond_periods.R --dry-run
	@echo "[chapter8-dry] Step 04: Combining and summarising results."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/04_combine_and_summarize.R
	@echo "[chapter8-dry] Step 05: Plotting intervention effects."
	$(R_RUNNER) $(CHAPTER8_SCRIPTS_DIR)/05_plot_intervention_effects.R
verify:
	@if [ "$(DATA_MODE)" = "proxy" ]; then \
		$(MAKE) DATA_MODE=proxy verify-proxy; \
	else \
		$(MAKE) DATA_MODE=real verify-real; \
	fi

verify-proxy: proxy-data all
	@echo "[verify] Generating checksum manifest at $(DATA_CHECKSUM_LOG)."
	@mkdir -p $(LOGS_DIR)/verify
	$(PYTHON_RUNNER) scripts/00_setup/generate_data_hashes.py --config $(CONFIG_FILE) --output $(DATA_CHECKSUM_LOG)
	@echo "[verify] Checking proxy execution and artifact structure (not empirical findings)."
	$(PYTHON_RUNNER) scripts/00_setup/verify_outputs.py --mode proxy --chapters 4,5,6,7

verify-proxy-quick: proxy-data validate-config chapter4 chapter5 chapter6
	@echo "[verify] Checking proxy Chapters 4–6 for fast CI."
	$(PYTHON_RUNNER) scripts/00_setup/generate_data_hashes.py --config $(CONFIG_FILE) --output $(DATA_CHECKSUM_LOG)
	$(PYTHON_RUNNER) scripts/00_setup/verify_outputs.py --mode proxy --chapters 4,5,6

verify-real: all
	@echo "[verify] Generating protected-data checksum manifest at $(DATA_CHECKSUM_LOG)."
	@mkdir -p $(LOGS_DIR)/verify
	$(PYTHON_RUNNER) scripts/00_setup/generate_data_hashes.py --config $(CONFIG_FILE) --output $(DATA_CHECKSUM_LOG)
	@echo "[verify] Enforcing real-data chapter benchmarks and Chapter 7 diagnostics."
	$(PYTHON_RUNNER) scripts/00_setup/verify_outputs.py --mode real --chapters 4,5,6,7

clean:
	@echo "[clean] Removing chapter outputs and logs."
	rm -rf $(CHAPTER4_OUTPUTS_DIR) $(CHAPTER5_OUTPUTS_DIR) $(CHAPTER6_OUTPUTS_DIR) $(CHAPTER7_OUTPUTS_DIR)
	rm -rf outputs/chapter4 outputs/chapter5 outputs/chapter6 outputs/chapter7 outputs/chapter8
	rm -rf $(LOGS_DIR)

all: validate-config chapter4 chapter5 chapter6 chapter7

run-all:
	@$(R_RUNNER) run_all.R

thesis:
	@echo "[thesis] Packaging chapter outputs for thesis build."
	@echo "[thesis] Ensuring thesis R package dependencies are installed."
	@$(R_PACKAGE_INSTALLER) --packages "$(THESIS_PACKAGES)"
	@$(R_RUNNER) $(THESIS_PACK_SCRIPT)
	@echo "[thesis] Rendering thesis artefacts."
	@$(R_RUNNER) $(THESIS_RENDER_SCRIPT)

visualise:
	@echo "[visualise] Generating network visualisations from proxy data."
	@mkdir -p outputs/visualisation
	$(R_RUNNER) scripts/visualisation/plot_network_panel.R $(PROXY_DATA_DIR) outputs/visualisation
	$(R_RUNNER) scripts/visualisation/plot_network_stability.R $(PROXY_DATA_DIR) outputs/visualisation
	@echo "[visualise] All plots saved to outputs/visualisation/"

portfolio: validate-config
	@if [ "$(DATA_MODE)" != "proxy" ]; then \
		echo "[portfolio] Refusing to generate public artifacts from DATA_MODE=$(DATA_MODE). Re-run with DATA_MODE=proxy or SAND_DATA_MODE=proxy." >&2; \
		exit 1; \
	fi
	@if [ ! -f "$(LIST_BY_WAVE_FILE)" ] || [ ! -f "$(CHAPTER4_PROXY_MARKER)" ]; then \
		$(MAKE) DATA_MODE=proxy proxy-data; \
	fi
	@echo "[portfolio] Ensuring portfolio R package dependencies are installed."
	@$(R_PACKAGE_INSTALLER) --packages "$(PORTFOLIO_PACKAGES)"
	$(R_RUNNER) scripts/portfolio/generate_validation_dashboard.R
	$(R_RUNNER) scripts/portfolio/generate_data_dictionary.R
	$(R_RUNNER) scripts/portfolio/generate_repro_manifest.R
	$(R_RUNNER) scripts/visualisation/05_interactive_networks.R $(PROXY_DATA_DIR)
	@$(MAKE) DATA_MODE=proxy privacy-check
	@echo "[portfolio] Public artifacts generated under $(PORTFOLIO_OUTPUTS_DIR) and $(PORTFOLIO_NETWORKS_DIR)."

privacy-check:
	@if [ -d "$(PORTFOLIO_OUTPUTS_DIR)" ]; then \
		$(R_RUNNER) scripts/portfolio/check_privacy.R $(PORTFOLIO_OUTPUTS_DIR); \
	else \
		echo "[privacy] Missing $(PORTFOLIO_OUTPUTS_DIR); run 'make DATA_MODE=proxy portfolio' first." >&2; \
		exit 1; \
	fi
	@if [ -d "$(PORTFOLIO_NETWORKS_DIR)" ]; then \
		$(R_RUNNER) scripts/portfolio/check_privacy.R $(PORTFOLIO_NETWORKS_DIR); \
	else \
		echo "[privacy] Missing $(PORTFOLIO_NETWORKS_DIR); run 'make DATA_MODE=proxy portfolio' first." >&2; \
		exit 1; \
	fi

portfolio-test:
	@if [ "$(DATA_MODE)" != "proxy" ]; then \
		echo "[portfolio-test] Tests require DATA_MODE=proxy. Re-run with DATA_MODE=proxy or SAND_DATA_MODE=proxy." >&2; \
		exit 1; \
	fi
	@if [ ! -f "$(LIST_BY_WAVE_FILE)" ] || [ ! -f "$(CHAPTER4_PROXY_MARKER)" ]; then \
		$(MAKE) DATA_MODE=proxy proxy-data; \
	fi
	@$(R_PACKAGE_INSTALLER) --packages "$(PORTFOLIO_PACKAGES)"
	$(R_RUNNER) tests/portfolio/testthat.R
