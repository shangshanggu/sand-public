#!/usr/bin/env bash
# run_ch4_to_ch7_clean.sh — Clean pipeline run: Chapters 4-7 only
#
# Usage:
#   nohup bash reproduced/run_ch4_to_ch7_clean.sh > reproduced/logs/ch4_to_ch7_run.log 2>&1 &
#
# Monitor:
#   tail -f reproduced/logs/ch4_to_ch7_run.log
#   cat reproduced/logs/pipeline_progress.txt

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PROGRESS_FILE="$REPO_ROOT/reproduced/logs/pipeline_progress.txt"
LOG_DIR="$REPO_ROOT/reproduced/logs"
mkdir -p "$LOG_DIR"

log_progress() {
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    echo "$msg"
    echo "$msg" >> "$PROGRESS_FILE"
}

> "$PROGRESS_FILE"
log_progress "=== Clean pipeline started: Chapters 4-7 ==="
log_progress "Working directory: $REPO_ROOT/reproduced"

cd "$REPO_ROOT/reproduced"

# --- Chapter 4 ---
log_progress "CHAPTER 4: Starting data collection pipeline"
make chapter4 2>&1
log_progress "CHAPTER 4: COMPLETE"

# --- Chapter 5 ---
log_progress "CHAPTER 5: Starting descriptive norms (NAM)"
make chapter5 2>&1
log_progress "CHAPTER 5: COMPLETE"

# --- Chapter 6 ---
log_progress "CHAPTER 6: Starting injunctive norms"
make chapter6 2>&1
log_progress "CHAPTER 6: COMPLETE"

# --- Chapter 7 ---
log_progress "CHAPTER 7: Starting SAOM estimation (32-core parallel, legacy covariates)"
make chapter7 2>&1
log_progress "CHAPTER 7: COMPLETE"

log_progress "=== Pipeline finished: Chapters 4-7 complete ==="
log_progress "Outputs at: $REPO_ROOT/reproduced/outputs/"
