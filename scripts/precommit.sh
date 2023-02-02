#!/usr/bin/env bash

# various checks to ensure code quality before committing code

set -xeo pipefail

script_dir=`dirname "$0"`
base_dir=`dirname "$script_dir"`

declare -a projects=(
    "src"
    "lib/common"
    "lib/kritzler"
)

for project in "${projects[@]}"; do
    zig fmt --check "$base_dir/$project"
    ./scripts/80.py "$base_dir/$project"
done
