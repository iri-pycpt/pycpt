unset CPT_BIN_DIR
if [[ -n "$_CONDA_SET_CPT_BIN_DIR" ]]; then
    export CPT_BIN_DIR="$_CONDA_SET_CPT_BIN_DIR"
    unset _CONDA_SET_CPT_BIN_DIR
fi
