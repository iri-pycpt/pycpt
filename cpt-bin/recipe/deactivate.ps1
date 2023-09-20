remove-item env:CPT_BIN_DIR -ErrorAction Ignore
if ($env:_CONDA_SET_CPT_BIN_DIR) {
    $env:CPT_BIN_DIR="$env:_CONDA_SET_CPT_BIN_DIR"
    remove-item env:_CONDA_SET_CPT_BIN_DIR
}
