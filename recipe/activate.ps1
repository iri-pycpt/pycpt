if ($env:CPT_BIN_DIR) {
    $env:_CONDA_SET_CPT_BIN_DIR=$env:CPT_BIN_DIR
}

$env:CPT_BIN_DIR="$env:CONDA_PREFIX\Library\opt\cpt"
