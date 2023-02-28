set -x

cd "$SRC_DIR/$PKG_VERSION"

# get rid of build artifacts Simon accidentally included in source dist
find . -name "*.o" -print0 | xargs -0 rm

cp lapack/lapack/make.inc.example lapack/lapack/make.inc
make FC="$FC" INSTALL_DIR="$PREFIX/opt/cpt" install
mkdir -p "$PREFIX/bin"
ln -s "$PREFIX/opt/cpt/bin/CPT.x" "$PREFIX/bin"


for CHANGE in "activate" "deactivate"
do
    mkdir -p "${PREFIX}/etc/conda/${CHANGE}.d"
    cp "${RECIPE_DIR}/${CHANGE}.sh" "${PREFIX}/etc/conda/${CHANGE}.d/${PKG_NAME}_${CHANGE}.sh"
done
