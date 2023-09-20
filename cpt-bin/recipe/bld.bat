REM When building from URL, conda-build unpacks automatically, but
REM when building from local tarball it doesn't, so we do it
REM ourselves, stripping the first path component to match
REM conda-build's behavior.
REM tar --strip-components=1 zxf "CPT.%PKG_VERSION%.tar.gz" || goto :error

cd "%PKG_VERSION%" || goto :error

set INSTALL_DIR="%PREFIX%\Library\cpt"
mkdir "%INSTALL_DIR%" || goto :error

patch -i "%RECIPE_DIR%\patch" || goto :error
copy lapack\lapack\make.inc.example lapack\lapack\make.inc || goto :error
make || goto :error

copy CPT.x "%INSTALL_DIR%\CPT_batch.exe" || goto :error
copy cpt.ini "%INSTALL_DIR%" || goto error
mkdir "%INSTALL_DIR%\data" || goto :error
copy data\labels.txt "%INSTALL_DIR%\data" || goto :error
copy data\download_IRIDL.txt "%INSTALL_DIR%\data" || goto :error

set MSYS_DLL_DIR=c:\msys64\usr\bin
copy "%MSYS_DLL_DIR%\msys-gfortran-5.dll" "%INSTALL_DIR%" || goto :error
copy "%MSYS_DLL_DIR%\msys-2.0.dll" "%INSTALL_DIR%" || goto :error
copy "%MSYS_DLL_DIR%\msys-gcc_s-seh-1.dll" "%INSTALL_DIR%" || goto :error
copy "%MSYS_DLL_DIR%\msys-quadmath-0.dll" "%INSTALL_DIR%" || goto :error

mkdir "%PREFIX%\etc\conda\activate.d" || goto :error
copy "%RECIPE_DIR%\activate.bat" "%PREFIX%\etc\conda\activate.d\%PKG_NAME%_activate.bat" || goto :error
copy "%RECIPE_DIR%\activate.ps1" "%PREFIX%\etc\conda\activate.d\%PKG_NAME%_activate.ps1" || goto :error
copy "%RECIPE_DIR%\activate.sh" "%PREFIX%\etc\conda\activate.d\%PKG_NAME%_activate.sh" || goto :error

mkdir "%PREFIX%\etc\conda\deactivate.d" || goto :error
copy "%RECIPE_DIR%\deactivate.bat" "%PREFIX%\etc\conda\deactivate.d\%PKG_NAME%_deactivate.bat" || goto :error
copy "%RECIPE_DIR%\deactivate.ps1" "%PREFIX%\etc\conda\deactivate.d\%PKG_NAME%_deactivate.ps1" || goto :error
copy "%RECIPE_DIR%\deactivate.sh" "%PREFIX%\etc\conda\deactivate.d\%PKG_NAME%_deactivate.sh" || goto :error

goto :EOF

:error
echo errorlevel %errorlevel%
exit /b %errorlevel%
