CPT_batch_installation_%PKG_VERSION%.exe /SP- /VERYSILENT /NOCANCEL /SUPPRESSMSGBOXES /NORESTART /NOCLOSEAPPLICATIONS /NORESTARTAPPLICATIONS /COMPONENTS=program,data /TASKS="" /DIR="%PREFIX%\Library\opt\cpt" || goto :error

del "%PREFIX%\Library\opt\cpt\unins*" || goto :error

REM mklink "%PREFIX%\Library\bin\CPT_batch.exe" "%PREFIX%\Library\opt\cpt\CPT_batch.exe" || goto :error

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
