cd log
sh build.bat
cd ..

cd ucs
sh build.bat
sh deploy.bat
cd ..

cd wbxml
sh build.bat
cd ..

cd xml
sh build.bat
cd ..

erlc +debug_info -o ebin syncserver.erl

