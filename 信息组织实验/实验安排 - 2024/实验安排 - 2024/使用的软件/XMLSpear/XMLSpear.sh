#!/bin/sh

cd "${0%/*}"
##
## first changedir to the directory of this script
## extra parameters will be interpreted as files that will be opened on startup
echo $PWD
java -Xms128m -Xmx1024m -Dcom.apple.mrj.application.apple.menu.about.name=XMLSpear -classpath lib/loader.jar com.dd.classloader.Loader XMLSpear.properties