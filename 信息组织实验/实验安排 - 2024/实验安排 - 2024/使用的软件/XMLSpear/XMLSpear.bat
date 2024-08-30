REM changedir to the directory of this bat file
cd "%~dp0"

REM start XMLSpear
REM java 1.5X or higher must be installed
REM you may also use the complete path to a java version for instance:
REM "c:\Program Files\Java\jre1.5.0_06\bin\java"  -cp lib/loader.jar com.dd.classloader.Loader XMLSpear.properties
REM extra parameters will be interpreted as files that will be opened on startup

java -Xms256m -Xmx1024m -Dsun.java2d.d3d=false -cp lib/loader.jar com.dd.classloader.Loader XMLSpear.properties
