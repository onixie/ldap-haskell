# arch-tag: Python script to generate Cabal build file

from distutils.sysconfig import *
import sys

incpath = get_python_inc()
libpath = get_python_lib()
otherlibpath = get_config_var('LIBDIR')
libpaths = "%s, %s" % (otherlibpath, libpath)
libname = "python%d.%d" % (sys.version_info[0], sys.version_info[1])

print " *** Generating MissingPy.cabal based on these settings"
print " *** Please edit MissingPy.cabal if the detected settings are"
print " *** incorrect."

print "Include path for Python headers:", incpath
print "Library paths for Python library:", libpaths
print "Python library name:", libname

cabalfile = """-- THIS FILE IS AUTOMATICALLY GENERATED BY gencabal.py
-- Begin detected settings section (edit these if wrong)
Include-Dirs: %(incpath)s
Extra-Libraries: %(libname)s
Extra-Lib-Dirs: %(libpaths)s
-- End detected settings section.  Everything below here should not
-- need editing.
CC-Options: -Iglue
Name: MissingPy
Version: 0.8.0
License: GPL
Maintainer: John Goerzen <jgoerzen@complete.org>
Stability: Alpha
Copyright: Copyright (c) 2005 John Goerzen
C-Sources: glue/glue.c
Exposed-Modules: Python.Types,
 Python.Utils,
 Python.Objects,
 Python.Interpreter,
 Python.Exceptions,
 Python.Exceptions.ExcTypes,
 Python.Objects.File,
 Python.Objects.Dict,
 MissingPy.FileArchive.GZip,
 MissingPy.FileArchive.BZip2,
 MissingPy.AnyDBM
Other-Modules: Python.ForeignImports
Build-Depends: haskell-src, MissingH>=0.9.0
GHC-Options: -O2
Extensions: ForeignFunctionInterface, TypeSynonymInstances
"""

fd = open("MissingPy.cabal", "wt")
fd.write(cabalfile % globals())
fd.close()

