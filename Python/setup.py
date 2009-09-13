from distutils.core import setup, Extension

import time

version = time.strftime("%Y%m%d", time.gmtime())

setup(name="pfool",
      version=version,
      packages=["pfool"],
      package_dir = { "pfool": "" },
      ext_package="pfool"
      )
