package net.jpbougie

import net.lag.configgy.Configgy
import net.lag.logging.Logger
import net.lag.FilterableSpecsFileRunner


object TestRunner extends FilterableSpecsFileRunner("src/test/scala/**/*.scala") {
//  Configgy.configure("src/resources/test.conf")
  if (System.getProperty("debugtrace") == null) {
    Logger.get("").setLevel(Logger.FATAL)
  } else {
    Logger.get("").setLevel(Logger.TRACE)
  }
}
