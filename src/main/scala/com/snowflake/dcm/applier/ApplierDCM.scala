package com.snowflake.dcm.applier

import scala.sys.process.Process

object ApplierDCM {

  def createProject(projectName: String): Unit = {
    Process(s"snow dcm create ${projectName} --if-not-exists").!!
  }


}
