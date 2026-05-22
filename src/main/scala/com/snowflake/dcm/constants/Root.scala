package com.snowflake.dcm.constants

import java.io.File

class Root extends App  {

  val programName: String = "terraform-to-dcm"
  val descriptionTask: String = "Converts Snowflake Terraform configurations to Snowflake DCM projects.\n"

  case class Config(
                     inputDir:          File   = new File("."),
                     outputDir:         File   = new File("dcm_output"),
                     projectName:       String = "migrated_project",
                     description:       String = "Migrated from Terraform",
                     defaultTarget:     String = "DCM_DEV",
                     accountIdentifier: String = "",
                     targetProjectName: String = ""
                   )




}
