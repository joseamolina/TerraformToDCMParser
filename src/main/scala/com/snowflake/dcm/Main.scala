package com.snowflake.dcm

import com.snowflake.dcm.converter.TerraformToDcmConverter
import com.snowflake.dcm.dcm.DcmProjectGenerator
import com.snowflake.dcm.dcm.DcmProjectGenerator.GeneratorConfig
import com.snowflake.dcm.terraform.TerraformLoader
import com.snowflake.dcm.constants.Root

import java.io.File
import javax.swing.JRootPane

/**
 * CLI entry point.
 *
 * Usage:
 *   terraform-to-dcm [options] <input-dir>
 *
 * Options:
 *   -o, --output <dir>       Output directory (default: ./dcm_output)
 *   -n, --name <name>        DCM project name (default: migrated_project)
 *   -d, --description <str>  Project description
 *   -h, --help               Print help
 */
object Main extends Root {

  val parser = new scopt.OptionParser[Config](programName) {
    head(programName, "1.0")
    note(descriptionTask)

    arg[File]("<input-dir>")
      .required()
      .action((f, c) => c.copy(inputDir = f))
      .text("Directory containing *.tf Terraform files")

    opt[File]('o', "output")
      .action((f, c) => c.copy(outputDir = f))
      .text("Output directory for DCM project files (default: ./dcm_output)")

    opt[String]('n', "name")
      .action((s, c) => c.copy(projectName = s))
      .text("DCM project name (default: migrated_project)")

    opt[String]('d', "description")
      .action((s, c) => c.copy(description = s))
      .text("DCM project description")

    opt[String]('t', "default-target")
      .action((s, c) => c.copy(defaultTarget = s))
      .text("Default target name (default: DCM_DEV)")

    opt[String]('a', "account-identifier")
      .action((s, c) => c.copy(accountIdentifier = s))
      .text("Snowflake account identifier for the default target")

    opt[String]('p', "target-project-name")
      .action((s, c) => c.copy(targetProjectName = s))
      .text("Fully-qualified DCM project name for the default target (e.g. DB.SCHEMA.PROJECT)")

    help("help").abbr("h").text("Print this help")

    checkConfig { c =>
      if (!c.inputDir.exists())  failure(s"Input directory does not exist: ${c.inputDir.getAbsolutePath}")
      else if (!c.inputDir.isDirectory) failure(s"Input path is not a directory: ${c.inputDir.getAbsolutePath}")
      else success
    }
  }

  // Once the configuration is set, we ensure to parse the configuration to see that it's ok.
  //
  parser.parse(args, Config()) match {
    case Some(cfg) =>
      println(s"[terraform-to-dcm] Loading Terraform files from: ${cfg.inputDir.getAbsolutePath}")
      val project    = TerraformLoader.loadDirectory(cfg.inputDir)
      val statements = TerraformToDcmConverter.convert(project)

      val targets = if (cfg.accountIdentifier.nonEmpty && cfg.targetProjectName.nonEmpty)
        Map(cfg.defaultTarget -> DcmProjectGenerator.DcmTarget(cfg.accountIdentifier, cfg.targetProjectName))
      else
        Map.empty[String, DcmProjectGenerator.DcmTarget]

      DcmProjectGenerator.generate(
        statements = statements,
        outputDir  = cfg.outputDir,
        project    = project,
        config     = GeneratorConfig(
          projectName   = cfg.projectName,
          description   = cfg.description,
          defaultTarget = cfg.defaultTarget,
          targets       = targets
        )
      )

    case None =>
      // scopt printed the error
      sys.exit(1)
  }
}
