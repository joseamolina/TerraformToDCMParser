package com.snowflake.dcm.terraform

import com.snowflake.dcm.hcl.{HclFile, HclParser, HclValue}

import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.mutable

/**
 * Discovers all *.tf files under a directory, parses them with HclParser,
 * and assembles a TerraformProject.
 */
object TerraformLoader {

  def loadDirectory(dir: File): TerraformProject = {
    val tfFiles = listTfFiles(dir)
    if (tfFiles.isEmpty) {
      System.err.println(s"[WARN] No *.tf files found under ${dir.getAbsolutePath}")
    }
    val files = tfFiles.map { f =>
      val content = new String(Files.readAllBytes(f.toPath), "UTF-8")
      (f.getName, HclParser.parse(content))
    }
    buildProject(files)
  }

  def loadString(content: String, fileName: String = "<inline>"): TerraformProject =
    buildProject(List((fileName, HclParser.parse(content))))

  // ─── Discovery ────────────────────────────────────────────────────────────

  private def listTfFiles(root: File): List[File] = {
    if (!root.exists() || !root.isDirectory)
      throw new IllegalArgumentException(s"Not a directory: ${root.getAbsolutePath}")

    def recurse(f: File): List[File] =
      if (f.isDirectory) f.listFiles().toList.flatMap(recurse)
      else if (f.getName.endsWith(".tf")) List(f)
      else Nil

    recurse(root).sortBy(_.getAbsolutePath)
  }

  // ─── Project assembly ─────────────────────────────────────────────────────

  private def buildProject(files: List[(String, HclFile)]): TerraformProject = {
    // resourceType → resourceName → resource
    val resources = mutable.HashMap[String, mutable.HashMap[String, TerraformResource]]()
    val variables = mutable.HashMap[String, TerraformVariable]()
    val locals    = mutable.HashMap[String, TerraformLocal]()

    files.foreach { case (fileName, hclFile) =>
      hclFile.blocks.foreach { block =>
        block.blockType match {

          case "resource" if block.labels.size >= 2 =>
            val rType = block.labels(0)
            val rName = block.labels(1)
            val resource = TerraformResource(
              resourceType  = rType,
              resourceName  = rName,
              attributes    = block.body.attributes,
              nestedBlocks  = block.body.blocks
            )
            resources.getOrElseUpdate(rType, mutable.HashMap()).put(rName, resource)

          case "variable" if block.labels.nonEmpty =>
            val name = block.labels.head
            variables(name) = TerraformVariable(
              name        = name,
              varType     = block.body.strAttr("type"),
              default     = block.body.attr("default"),
              description = block.body.strAttr("description")
            )

          case "locals" =>
            block.body.attributes.foreach { case (k, v) =>
              locals(k) = TerraformLocal(k, v)
            }

          case "terraform" | "provider" | "data" | "module" | "output" =>
            // Not converted; silently skip

          case other =>
            System.err.println(s"[$fileName] Skipping unsupported top-level block: $other")
        }
      }
    }

    TerraformProject(
      resources = resources.map { case (k, v) => k -> v.toMap }.toMap,
      variables = variables.toMap,
      locals    = locals.toMap
    )
  }
}
