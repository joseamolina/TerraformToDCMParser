package com.snowflake.dcm.terraform

import com.snowflake.dcm.hcl.{HclBlock, HclValue}

/**
 * Domain model representing a parsed Terraform project.
 */

/** A single `resource "type" "name" { … }` block */
final case class TerraformResource(
  resourceType: String,
  resourceName: String,
  attributes:   Map[String, HclValue],
  nestedBlocks: List[HclBlock]
) {
  def attr(name: String): Option[HclValue]  = attributes.get(name)
  def str(name: String): Option[String]     = attr(name).map(HclValue.asString)
  def bool(name: String): Option[Boolean]   = attr(name).collect { case HclValue.Bool(b) => b }
  def num(name: String): Option[Double]     = attr(name).collect { case HclValue.Num(n) => n }
  def list(name: String): List[String]      = attr(name) match {
    case Some(HclValue.Lst(items)) => items.map(HclValue.asString)
    case _                         => Nil
  }
  def block(name: String): Option[HclBlock]   = nestedBlocks.find(_.blockType == name)
  def allBlocks(name: String): List[HclBlock] = nestedBlocks.filter(_.blockType == name)
}

/** A `variable "name" { … }` declaration */
final case class TerraformVariable(
  name:        String,
  varType:     Option[String],
  default:     Option[HclValue],
  description: Option[String]
)

/** A `locals { key = value … }` entry */
final case class TerraformLocal(
  name:  String,
  value: HclValue
)

/**
 * The full set of parsed Terraform resources, variables, and locals.
 *
 * resources: resourceType → resourceName → TerraformResource
 */
final case class TerraformProject(
  resources: Map[String, Map[String, TerraformResource]],
  variables: Map[String, TerraformVariable],
  locals:    Map[String, TerraformLocal]
) {
  def resourcesOfType(t: String): List[TerraformResource] =
    resources.getOrElse(t, Map.empty).values.toList.sortBy(_.resourceName)
}
