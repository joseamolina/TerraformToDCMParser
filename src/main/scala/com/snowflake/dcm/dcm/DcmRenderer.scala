package com.snowflake.dcm.dcm

/**
 * Renders DcmStatement values to SQL strings compatible with Snowflake DCM
 * projects.
 *
 * DCM syntax follows Snowflake SQL CREATE syntax, but uses the DEFINE keyword.
 * Grant statements use standard Snowflake GRANT syntax (no DEFINE).
 * Jinja2 template variables ({{ name }}) are preserved as-is.
 */
object DcmRenderer {

  def render(stmt: DcmStatement): String = stmt match {
    case s: DefineDatabase     => renderDatabase(s)
    case s: DefineSchema       => renderSchema(s)
    case s: DefineWarehouse    => renderWarehouse(s)
    case s: DefineRole         => renderRole(s)
    case s: DefineTable        => renderTable(s)
    case s: DefineView         => renderView(s)
    case s: DefineDynamicTable => renderDynamicTable(s)
    case s: GrantPrivileges    => renderGrantPrivileges(s)
    case s: GrantRole          => renderGrantRole(s)
    case s: GrantOwnership     => renderGrantOwnership(s)
    case s: GrantAllPrivileges => renderGrantAllPrivileges(s)
    case s: GrantOnAll         => renderGrantOnAll(s)
    case s: GrantOnFuture      => renderGrantOnFuture(s)
    case s: UnsupportedResource =>
      s"-- UNSUPPORTED: ${s.resourceType}.${s.resourceName} — ${s.reason}"
  }

  // ─── Database ─────────────────────────────────────────────────────────────

  private def renderDatabase(d: DefineDatabase): String = {
    val kw    = if (d.isTransient) "DEFINE TRANSIENT DATABASE" else "DEFINE DATABASE"
    val props = Seq(
      d.dataRetentionDays.map(n => s"  DATA_RETENTION_TIME_IN_DAYS = $n"),
      d.maxDataExtensionDays.map(n => s"  MAX_DATA_EXTENSION_TIME_IN_DAYS = $n"),
      d.defaultDdlCollation.map(c => s"  DEFAULT_DDL_COLLATION = '${esc(c)}'"),
      d.storageSerializationPolicy.map(p => s"  STORAGE_SERIALIZATION_POLICY = $p"),
      d.logLevel.map(l => s"  LOG_LEVEL = '$l'"),
      d.traceLevel.map(t => s"  TRACE_LEVEL = '$t'"),
      d.comment.map(c => s"  COMMENT = '${esc(c)}'")
    ).flatten
    buildStatement(s"$kw ${d.name}", props)
  }

  // ─── Schema ───────────────────────────────────────────────────────────────

  private def renderSchema(s: DefineSchema): String = {
    val kw    = if (s.isTransient) "DEFINE TRANSIENT SCHEMA" else "DEFINE SCHEMA"
    val ma    = if (s.withManagedAccess) Some("  WITH MANAGED ACCESS") else None
    val props = Seq(
      ma,
      s.dataRetentionDays.map(n => s"  DATA_RETENTION_TIME_IN_DAYS = $n"),
      s.defaultDdlCollation.map(c => s"  DEFAULT_DDL_COLLATION = '${esc(c)}'"),
      s.comment.map(c => s"  COMMENT = '${esc(c)}'")
    ).flatten
    buildStatement(s"$kw ${s.name}", props)
  }

  // ─── Warehouse ────────────────────────────────────────────────────────────

  private def renderWarehouse(w: DefineWarehouse): String = {
    val props = Seq(
      w.warehouseType.map(t => s"  WAREHOUSE_TYPE = '$t'"),
      w.warehouseSize.map(s => s"  WAREHOUSE_SIZE = '$s'"),
      w.maxClusterCount.map(n => s"  MAX_CLUSTER_COUNT = $n"),
      w.minClusterCount.map(n => s"  MIN_CLUSTER_COUNT = $n"),
      w.scalingPolicy.map(p => s"  SCALING_POLICY = '$p'"),
      w.autoSuspend.map(n => s"  AUTO_SUSPEND = $n"),
      w.autoResume.map(b => s"  AUTO_RESUME = ${b.toString.toUpperCase}"),
      w.initiallySuspended.map(b => s"  INITIALLY_SUSPENDED = ${b.toString.toUpperCase}"),
      w.enableQueryAcceleration.map(b => s"  ENABLE_QUERY_ACCELERATION = ${b.toString.toUpperCase}"),
      w.queryAccelerationMaxScaleFactor.map(n => s"  QUERY_ACCELERATION_MAX_SCALE_FACTOR = $n"),
      w.resourceMonitor.map(m => s"  RESOURCE_MONITOR = '$m'"),
      w.comment.map(c => s"  COMMENT = '${esc(c)}'")
    ).flatten
    buildStatement(s"DEFINE WAREHOUSE ${w.name}", props)
  }

  // ─── Role ─────────────────────────────────────────────────────────────────

  private def renderRole(r: DefineRole): String = {
    val props = r.comment.map(c => s"  COMMENT = '${esc(c)}'").toSeq
    buildStatement(s"DEFINE ROLE ${r.name}", props)
  }

  // ─── Table ────────────────────────────────────────────────────────────────

  private def renderTable(t: DefineTable): String = {
    val kw   = if (t.isTransient) "DEFINE TRANSIENT TABLE" else "DEFINE TABLE"
    val cols = t.columns.map(renderColumn)
    val colBlock =
      if (cols.isEmpty) "()"
      else cols.mkString("(\n    ", ",\n    ", "\n  )")

    val props = Seq(
      t.dataRetentionDays.map(n => s"  DATA_RETENTION_TIME_IN_DAYS = $n"),
      t.comment.map(c => s"  COMMENT = '${esc(c)}'")
    ).flatten

    val head = s"$kw ${t.name}"
    if (props.isEmpty) s"$head $colBlock;"
    else s"$head $colBlock\n${props.mkString("\n")};"
  }

  private def renderColumn(c: ColumnDef): String = {
    val sb      = new StringBuilder
    sb.append(c.name).append(" ").append(c.dataType)
    if (!c.nullable) sb.append(" NOT NULL")
    c.defaultValue.foreach(d => sb.append(s" DEFAULT $d"))
    c.comment.foreach(cm => sb.append(s" COMMENT '${esc(cm)}'"))
    sb.toString()
  }

  // ─── View ─────────────────────────────────────────────────────────────────

  private def renderView(v: DefineView): String = {
    val comment = v.comment.map(c => s"\n  COMMENT = '${esc(c)}'").getOrElse("")
    s"DEFINE VIEW ${v.name}$comment\nAS\n${v.query.trim};"
  }

  // ─── Dynamic Table ────────────────────────────────────────────────────────

  private def renderDynamicTable(dt: DefineDynamicTable): String = {
    val lagStr = if (dt.targetLag.toUpperCase == "DOWNSTREAM") "DOWNSTREAM" else s"'${dt.targetLag}'"
    val comment = dt.comment.map(c => s"\n  COMMENT = '${esc(c)}'").getOrElse("")
    s"""DEFINE DYNAMIC TABLE ${dt.name}
  TARGET_LAG = $lagStr
  WAREHOUSE = ${dt.warehouse}$comment
AS
${dt.query.trim};"""
  }

  // ─── Grant statements ─────────────────────────────────────────────────────

  private def renderGrantPrivileges(g: GrantPrivileges): String = {
    val privStr = g.privileges.mkString(", ")
    val target  =
      if (g.objectName.isEmpty) g.objectType
      else s"${g.objectType} ${g.objectName}"
    val opt = if (g.withGrantOption) " WITH GRANT OPTION" else ""
    s"GRANT $privStr ON $target TO ROLE ${g.roleName}$opt;"
  }

  private def renderGrantRole(g: GrantRole): String =
    s"GRANT ROLE ${g.roleName} TO ROLE ${g.parentRole};"

  private def renderGrantOwnership(g: GrantOwnership): String =
    s"GRANT OWNERSHIP ON ${g.objectType} ${g.objectName} TO ROLE ${g.roleName};"

  private def renderGrantAllPrivileges(g: GrantAllPrivileges): String =
    s"GRANT ALL PRIVILEGES ON ${g.objectType} ${g.objectName} TO ROLE ${g.roleName};"

  private def renderGrantOnAll(g: GrantOnAll): String = {
    val privStr = g.privileges.mkString(", ")
    s"GRANT $privStr ON ALL ${g.objectTypePlural} IN ${g.containerType} ${g.containerName} TO ROLE ${g.roleName};"
  }

  private def renderGrantOnFuture(g: GrantOnFuture): String = {
    val privStr = g.privileges.mkString(", ")
    s"GRANT $privStr ON FUTURE ${g.objectTypePlural} IN ${g.containerType} ${g.containerName} TO ROLE ${g.roleName};"
  }

  // ─── Utilities ────────────────────────────────────────────────────────────

  /** Escape single quotes inside SQL string literals. */
  private def esc(s: String): String = s.replace("'", "''")

  /**
   * Build a DEFINE statement: if there are property lines, put them on
   * subsequent lines; otherwise end the statement on the same line.
   */
  private def buildStatement(header: String, props: Seq[String]): String =
    if (props.isEmpty) s"$header;"
    else s"$header\n${props.mkString("\n")};"
}
