package com.snowflake.dcm.dcm

/**
 * DCM output model – one case class per supported statement type.
 *
 * All name fields carry fully-qualified identifiers where applicable
 * (e.g. "MY_DB.MY_SCHEMA.MY_TABLE").
 */

sealed trait DcmStatement

// ── Object definitions ────────────────────────────────────────────────────────

final case class DefineDatabase(
  name:                       String,
  isTransient:                Boolean           = false,
  dataRetentionDays:          Option[Int]       = None,
  maxDataExtensionDays:       Option[Int]       = None,
  defaultDdlCollation:        Option[String]    = None,
  storageSerializationPolicy: Option[String]    = None,
  logLevel:                   Option[String]    = None,
  traceLevel:                 Option[String]    = None,
  comment:                    Option[String]    = None
) extends DcmStatement

final case class DefineSchema(
  name:                String,             // fully-qualified: DB.SCHEMA
  isTransient:         Boolean             = false,
  withManagedAccess:   Boolean             = false,
  dataRetentionDays:   Option[Int]         = None,
  defaultDdlCollation: Option[String]      = None,
  comment:             Option[String]      = None
) extends DcmStatement

final case class DefineWarehouse(
  name:                          String,
  warehouseType:                 Option[String]  = None,
  warehouseSize:                 Option[String]  = None,
  maxClusterCount:               Option[Int]     = None,
  minClusterCount:               Option[Int]     = None,
  scalingPolicy:                 Option[String]  = None,
  autoSuspend:                   Option[Int]     = None,
  autoResume:                    Option[Boolean] = None,
  initiallySuspended:            Option[Boolean] = None,
  enableQueryAcceleration:       Option[Boolean] = None,
  queryAccelerationMaxScaleFactor: Option[Int]   = None,
  resourceMonitor:               Option[String]  = None,
  comment:                       Option[String]  = None
) extends DcmStatement

final case class DefineRole(
  name:    String,
  comment: Option[String] = None
) extends DcmStatement

final case class ColumnDef(
  name:         String,
  dataType:     String,
  nullable:     Boolean         = true,
  defaultValue: Option[String]  = None,
  comment:      Option[String]  = None
)

final case class DefineTable(
  name:              String,          // DB.SCHEMA.TABLE
  isTransient:       Boolean          = false,
  columns:           List[ColumnDef]  = Nil,
  dataRetentionDays: Option[Int]      = None,
  comment:           Option[String]   = None
) extends DcmStatement

final case class DefineView(
  name:    String,          // DB.SCHEMA.VIEW
  query:   String,
  comment: Option[String]   = None
) extends DcmStatement

final case class DefineDynamicTable(
  name:       String,          // DB.SCHEMA.DT
  targetLag:  String,          // e.g. "1 minute" or "DOWNSTREAM"
  warehouse:  String,
  query:      String,
  comment:    Option[String]   = None
) extends DcmStatement

// ── Grant statements ──────────────────────────────────────────────────────────

/** GRANT <privileges> ON <objectType> <objectName> TO ROLE <roleName> */
final case class GrantPrivileges(
  privileges:      List[String],
  objectType:      String,
  objectName:      String,
  roleName:        String,
  withGrantOption: Boolean         = false
) extends DcmStatement

/** GRANT ROLE <roleName> TO ROLE <parentRole> */
final case class GrantRole(
  roleName:   String,
  parentRole: String
) extends DcmStatement

/** GRANT OWNERSHIP ON <objectType> <objectName> TO ROLE <roleName> */
final case class GrantOwnership(
  objectType: String,
  objectName: String,
  roleName:   String
) extends DcmStatement

/** GRANT ALL PRIVILEGES ON <objectType> <objectName> TO ROLE <roleName> */
final case class GrantAllPrivileges(
  objectType: String,
  objectName: String,
  roleName:   String
) extends DcmStatement

/** GRANT <privileges> ON ALL <objectTypePlural> IN {DATABASE|SCHEMA} <container> TO ROLE <roleName> */
final case class GrantOnAll(
  privileges:        List[String],
  objectTypePlural:  String,
  containerType:     String,    // DATABASE or SCHEMA
  containerName:     String,
  roleName:          String
) extends DcmStatement

/** GRANT <privileges> ON FUTURE <objectTypePlural> IN {DATABASE|SCHEMA} <container> TO ROLE <roleName> */
final case class GrantOnFuture(
  privileges:        List[String],
  objectTypePlural:  String,
  containerType:     String,
  containerName:     String,
  roleName:          String
) extends DcmStatement

/** Catch-all for unrecognised / unsupported resource types */
final case class UnsupportedResource(
  resourceType: String,
  resourceName: String,
  reason:       String
) extends DcmStatement
