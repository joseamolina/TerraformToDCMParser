ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.21"

lazy val root = (project in file("."))
  .settings(
    name := "parserTerraformToDCM",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt"      % "4.1.0",
      "org.scalatest"    %% "scalatest"  % "3.2.17" % Test
    ),
    Compile / mainClass   := Some("com.snowflake.dcm.Main"),
    assembly / mainClass    := Some("com.snowflake.dcm.Main"),
    assembly / assemblyJarName := "terraform-to-dcm.jar"
  )

