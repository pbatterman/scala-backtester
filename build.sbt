name := "Backtester"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies  ++= Seq(
  // Last snapshot
  "org.scalanlp" %% "breeze" % "latest.integration",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.13-0598e003cfa7f00f76919aa556009ad6d4fc1332",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code.
  "org.scalanlp" %% "breeze-viz" % "0.13-0598e003cfa7f00f76919aa556009ad6d4fc1332"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"