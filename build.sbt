lazy val root =
	(project in file(".")).
		settings(
			name := "tic-tac-toe",
			version := "0.0.1",
			organization := "com.github.ikio",
			scalaVersion := "2.11.4",
			libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test",
			assemblyJarName in assembly := "tic-tac-toe.jar"
		)
