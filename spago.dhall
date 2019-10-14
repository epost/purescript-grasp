{ name =
	"grasp"
, dependencies =
	[ "console", "debug", "effect", "parsing", "psci-support", "spec" ]
, packages =
	./packages.dhall
, sources =
	[ "src/**/*.purs", "test/**/*.purs" ]
}