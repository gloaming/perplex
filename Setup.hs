import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "perplex"
                  Nothing -- No icon
                  Nothing -- No Info.plist
                  [] -- No other resources.
                  [] -- No other binaries.
                  (const DoNotChase ChaseWithDefaults)
          ]
