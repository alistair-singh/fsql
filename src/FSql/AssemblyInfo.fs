namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSql")>]
[<assembly: AssemblyProductAttribute("FSql")>]
[<assembly: AssemblyDescriptionAttribute("Lint tool for T-SQL.")>]
[<assembly: AssemblyVersionAttribute("0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1"
