// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 

// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSql"
let projectApi = "FSql"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Lint tool for T-SQL."
let summaryApi = "FSql Api (Lint tool for T-SQL)."

// List of author names (for NuGet package)
let authors = [ "Alistair Singh" ]

let version = "0.1"
let apiVersion = "0.1-alpha"

let packagingRoot = "./packaging/"
let toolPackagingDir = packagingRoot @@ "tool"
let apiPackagingDir = packagingRoot @@ "api"

// File system information 
// (<solutionFile>.sln is built during the building process)
let solutionFile  = "FSql"
// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/alistair-singh/FSql"
// The name of the project on GitHub
let gitName = "FSql"

// Generate assembly info files with the right version & up-to-date information
let genAssemblyInfo (projectPath) =
    let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
    let basePath = "src/" + projectName
    let fileName = basePath + "/AssemblyInfo.fs"
    CreateFSharpAssemblyInfo fileName
      [ Attribute.Title project
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version version
        Attribute.FileVersion version ]

Target "AssemblyInfo" (fun _ ->
    !! "src/**/*.fsproj"
        |> Seq.iter genAssemblyInfo)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" RestorePackages

Target "Clean" (fun _ ->
    CleanDirs ["bin"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    !! (solutionFile + ".sln")
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    !! testAssemblies 
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

#I @"packages/FSharpLint/"
#r @"packages/FSharpLint/FSharpLint.FAKE.dll"
open FSharpLint.FAKE

Target "Lint" (fun _ ->
    !! "src/**/*.fsproj"
        |> Seq.iter (FSharpLint id))

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean" ==> 
    "RestorePackages" ==> 
    "AssemblyInfo" ==> 
    "Build" ==> 
    //"Lint" ==>
    "RunTests" ==> "All"

RunTargetOrDefault "All"
