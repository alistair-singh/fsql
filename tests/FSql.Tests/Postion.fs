namespace FSql.Tests

module Position = 
  open NUnit.Framework
  open FsUnit
  open FSql.Position

  [<Test>]
  let ``Position: initial position with no source file correct``()=
    let expected = 
      { name = None 
        line = 1
        column = 1
        absolute = 0 }
    initialPos None |> should equal expected

  [<Test>]
  let ``Position: initial position with no source file correct``()=
    let expected = 
      { name = Some "TestFile" 
        line = 1
        column = 1
        absolute = 0 }
    initialPos (Some "TestFile") |> should equal expected

