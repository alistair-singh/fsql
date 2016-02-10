namespace FSql.Tests

module Position = 
  open NUnit.Framework
  open FsUnit
  open FSql.Position
  
  [<Test>]
  let ``initialPos - with no source file correct``() = 
    let expected = 
      { name = None
        line = 1
        column = 1
        absolute = 0 }
    initialPos None |> should equal expected
  
  [<Test>]
  let ``initialPos - with source file correct``() = 
    let expected = 
      { name = Some "TestFile"
        line = 1
        column = 1
        absolute = 0 }
    initialPos (Some "TestFile") |> should equal expected
  
  [<Test>]
  let ``updatePosChar - normal character column increment``() = 
    let expected = 
      { name = None
        line = 1
        column = 2
        absolute = 1 }
    
    let initial = initialPos None
    updatePosChar initial 'x' |> should equal expected
  
  [<Test>]
  let ``updatePosChar - '\r' character absolute increment``() = 
    let expected = 
      { name = None
        line = 1
        column = 1
        absolute = 1 }
    
    let initial = initialPos None
    updatePosChar initial '\r' |> should equal expected
  
  [<Test>]
  let ``updatePosChar - '\n' character column reset, line increment``() = 
    let expected = 
      { name = None
        line = 4
        column = 1
        absolute = 10 }
    
    let initial = 
      { name = None
        line = 3
        column = 4
        absolute = 9 }
    
    updatePosChar initial '\n' |> should equal expected
  
  [<Test>]
  let ``updatePosString - "" string return same value``() = 
    let expected = 
      { name = None
        line = 4
        column = 1
        absolute = 10 }
    
    let initial = 
      { name = None
        line = 4
        column = 1
        absolute = 10 }
    
    updatePosString initial "" |> should equal expected
  
  [<Test>]
  let ``updatePosString - "abc\r\nabc\nab" string return valid position``() = 
    let expected = 
      { name = None
        line = 3
        column = 3
        absolute = 11 }
    
    let initial = initialPos None
    updatePosString initial "abc\r\nabc\nab" |> should equal expected
