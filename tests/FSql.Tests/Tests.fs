namespace FSql.Tests

module Test = 
  open NUnit.Framework
  open FsUnit
  open FSql.Parser
  open FSql.Error
  
  [<Test>]
  let ``Single char parser pass``() = 
    let expected = 
      { state = 
          { input = [ 'b'; 'c' ]
            position = 
              { name = None
                line = 1
                column = 2
                absolute = 1 } }
        result = Ok 'a'
        consumption = Consumed }
    
    let actual = runParsec' (char' 'a') (initialState None "abc")
    actual |> should equal expected
  
  [<Test>]
  let ``Single char parser fail``() = 
    let expected : Reply<char, char> = 
      { state = 
          { input = "abc"
            position = 
              { name = None
                line = 1
                column = 1
                absolute = 0 } }
        result = 
          Error { position = 
                    { name = None
                      line = 1
                      column = 1
                      absolute = 0 }
                  messages = 
                    [ Expected "'b'"
                      Unexpected "'a'" ] }
        consumption = Virgin }
    
    runParsec' (char' 'b') (initialState None "abc") |> should equal expected
