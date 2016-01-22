namespace FSql.Tests

// Things to test list
//// Error.badMessage
//// Error.errorIsUnknown
//// Error.addErrorMessage
//// Error.addErrorMessages
//// Error.newErrorUnknown
//// Error.newErrorMessages
//// Error.newErrorMessage
//// Error.setErrorMessages
//// Primitive.unexpectedErr
//// Primitive.hEmpty
//// Primitive.toHints
//// Primitive.withHints
//// Primitive.accHints
//// Primitive.refreshLastHints
//// Primitive.uncons
//// Primitive.showToken
//// Primitive.initialState
//// Primitive.runParsec'
//// Primitive.runParser'
//// Primitive.runParser
//// Primitive.pMap -> rename to map
//// Primitive.pBind -> rename to bind
//// Primitive.pReturn -> rename to return
//// Primitive.pFail -> rename to fail
//// Primitive.pFailure -> rename to failure
//// Primitive.pZero -> rename to zero
//// Primitive.pLabel -> rename to label
//// Primitive.pEof -> rename to eof
//// Primitive.pToken -> rename to token
//// Primitive.pTokens -> rename to tokens
//// Primitive.satisfy -> move to Char.fs
//// Primitive.char' -> move to Char.fs
//// Primitive.string' -> move to Char.fs

module Test = 
  open NUnit.Framework
  open FsUnit
  open FSql.Primitive
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
