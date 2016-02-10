namespace FSql.Tests

// Things to test list
//// Primitive.unexpectedErr
//// Primitive.hEmpty -> rename to emptyHints
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
module Test = 
  open NUnit.Framework
  open FsUnit
  open FSql.Primitive
  open FSql.Character
  open FSql.Error
  
