namespace FSql.Tests

//// Error.badMessage
//// Error.errorIsUnknown
//// Error.addErrorMessage
//// Error.addErrorMessages
//// Error.newErrorUnknown
//// Error.newErrorMessages
//// Error.newErrorMessage
//// Error.setErrorMessages

module Error = 
  open NUnit.Framework
  open FsUnit
  open FSql.Error

  [<Test>]
  let ``Error: from enum Unexpected = 0``()=
    let expected = 0
    fromEnum (Unexpected "some message")|> should equal expected

  [<Test>]
  let ``Error: from enum Expected = 1``()=
    let expected = 1
    fromEnum (Expected "some message")|> should equal expected

  [<Test>]
  let ``Error: from enum Message = 2``()=
    let expected = 2
    fromEnum (Message "some message")|> should equal expected

