namespace FSql.Tests

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
  open FSql.Position

  [<Test>]
  let ``Error: from enum Unexpected = 0``()=
    let expected = 0
    fromEnum (Unexpected "some message") |> should equal expected

  [<Test>]
  let ``Error: from enum Expected = 1``()=
    let expected = 1
    fromEnum (Expected "some message") |> should equal expected

  [<Test>]
  let ``Error: from enum Message = 2``()=
    let expected = 2
    fromEnum (Message "some message") |> should equal expected

  [<Test>]
  let ``Error: badMessage returns true for bad messages``()=
    let expected = true
    badMessage (Message "") |> should equal expected

  [<Test>]
  let ``Error: badMessage returns false for good messages``()=
    let expected = false
    badMessage (Message "some random message") |> should equal expected

  [<Test>]
  let ``Error: errorIsUnknown returns true for empty errors``()=
    let expected = true
    let errors = 
      { position = initialPos None
        messages = [] }
    errorIsUnknown errors |> should equal expected

  [<Test>]
  let ``Error: errorIsUnknown returns false for non empty errors``()=
    let expected = false
    let errors = 
      { position = initialPos None
        messages = [Message "Known error"] }
    errorIsUnknown errors |> should equal expected

