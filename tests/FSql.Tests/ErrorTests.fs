namespace FSql.Tests

module Error = 
  open NUnit.Framework
  open FsUnit
  open FSql.Error
  open FSql.Position
  
  [<Test>]
  let ``fromEnum - Unexpected = 0``() = 
    let expected = 0
    fromEnum (Unexpected "some message") |> should equal expected
  
  [<Test>]
  let ``fromEnum - Expected = 1``() = 
    let expected = 1
    fromEnum (Expected "some message") |> should equal expected
  
  [<Test>]
  let ``fromEnum - Message = 2``() = 
    let expected = 2
    fromEnum (Message "some message") |> should equal expected
  
  [<Test>]
  let ``badMessage - returns true for bad messages``() = 
    let expected = true
    badMessage (Message "") |> should equal expected
  
  [<Test>]
  let ``badMessage - returns false for good messages``() = 
    let expected = false
    badMessage (Message "some random message") |> should equal expected
  
  [<Test>]
  let ``errorIsUnknown - returns true for empty errors``() = 
    let expected = true
    
    let errors = 
      { position = initialPos None
        messages = [] }
    errorIsUnknown errors |> should equal expected
  
  [<Test>]
  let ``errorIsUnknown - returns false for non empty errors``() = 
    let expected = false
    
    let errors = 
      { position = initialPos None
        messages = [ Message "Known error" ] }
    errorIsUnknown errors |> should equal expected
  
  [<Test>]
  let ``addErrorMessage - must order add message in correct order``() = 
    let errors = 
      { position = initialPos None
        messages = 
          [ Unexpected "A: Unexpected 1"
            Unexpected "Z: Unexpected 2"
            Expected "A: Expected 1"
            Expected "Z: Expected 2"
            Message "A: Message 1"
            Message "Z: Message 2" ] }
    
    let msg = Expected "Some error message"
    
    let expected = 
      { position = initialPos None
        messages = 
          [ Unexpected "A: Unexpected 1"
            Unexpected "Z: Unexpected 2"
            Expected "A: Expected 1"
            Expected "Some error message"
            Expected "Z: Expected 2"
            Message "A: Message 1"
            Message "Z: Message 2" ] }
    addErrorMessage msg errors |> should equal expected
  
  [<Test>]
  let ``addErrorMessages - must order add messages in correct order``() = 
    let errors = 
      { position = initialPos None
        messages = 
          [ Unexpected "A: Unexpected 1"
            Unexpected "Z: Unexpected 2"
            Expected "A: Expected 1"
            Expected "Z: Expected 2"
            Message "A: Message 1"
            Message "Z: Message 2" ] }
    
    let msgs = 
      [ Message "Some error message"
        Expected "Some error message"
        Unexpected "Some error message" ]
    
    let expected = 
      { position = initialPos None
        messages = 
          [ Unexpected "A: Unexpected 1"
            Unexpected "Some error message"
            Unexpected "Z: Unexpected 2"
            Expected "A: Expected 1"
            Expected "Some error message"
            Expected "Z: Expected 2"
            Message "A: Message 1"
            Message "Some error message"
            Message "Z: Message 2" ] }
    
    addErrorMessages msgs errors |> should equal expected
  
  [<Test>]
  let ``newErrorUnknown - is valid``() = 
    let pos = initialPos None
    
    let expected = 
      { position = pos
        messages = List.empty }
    newErrorUnknown pos |> should equal expected
  
  [<Test>]
  let ``newErrorUnknown - is verifyable by errorIsUnknown``() = 
    let pos = initialPos None
    let expected = true
    errorIsUnknown (newErrorUnknown pos) |> should equal expected
  
  [<Test>]
  let ``newErrorMessage - creates valid error``() = 
    let pos = initialPos None
    let msg = Unexpected "Some message"
    
    let expected = 
      { position = pos
        messages = [ msg ] }
    newErrorMessage msg pos |> should equal expected
  
  [<Test>]
  let ``newErrorMessages - creates valid error``() = 
    let pos = initialPos None
    
    let msgs = 
      [ Unexpected "Some message"
        Message "some message 2" ]
    
    let expected = 
      { position = pos
        messages = msgs }
    
    newErrorMessages msgs pos |> should equal expected

  [<Test>]
  let ``setErrorMessage - replaces the current error``() = 
    let pos = initialPos None
    
    let msg = Message "new message 3"
    
    let initial = 
      { position = pos
        messages = [ Unexpected "message 1"
                     Expected "message 2"
                     Message "old message 3" ] }
    let expected = 
      { position = pos
        messages = [ Unexpected "message 1"
                     Expected "message 2"
                     msg ] }
    
    setErrorMessage msg initial |> should equal expected

