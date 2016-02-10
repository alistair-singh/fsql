namespace FSql.Tests

module Character = 
  open NUnit.Framework
  open FsUnit
  open FSql.Primitive
  open FSql.Character
  open FSql.Error
  open FSql.Position

  [<Test>]
  let ``char' - Single char parser pass``() = 
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
  let ``char' - Single char parser fail``() = 
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
                    [ Unexpected "'a'"
                      Expected "'b'" ] }
        consumption = Virgin }
    runParsec' (char' 'b') (initialState None "abc") |> should equal expected

  [<Test>]
  let ``string' - Single string parser pass``() = 
    let expected = 
      { state = 
          { input = [ 'c' ]
            position = 
              { name = None
                line = 1
                column = 3
                absolute = 2 } }
        result = Ok ['a';'b']
        consumption = Consumed }
    
    let actual = runParsec' (string' "ab") (initialState None "abc")
    actual |> should equal expected

  [<Test>]
  let ``string' - Single string parser fail consumed``() = 
    let expected : Reply<char, char list> = 
      { state = 
          { input = "aac"
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
                    [ Unexpected "['a'; 'a']"
                      Expected "\"ab\"" ] }
        consumption = Consumed }
    let actual = (runParsec' (string' "ab") (initialState None "aac")) 
    actual |> should equal expected

  [<Test>]
  let ``string' - Single string parser fail virgin``() = 
    let expected : Reply<char, char list> = 
      { state = 
          { input = "zac"
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
                    [ Unexpected "['z']"
                      Expected "\"ab\"" ] }
        consumption = Virgin }
    let actual = (runParsec' (string' "ab") (initialState None "zac")) 
    actual |> should equal expected

  [<Test>]
  let ``string' - Single long string parser fail consumed``() = 
    let expected : Reply<char, char list> = 
      { state = 
          { input = "abcdefghijklmnop"
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
                    [ Unexpected "['a'; 'b'; 'c'; 'd'; 'e']"
                      Expected "\"abcdgssdgcks\"" ] }
        consumption = Consumed }
    let actual = (runParsec' (string' "abcdgssdgcks") (initialState None "abcdefghijklmnop")) 
    actual |> should equal expected


