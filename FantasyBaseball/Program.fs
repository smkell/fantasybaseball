open NUnit.Framework

type Result<'TSuccess,'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure 

type Request = {name:string; email:string}

let bind switchFunction twoTrackInput = 
    match twoTrackInput with
    | Success s -> switchFunction s
    | Failure f -> Failure f 

let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput 

let (>=>) switch1 switch2 =
    switch1 >> (bind switch2)

let switch f x =
    f x |> Success

let tee f x =
    f x |> ignore
    x 

let tryCatch f x =
    try 
        f x |> Success 
    with 
    | ex -> Failure ex.Message

let doubleMap successFunc failureFunc twoTrackInput =
    match twoTrackInput with
    | Success s -> Success (successFunc s)
    | Failure f -> Failure (failureFunc f)

let succeed x =
    Success x 

let fail x =
    Failure x

let both addSuccess addFailure switch1 switch2 x =
    match (switch1 x),(switch2 x) with
    | Success s1, Success s2 -> Success(addSuccess s1 s2)
    | Failure f1, Success _  -> Failure f1
    | Success _,  Failure f2 -> Failure f2
    | Failure f1, Failure f2 -> Failure(addFailure f1 f2)

let either addSuccess addFailure switch1 switch2 x =
    match (switch1 x),(switch2 x) with
    | Success s1, Success s2 -> Success(addSuccess s1 s2)
    | Failure f1, Success s2 -> Success s2
    | Success s1, Failure f2 -> Success s1
    | Failure f1, Failure f2 -> Failure(addFailure f1 f2)

let (&&&) v1 v2 =
    let addSuccess r1 r2 = r1 // Return first 
    let addFailure s1 s2 = s1 + ";" + s2 // concat
    both addSuccess addFailure v1 v2 

let (|||) v1 v2 =
    let addSuccess r1 r2 = r1 // Return first
    let addFailure s1 s2 = s1 + ";" + s2 // concat
    either addSuccess addFailure v1 v2

type Config = { debug:bool }

let debugLogger twoTrackInput =
    let success x = printfn "DEBUG. Success so far %A" x; x
    let failure = id // don't log here
    doubleMap success failure twoTrackInput

let injectableLogger config =
    if config.debug then debugLogger else id

let validate1 input =
    if input.name = "" then Failure "Name must not be blank"
    else Success input 

let validate2 input = 
    if input.name.Length > 50 then Failure "Name must not be longer than 50 chars"
    else Success input 

let validate3 input =
    if input.email = "" then Failure "Email must not be blank"
    else Success input 

let canonicalizeEmail input =
    { input with email = input.email.Trim().ToLower() }

let updateDatabase input =
    ()  // dummy dead-end for now 

let log twoTrackInput =
    let success x = printfn "DEBUG. Success so far: %A" x; x
    let failure x = printfn "ERROR. %A" x; x
    doubleMap success failure twoTrackInput 

let combinedValidation =
    validate1
    &&& validate2 
    &&& validate3
    >=> switch canonicalizeEmail

let usecase config =
   combinedValidation
   >=> tryCatch (tee updateDatabase)
   >> injectableLogger config 

[<Test>]
let ``When I provide request objects then they should be correctly validated``() =
    for (request, expected) in 
        [ ({name=""; email="good"}, Failure "Name must not be blank");
          ({name="asdlk;fja;sldkfja;dlskfjads;lkfjasdl;kfja;dslkj;aflkjsdf;lkajdsf;lakdsjf;aldskjf;alksdjfa;ldskfjasdlkfj;alsdkfj"; email="good"}, Failure "Name must not be longer than 50 chars");
          ({name="Alice"; email=""}, Failure "Email must not be blank");
          ({name ="Alice"; email="Good@test.com"}, Success {name="Alice"; email="good@test.com"})] do
        let actual = combinedValidation request
        match (expected, actual) with
        | (Success e, Success a) -> Assert.AreEqual(e, a)
        | (Failure e, Failure a) -> Assert.AreEqual(e, a)
        | (Success _, Failure a) -> Assert.Fail("Unexpected failure with message: {0}", a)
        | (Failure e, Success _) -> Assert.Fail("Expected failure with message: {0} did not occur", e)
 
[<EntryPoint>]
let main argv = 
    let input1 = {name=""; email=""}
    combinedValidation input1
    |> printfn "Result1=%A"

    let input2 = {name="Alice";email=""}
    combinedValidation input2
    |> printfn "Result2=%A"

    let input3 = {name="Alice"; email="good"}
    combinedValidation input3
    |> printfn "Result3=%A"

    let input = {name="Alice"; email="Good"}

    let releaseConfig = {debug=false}
    input
    |> usecase releaseConfig
    |> ignore 

    let debugConfig = {debug=true}
    input
    |> usecase debugConfig
    |> ignore
    0
