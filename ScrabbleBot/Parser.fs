// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let clamping c = pchar '{' >*>. c .>*> pchar '}'

    let pid = (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_')
              |>> fun (first, rest) -> first :: rest |> List.toArray |> System.String.Concat

    
    let unop op a = op >*>. a
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') TermParse |>> (fun x -> Mul(N -1, x)) <?> "Neg"
    let PValueParse = unop pPointValue ParParse |>> PV <?> "PV"
    let VarParse = pid |>> V <?> "V"
    let IntLitParse = pint32 |>> N <?> "IntLit"
    let CharToIntParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [NParse; ParParse; CharToIntParse; IntLitParse]

    let AexpParse = TermParse 

    let CParse = between (pchar '\'') (pchar '\'') anyChar |>> C <?> "C"
    let CValueParse = unop pCharValue (parenthesise TermParse) |>> CV <?> "CV"
    let IntToCharParse = unop pIntToChar (parenthesise TermParse) |>> IntToChar <?> "IntToChar"
    let UpperParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let LowerParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"
    do cref := choice [CValueParse; IntToCharParse; UpperParse; LowerParse; CParse]
    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
