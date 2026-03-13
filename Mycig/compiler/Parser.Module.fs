namespace Mycig.Compiler

open FParsec

module ParserModule =
    let endLines: Parser<char list, unit> = many (newline <|> pchar ';')
    let funcEndLines: Parser<char list, unit> = many newline
    let ifEndLines: Parser<char list, unit> = many newline
    let frameEndLines: Parser<char list, unit> = many newline
    let implEndLines: Parser<char list, unit> = many newline
    let fieldEndLines: Parser<char list, unit> = many newline
    let ident: Parser<string, unit> = regex @"[\p{L}_][\p{L}\p{N}_]*"
