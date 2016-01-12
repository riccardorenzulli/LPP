{
module Parserccs (parser,lexer,Process(..)) where

import Data.Char
}

%name parser
%tokentype { Token }
%left '+' '.'
%attributetype { SymT a }
%attribute value { a }
%attribute list { [[Char]]}
%error { parseError }

%token 
       let     { TokenLet }
       in      { TokenIn }
       id      { TokenId $$ }
       var     { TokenVar }
       iaction { TokenIAction $$ }
       oaction { TokenOAction $$ }
       '0'     { TokenNil }
       '='     { TokenDef }
       '.'     { TokenPrefix }
       ';'     { TokenEndDef }
       ','     { TokenComma }
       '+'     { TokenExtChoice }
       '|'     { TokenPar }
       '('     { TokenOB }
       ')'     { TokenCB }

%%

Prog     : var Expr ';' Stmt           { $$ = $4; $4.list = $2.list }
         | Stmt                        { $$ = $1; $1.list = [] }

Expr     : Expr ',' id                 { $$ = $1 ++ [($3,Nil)]; $$.list = $1.list ++ [$3] }
         | id                          { $$ = [($1,Nil)]; $$.list = [$1] }

Stmt     : let Decl ';' in ListProc    { $$ = ($2,Par $5); $2.list = $$.list; $5.list = $$.list }
         | ListProc                    { $$ = ([],Par $1); $1.list = $$.list }

Decl     : Decl ',' id '=' Process     { $$ = $1 ++ [($3,$5)]; where failUnless (elem $3 $$.list) ("Process " ++ $3 ++ " not declared"); $1.list = $$.list; $5.list = $1.list }
         | id '=' Process              { $$ = [($1,$3)]; where failUnless (elem $1 $$.list) ("Process " ++ $1 ++ " not declared"); $3.list = $$.list }

ListProc : ListProc '|' Process        { $$ = $1 ++ [$3]; $1.list = $$.list; $3.list = $$.list}
         | Process                     { $$ = $1 : []; $1.list = $$.list }

Process  : iaction '.' Process         { $$ = InputAction $1 $3; $3.list = $$.list }
         | oaction '.' Process         { $$ = OutputAction $1 $3; $3.list = $$.list }
         | Process '+' Process         { $$ = ExtChoice $1 $3; $1.list = $$.list; $3.list = $$.list }
         | '(' Process ')'             { $$ = $2; $2.list = $$.list }
         | id                          { $$ = Id $1; where failUnless (elem $1 $$.list) ("Process " ++ $1 ++ " not declared") }
         | '0'                         { $$ = Nil }


{
parseError :: [Token] -> a
parseError [] = error "Empty list"
parseError xs = error ("Parse error" ++ show (xs))

failUnless b msg = if b then () else error msg


type ListProc = [Process]

type Expr = [([Char],Process)]

type Decl = [([Char],Process)]

type Prog = ([([Char],Process)],Process)

type Action = [Char]

data Process
      = InputAction Action Process
      | OutputAction Action Process
      | ExtChoice Process Process
      | Par [Process]
      | Id [Char]
      | Nil
      deriving (Show,Eq)

data Token
      = TokenLet
      | TokenIn
      | TokenId [Char]
      | TokenIAction [Char]
      | TokenOAction [Char]
      | TokenNil
      | TokenDef
      | TokenPrefix
      | TokenExtChoice
      | TokenPar
      | TokenOB
      | TokenCB
      | TokenComma
      | TokenEndDef
      | TokenVar
      deriving (Show,Eq)

lexer :: [Char] -> [Token]
lexer [] = []
lexer ('|':cs) = TokenPar : lexer cs
lexer ('+':cs) = TokenExtChoice : lexer cs
lexer ('.':cs) = TokenPrefix : lexer cs
lexer ('!':cs) = lexAction ('!':cs)
lexer ('0':cs) = TokenNil : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('=':cs) = TokenDef : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (';':cs) = TokenEndDef : lexer cs
lexer (c:cs) 
 | isUpper c   = lexId (c:cs)
 | isLower c   = lexAction (c:cs)
 | isSpace c   = lexer cs
 | otherwise   = error $ "Cannot tokenize " ++ [c]

lexId :: [Char] -> [Token]
lexId (c:cs) = case span isUpper (c:cs) of (id,rest) -> TokenId id : lexer rest

lexAction ('!':cs) = case span isLower cs of (action,rest)   -> TokenOAction action : lexer rest
lexAction (c:cs) = case span isLower (c:cs) of ("let",rest)  -> TokenLet : lexer rest
                                               ("in",rest)   -> TokenIn : lexer rest
                                               ("var",rest)  -> TokenVar : lexer rest
                                               (action,rest) -> TokenIAction action : lexer rest
}
