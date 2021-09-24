-- NAME: Bhavit Wadhwa
-- ID: 1516846

-- (4 programs at EOF)

import Parsing

type Ide = String

data Exp = Zero | One | TT | FF | Read | I Ide | Not Exp | Equal Exp Exp | Plus Exp Exp deriving Show

data Cmd = Assign Ide Exp | Output Exp | IfThenElse Exp Cmd Cmd | WhileDo Exp Cmd | Seq Cmd Cmd deriving Show

--  Answers an 

-- Here we have not binding tighter than = or +.

-- GRAMMAR: <expr> ::= <term> + <expr> | <term> = <expr> | <term>
expr :: Parser Exp
expr = do e1 <- term
          do  symbol "+"
              e2 <- expr
              return (Plus e1 e2)
          +++
          do  e1 <- term
              do  symbol "="
                  e2 <- expr
                  return (Equal e1 e2)
          +++
          term

-- GRAMMAR: <term> ::= not <expr> | <factor>
term :: Parser Exp
term =  do  symbol "not" 
            do  e1 <- expr
                return (Not e1)
        +++
        do  e2 <- factor
            return (e2)

-- Have a look at expr was done, 

-- GRAMMAR: <factor> ::= read | false | true | 0 | 1 | <ide> | (<expr>)
factor :: Parser Exp
factor =  do  symbol "read"
              return Read
          +++
          do  symbol "false"
              return FF
          +++
          do  symbol "true"
              return TT
          +++
          do  symbol "0"
              return Zero
          +++
          do  symbol "1"
              return One
          +++
          do  symbol "true"
              return TT
          +++
          do  i1 <- identifier
              many (char ' ')
              return (I i1)
          +++
          do  symbol "("
              e1 <- expr
              symbol ")"
              return (e1)

-- GRAMMER: <command> ::= <ide>:=<expr> | output | if <expr> then <cmd> else <cmd> | while <expr> do <command>
command :: Parser Cmd
command =  do i1 <- identifier
              symbol ":="
              e1 <- expr
              return (Assign i1 e1)
           +++
           do symbol "output"
              e1 <- expr
              return (Output e1)
           +++
           do symbol "if"
              e1 <- expr
              symbol "then"
              c1 <- command   
              symbol "else"
              c2 <- command
              return (IfThenElse e1 c1 c2)
           +++
           do symbol "while"
              e1 <- expr
              symbol "do"
              c1 <- command
              return (WhileDo e1 c1)


-- Example: cmd; (cmd)*; cmd => is treated as cmd; (sequence)*; cmd
-- Where (cmd)* => is 0 to many commands.

-- GRAMMER: <sequence> ::= command ; command
commandsequence :: Parser Cmd
commandsequence =  do c1 <- command
                      symbol ";"
                      cs2 <- commandsequence
                      return (Seq c1 cs2)
                      +++
                      command


-- Testing functions for the expression parser and command parser
eeval :: String -> Exp
eeval xs = case (parse expr xs) of
  [(n,[])] -> n
  [(_,out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"

cparse :: String -> Cmd
cparse xs = case (parse commandsequence xs) of
  [(n,[])] -> n
  [(_,out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"


-- Using section 2.3 of Gordon
-----------------------------------------------------------
-- INPUT:   cparse "sum:=0; x:=read; while not(x=true) do sum:=sum+x; x:=read; output sum"
-- OUTPUT:  Seq (Assign "sum" Zero) (Seq (Assign "x" Read) (Seq (WhileDo (Not (Equal (I "x") TT)) (Assign "sum" (Plus (I "sum") (I "x")))) (Seq (Assign "x" Read) (Output (I "sum")))))

-- Parser Failed (failed correctly - invalid syntax) example:
-----------------------------------------------------------
-- PROGRAM 1: cparse "if 1 then e1:=0 else 1e:=1; output 1+1; e3:=1" - incorrect syntax => assign expects indentfier and recieves "1e"
-- OUTPUT:
-- *** Exception: invalid input
-- CallStack (from HasCallStack):
--   error, called at coursework3.hs:115:9 in main:Main

-- PROGRAM 2: cparse "while 1 do e10:=1 output not(1); e3:=1" - missing semicolon between c1 and c2";"
-- OUTPUT:
-- *** Exception: unused input output not(1); e3:=1
-- CallStack (from HasCallStack):
--   error, called at coursework3.hs:114:16 in main:Main


-- Parser Successful
-----------------------------------------------------------
-- PROGRAM 1: cparse "if 1 then e1:=0 else e1:=1; output 1+1; e3:=1"
-- OUTPUT:
-- Seq (IfThenElse One (Assign "e1" Zero) (Assign "e1" One)) (Seq (Output (Plus One One)) (Assign "e3" One))

-- PROGRAM 2: cparse "while 1 do e10:=1; output not(1); e3:=1"
-- OUTPUT: 
-- Seq (WhileDo One (Assign "e10" One)) (Seq (Output (Not One)) (Assign "e3" One))