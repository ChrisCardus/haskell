module PrettyPrinting where
    
    
    import Unsolved
    import AbstractSyntax
    
    spaces n = take n (repeat ' ')

    ppOp :: OpName -> String
    ppOp Or = "||"
    ppOp And = "&&"
    ppOp Eq = "=="
    ppOp Leq = "<="
    ppOp Less = "<"
    ppOp Geq = ">="
    ppOp Greater = ">"
    ppOp Add = "+"
    ppOp Sub = "-"
    ppOp Mul = "*"
    ppOp Div = "/"
    ppOp Mod = "%"
    ppOp Not = "!"

    
    ppExpr :: Expr -> String
    ppExpr (Constant x) = show x
    ppExpr (Var x) = x
    ppExpr (Op p (y:ys:yss)) =  if p == Not
                                then
                                    if (ys:yss) == []
                                    then x ++ " " ++ z
                                    else x ++ "(" ++ z ++ " " ++ (ppExpr (Op p (ys:yss))) ++ ")"
                                else
                                    if zss == []
                                    then z ++ " " ++ x ++ " " ++ zs
                                    else z ++ " " ++ x ++ " " ++ (ppExpr (Op p (ys:yss)))
                        where
                            x = ppOp p
                            (z:zs:zss) = (map ppExpr (y:ys:yss))

    
    indent :: Int -> String -> String
    indent 0 y = y
    indent x y = indent (x - 1) ("  " ++ y)

    level = 0

    ppProgram :: Program -> (Int -> String)
    ppProgram = question "pretty print programs"
    {-ppProgram (x := e) = x ++ " " ++ "=" ++ " " ++ (ppExpr e) ++ ";\n"
    ppProgram (Block (p:ps)) = "{\n" ++ if ps == [] then ppProgram p else ppProgram p ++ " "-}