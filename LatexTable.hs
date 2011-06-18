-- Thomas ANBERREE 
-- June 2011

import SeqEnum
-- Helpers to write the LaTeX files 
-- and tests
-- for the paper Definable Quotients in Type Theories



         
-- for testing:
c j = map (flip phi j) [0..]

evens (x:_:xs) = x:evens xs
odds  (_:x:xs) = x:odds  xs

rank n = evens . (expunge n)

expunge 0 = id
expunge n = odds . (expunge (n-1)  )

r i = [phi i j  | j <- [0..5]]

t = filter (all (<5)) [r j | j <- [0..1000000]]


-- To generate a LaTeX table showing the matrix phi i j 
main = putStrLn latex
latex = "\\[\\begin{array}{|" ++ concat ["c|" | _ <-[0..nc]] ++ "}\n\\hline\n" 
    ++ concat (intersperse " & " ["\\varphi_{"++show c++"}" | c <- [0..nc]]) ++ "\\\\\n\\hline\n"
    ++ concat
    (intersperse "\\\\\n"
     [ concat (intersperse " & " 
                [show  (phi c r)
                | c <- [0..nc]])
             |  r <- [0..nr]])
    ++ "\\\\\n"
    ++ concat (intersperse " & " ["\\vdots" | c <- [0..nc]]) 
    ++ "\\\\\n\\hline\n\\end{array}\\]"
    where
    nc = 20
    nr = 5




