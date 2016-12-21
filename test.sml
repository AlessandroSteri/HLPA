use "/Users/alessandrosteri/Documents/Università/LinguaggiDiProgrammazione/HLPA/files.sml";


val form = Hoare.Triple( Hoare.Equal(Hoare.Var "x", Hoare.Num 0)
                       , Hoare.Comp( Hoare.Skip 
                       	           , Hoare.If( Hoare.Minor(Hoare.Var "x", Hoare.Num 0) 
                       	           	         , Hoare.Assign("x", Hoare.Plus(Hoare.Var "x", Hoare.Num 1))
                       	           	         , Hoare.Assign("x", Hoare.Plus(Hoare.Var "x", Hoare.Num 2))))
                       , Hoare.Equal(Hoare.Var "x", Hoare.Num 2));


open Controller;
goal form;
by (Rule.tacComp 1);
by (Rule.tacIf 2);
(* by (Rule.tacAssign 3); *)
by (Rule.tacStr 3);
by (Rule.tacAssign 4);

by ( Rule.tacNorm 4);

by (Rule.tacAxiom 4);