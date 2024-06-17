% juntar(?Lista1,?Lista2,?Lista3)

juntar([],Lista2,Lista2)
juntar([X|T1],Lista2,[X|T3]) :- juntar(T1,Lista2,T3)


