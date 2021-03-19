lowerCase(X) --> [X], { code_type(X, alpha), is_lower(X) }.

upperCase(X) --> [X], { code_type(X,alpha), is_upper(X) }.

lowerGroup([]) --> [].
lowerGroup([X]) --> lowerCase(X).
lowerGroup([X | Xs]) --> lowerCase(X), lowerGroup(Xs).

digit(X) --> [X], { number(X) }.
reference([X]) --> digit(X).

reference([X | Xs]) --> 
    digit(X), 
    reference(Xs).

atom([X | Xs],Atoms-A,Edges-E,IdxStack,RefIdx,NextAvailIdx,
        AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdx, NextAvailIdxNew) --> 
    upperCase(X), 
    lowerGroup(Xs),
    {Ws = [X | Xs],
    addAtom(Ws,Atoms-A,Edges-E,NextAvailIdx,IdxStack,
                            AtomsNew-B,EdgesNew-F,NextAvailIdxNew, IdxStackNew)
    }.

%atom([X],Atoms-A,Edges-E,IdxStack,RefIdx) --> 
 %   upperCase(X).

atom(Xs,Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
           Atoms-A, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew) --> 
    reference(Xs),
    {
    IdxStack \= [],
    combineListToNumber(Xs,RefNum),
    first(IdxStack,J),

   ( refExists(RefNum,RefIdx) -> 
         getIdx(RefNum,RefIdx,I), 
         connect(I,IdxStack,Edges-E,IdxStackNew,EdgesNew-F),
         RefIdxNew = RefIdx,
         NextAvailIdxNew is NextAvailIdx + 1;

        pushFront(RefNum-J,RefIdx,RefIdxNew),
        NextAvailIdxNew = NextAvailIdx,
        EdgesNew = Edges,
        IdxStackNew = IdxStack,
        F = E
    ) }.

smiles(Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
        AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew) --> 
    atom(_,Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
            AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew).

smiles(Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
        AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew) --> 
    ['('], 
    {
            first(IdxStack,I),
            pushFront(I,IdxStack,IdxStack2)
    },
    smiles(Atoms-A,Edges-E,IdxStack2,RefIdx, NextAvailIdx,
            AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew).

smiles(Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
        AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew) --> 
    atom(_,Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
            Atoms2-A2, Edges2-E2, IdxStack2,RefIdx2, NextAvailIdx2), 
    smiles(Atoms2-A2,Edges2-E2,IdxStack2,RefIdx2,NextAvailIdx2,
            AtomsNew-B, EdgesNew-F, IdxStack3,RefIdxNew, NextAvailIdxNew), 
    [')'],
    {
        pop(IdxStack3,IdxStackNew)
    }.

smiles(Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
        AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew) --> 
    [')'],
    {
    pop(IdxStack,IdxStack2)
    }, 
    smiles(Atoms-A,Edges-E,IdxStack2,RefIdx,NextAvailIdx,
            AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew).

smiles(Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
        AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew) --> 
    atom(_,Atoms-A,Edges-E,IdxStack,RefIdx, NextAvailIdx,
            Atoms2-A2, Edges2-E2, IdxStack2,RefIdx2, NextAvailIdx2), 
    smiles(Atoms2-A2,Edges2-E2,IdxStack2,RefIdx2, NextAvailIdx2,
            AtomsNew-B, EdgesNew-F, IdxStackNew,RefIdxNew, NextAvailIdxNew).

connect(I,IdxStack,Edges-E,
    IdxStackNew,Edges-E) :-
        IdxStack = [],
        pushFront(I,IdxStack,IdxStackNew).

connect(I,IdxStack,Edges-E,
              IdxStackNew,EdgesNew-F) :-
        IdxStack \= [],
        first(IdxStack,J),
        pop(IdxStack, IdxStack2),
        pushFront(I,IdxStack2,IdxStackNew),
        diffPushBack(edge(I,J),Edges-E,EdgesNew-F).



% kolekce atomu (jejich unitkatni id bude idx)
% kolekce hran -- edge(idx, idx)
% stack idx atomu, s kterym vytvarim relaci
% kolekce reference-idx

%udelat funkce pro rozdilove seznamy, atoms a edges musi
%byt rozdilove seznamy

% smiles_graph(+SmilesString, -Graf)
smiles_graph(InputString, graph(V,E)) :-
    string_chars(InputString, InputArr),
    Atoms = [],
    Edges = [],
    IdxStack = [],
    RefIdx = [],
    smiles(Atoms-_,Edges-_,IdxStack,RefIdx,0,
            AtomsNew-B, EdgesNew-F, _,_, _, InputArr,[]),
    diff2list(AtomsNew-B,V),
    diff2list(EdgesNew-F,E).

getIdx(S, [S-J | _], J). 
getIdx(S, [_-_ | RefIdx],Output) :- getIdx(S,RefIdx,Output). 

getRef(I, [R-I | _],R).
getRef(I, [_-_ | RefIdx], Output) :- getRef(I,RefIdx,Output).

refExists(_,[]) :- fail.
refExists(R,[R-_ | _]).
refExists(S,[_-_ | RefIdx]) :- refExists(S,RefIdx).

list2diff([],X-X).
list2diff([H|T],[H|S]-X):-list2diff(T,S-X).

diff2list(X-[],X).

diffAppend(A-B,B-C,A-C).
diffPushBack(X,[]-_,[X|B]-B).
diffPushBack(X,A-B,A-D) :- A\=[], B = [X | D].
pushFront(X,Xs,[X | Xs]).

first([X | _],X).
first([],_) :- fail. 

pop([],_) :- fail.
pop([_ | Xs], Xs).

addAtom(AtomArr,Atoms-A,Edges-E,NextAvailIdx,IdxStack,
                 AtomsNew-B,EdgesNew-F,NextAvailIdxNew, IdxStackNew) :-
    atom_chars(AtomName,AtomArr),
    diffPushBack(AtomName,Atoms-A,AtomsNew-B),
    connect(NextAvailIdx,IdxStack,Edges-E,IdxStackNew,EdgesNew-F),
    NextAvailIdxNew is NextAvailIdx + 1.

combineListToNumber(List,Number) :-
    atomic_list_concat(List,NumberAtom),
    atom_number(NumberAtom,Number).


% kazdej atom ma unikatni index (podle vyskytu)

        









% lowerCase --> [a] ; [b]; [c]; [č]; [d]; [ď]; [e]; [f]; [g]; 
%     [h];[i];[j];[k];[l];[m];[n];[ň]; [o];[p];
%     [q];[r];[ř];[s];[š];[t]; [ť];[u];[v];[w];
%     [x];[y];[z];[ž].

% upperCase --> ['A'] ; ['B']; ['C']; ['Č']; ['D']; ['Ď']; ['E']; ['F']; ['G']; 
% ['H'];['I'];['J'];['K'];['L'];['M'];['N'];['Ň']; ['O'];['P'];
% ['Q'];['R'];['Ř'];['S'];['Š'];['T']; ['Ť'];['U'];['V'];['W'];
% ['X'];['Y'];['Z'];['Ž'].

% lowerGroup --> lowerCase.
% lowerGroup --> lowerCase, lowerGroup.

% number --> [0] ; [1] ; [2] ; [3] ; [4] ; [5] ; [6] ; [7] ; [8] ; [9].

% reference --> number.
% reference --> number, reference.

% atom --> 
% upperCase, 
% lowerGroup.


% atom --> upperCase.
% atom --> reference.

% smiles --> 
% atom.

% smiles --> 
% ['('], 
% smiles.

% smiles --> 
% atom, 
% smiles, 
% [')'].

% smiles --> 
% [')'], 
% smiles.

% smiles --> 
% atom, 
% smiles.