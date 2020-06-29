cantll([], 0).
cantll([H|T], N) :- list_length(H, HN),
                    cantll(T, TN),
                    N is HN + TN.

list_length([] , 0 ).
list_length([_|Xs] , L ) :- list_length(Xs,N) , L is N+1 .
list_length(A , 0 ):- not(is_list(A)).








