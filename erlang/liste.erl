%% ------------------ %%
-module(liste).
%-export([ last/1
%        , last_two/1
%        , at/2
%        , lenght/1
%        , rev/1
%        , is_palindrome/1
%        , flatten/1
%        , compress/1
%        , pack/1
%        , encode/1
%        , encode2/1
%        , decode/1
%        , duplicate/1
%        , replicate/2
%        , drop/2
%        , split/2
%        , slice/3
%        , rotate/2
%        , remove_at/2
%        ]).

-compile(export_all).

-record(one, {char}).
-record(many, {char, num}).

%% ------------------ %%

%% Problem 1 %%
% Write a function `last : 'a list -> 'a option` that returns the last element of a list
last([]) ->
    nothing;
last([X]) ->
    X;
last([_ | XS]) ->
    last(XS).

%% Problem 2 %% 
% Find the last but one (last and penultimate) elements of a list.
last_two(L) ->
    case L of
        []	 	-> nothing;
        [_] 	-> nothing;
        [A, B]          -> {A, B};
        [_ | XS]	-> last_two(XS)
    end.

%% Problem 3 %%
% Find the N'th element of a list.
at(N, L) -> 
    case {N, L} of
        {_, []}  	-> erlang:error(outOfBoundExcpetion);
        {0, [X | _]}  	-> X; 
        {_, [_ | XS]}  -> at(N - 1, XS)
    end.

%% Problem 4 %% 
% Find the number of elements of a list.
lenght(L) -> 
    K = fun Tail(X, Acc) ->
	    case X of
	        [] 	     -> Acc;
	        [_ | XS] -> Tail(XS, Acc + 1)
	    end
        end,
    K(L, 0).

%% Problem 5 %% 
% Reverse a list.
rev(L) -> 
    K = fun Tail(LS, Acc) ->
	    case LS of
	        [] 		-> Acc;
	        [X | XS]  	-> Tail(XS, [X | Acc])
	    end
        end,
    K(L, []).

%% Problem 6 %% 
% Find out whether a list is a palindrome.
is_palindrome(L) ->
    L =:= lists:reverse(L).

%% Problem 7 %%
% Flatten a nested list structure.
flatten(L) ->
    case L of
        []       -> [];
        [X | XS] -> case X of
                        Y when not erlang:is_list(Y) -> [X | flatten(XS)];
		_ 		     -> flatten(X) ++ flatten(XS)
                    end
    end.

%% Problem 8 %%
% Eliminate consecutive duplicates of list elements.
compress([]) -> 
    [];
compress([X | XS]) -> 
    F = fun Tail(LS, ACC, LAST) -> 
	    case LS of
	        [] 		          -> ACC;
	        [Y | YS]  when Y =/= LAST -> Tail(YS, [Y | ACC], Y);
	        [_ | YS]	          -> Tail(YS, ACC, LAST)
	    end
        end,
    lists:reverse(F(XS, [X | []], X)).

%% Problem 9 %%
% Pack consecutive duplicates of list elements into sublists.                                                      
pack([]) -> 
    [];
pack([X | XS]) -> 
    F = fun Tail(LS, ACC, LAST) -> 
	    case LS of
	        [] 		         -> ACC;
	        [Y | YS] when Y =/= LAST -> Tail(YS, [[Y | []] | ACC], Y);
	        [_ | YS]	         -> case ACC of [A | ASS] -> Tail(YS, [[LAST | A] | ASS], LAST)  end
	    end
        end,
    lists:reverse(F(XS, [[X | []]], X)).
                           


                                       
encode(L) ->
    Help = fun F(FROM, TO) ->
	    case {FROM, TO} of
	        {[X | XS], [{X, N} | RS]} -> F(XS, [{X, N + 1} | RS]);
	        {[X | XS], RS}            -> F(XS, [{X, 1} | RS]);
	        {[], RS}                  -> RS
	    end
        end,
    lists:reverse(Help(L, [])).

% liste:encode(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]).

encode2(L) ->
    Help = fun F(FROM, TO) ->
	       case {FROM, TO} of
	           {[X | XS], [#many{char = X, num = N} | RS]} -> F(XS, [#many{char = X,num = N + 1} | RS]);		    
	           {[X | XS], [#one{char = X} | RS]}     -> F(XS, [#many{char = X, num = 2} | RS]);		    
	           {[X | XS], RS}                 -> F(XS, [#one{char = X} | RS]);
	           {[], RS}                       -> RS
	       end
           end,
    lists:reverse(Help(L, [])).


	           
decode(L) ->
    Repeat = fun R(From, To) ->
	         case From of
		 [#one{char = X} | XS]     -> R(XS, [X | To]);
		 [#many{char = X, num = 2} | XS] -> R([#one{char = X} | XS], [X | To]);
		 [#many{char = X, num = N} | XS] -> R([#many{char = X, num = N - 1} | XS], [X | To]);
		 [] -> To
	         end
	 end,
    lists:reverse(Repeat(L, [])).

duplicate([]) ->
    [];
duplicate([X | XS]) ->
    [ X | [ X | duplicate(XS) ]].

replicate(L, N) ->
    Repeat = fun R(From, N1, To) ->
	         case {From, N1} of
		 {[X | XS], 1} -> R(XS, N, [X | To]);
		 {[X | XS], _} -> R([X | XS], N1 - 1, [X | To]);
		 {[], _}       -> To
	         end
	 end,
    lists:reverse(Repeat(L, N, [])).

drop(L, N) ->
    Func = fun F(From, N1) ->
	       case {From, N1} of
	           {[], _} -> [];
	           {[_ | XS], 1} -> F(XS, N);
	           {[X | XS], K} -> [ X | F(XS, K - 1)]
	       end
           end,
    Func(L, N).

split(L, N) ->
    Func = fun F(From, N1, To) ->
	       case {From, N1} of
	           {[], _} -> {To, []};
	           {[X | XS], 1} -> {lists:reverse([X | To]), XS};
	           {[X | XS], K} -> F(XS, K - 1, [X | To])
	       end
           end,
    Func(L, N, []).

slice([], _, _) ->
    [];
slice(_, _, 1) ->
    [];
slice([X | XS], 0, F) ->
    [X | slice(XS, 0, F - 1)];
slice([_ | XS], I, F) ->
    slice(XS, I - 1, F).

rotate(L, N) ->
    {A, B} = split(L, N),
    lists:append(B, A).


remove_at(_, []) ->
    [];
remove_at(0, [_ | XS]) ->
    XS;
remove_at(N, [X | XS]) ->
    [ X | remove_at(N - 1, XS) ].

%% Problem 22 %%
% Create a list containing all integers within a given range.
% If first argument is greater than second, produce a list in decreasing order.
range(N, M) when N < M ->
            K = fun Tail(K, Acc) -> 
                        if K == N -> [ K | Acc ];
                           true   -> Tail(K - 1, [ K | Acc ])
                        end
                end,
            K(M, []);
range(N, M) ->
            K = fun Tail(K, Acc) ->
                        if K == N -> [ K | Acc ];
                           true   -> Tail(K + 1, [ K | Acc ])
                        end
                end,
            K(M, []).
                     
%% Problem 23 %%
% Extract a given number of randomly selected elements from a list.
% The selected items shall be returned in a list. We use the `Random` module but 
% do not initialize it with `Random.self_init` for reproducibility.
rand_select([], _) -> [];                  
rand_select(L, N) ->
            Len = length(L),
            K = fun Tail(0, Acc) -> Acc;
                    Tail(N, Acc) ->
                        Tail(N - 1, [ lists:nth(rand:uniform(Len), L) | Acc ])
                end,
            K(N, []).
                        
%% Problem 24 %%
% Lotto: Draw N different random numbers from the set 1..M.
% Draw N different random numbers from the set 1..M.
% The selected numbers shall be returned in a list.

% not really efficient
lotto_select(N, Max) ->
            L = range(1, Max),
            K = fun Tail(0, _, _, Acc)                -> Acc;
                    Tail(N, L, Max, Acc) when N < Max ->
                        Ext = lists:nth(rand:uniform(Max), L),
                        Tail(N - 1, lists:delete(Ext, L), Max - 1, [ Ext | Acc ])
                end,
            K(N, L, Max, []).

%% Problem 25 %%
% Generate a random permutation of the elements of a list.
permutation(L) -> 
            K = fun Tail(_, 0, Acc) -> Acc;
                    Tail(L, N, Acc) ->
                        Ext = lists:nth(rand:uniform(N), L),
                        Tail(lists:delete(Ext, L), N - 1, [ Ext | Acc ])
                end,
            K(L, length(L), []).

%% Problem 26 %%
% Generate the combinations of K distinct objects chosen from the N elements of a list.
combinazioni(_, 0) -> [[]];                                   
combinazioni(L, N) ->
            lists:foldl( fun(El, Acc) -> 
                                    Acc ++ lists:map( fun(Comb) -> [ El | Comb ] end
                                                    , combinazioni(lists:delete(El, L), N - 1))
                         end
                        , []
                        , L).
%% Problem 27 %%
% Group the elements of a set into disjoint subsets
% 1. In how many ways can a group of 9 people work in 3 disjoint subgroups of 2,
%    3 and 4 persons? Write a function that generates all the possibilities and 
%    returns them in a list.
% 2. Generalize the above function in a way that we can specify a list of group
%    sizes and the function will return a list of groups.


                        
%% Problem 28 %%
% Sorting a list of lists according to length of sublists.
% 1. We suppose that a list contains elements that are lists themselves. The 
%    objective is to sort the elements of this list according to their length. 
%    E.g. short lists first, longer lists later, or vice versa.
length_sort(L) -> lists:sort(fun(A, B) when length(A) < length(B) -> true; (_, _) -> false end, L).
 
% 2. Again, we suppose that a list contains elements that are lists themselves. 
%    But this time the objective is to sort the elements of this list 
%    according to their length frequency; i.e., in the default, where sorting 
%    is done ascendingly, lists with rare lengths are placed first, others with 
%    a more frequent length come later.

