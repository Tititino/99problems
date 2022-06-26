%% ------------------ %%
-module(liste).
-export([ last/1
        , last_two/1
        , at/2
        , lenght/1
        , rev/1
        , is_palindrome/1
        , flatten/1
        , compress/1
        , pack/1
        , encode/1
        , encode2/1
        , decode/1
        , duplicate/1
        , replicate/2
        , drop/2
        , split/2
        , slice/3
        , rotate/2
        , remove_at/2
        ]).
-record(one, {char}).
-record(many, {char, num}).

%% ------------------ %%

last([]) ->
    nothing;
last([X]) ->
    X;
last([_ | XS]) ->
    last(XS).


last_two(L) ->
    case L of
        []	 	-> nothing;
        [_]		-> nothing;
        [A, B]          -> {A, B};
        [_ | XS]	-> last_two(XS)
    end.

at(N, L) -> 
    case {N, L} of
        {_, []}  	-> erlang:error(outOfBoundExcpetion);
        {1, [X | _]}  	-> X; 
        {_, [_ | XS]}  -> at(N - 1, XS)
    end.

lenght(L) -> 
    K = fun Tail(X, ACC) ->
	    case X of
	        [] 	-> ACC;
	        [_ | XS]  	-> Tail(XS, ACC + 1)
	    end
        end,
    K(L, 0).

rev(L) -> 
    K = fun Tail(LS, ACC) ->
	    case LS of
	        [] 		-> ACC;
	        [X | XS]  	-> Tail(XS, [X | ACC])
	    end
        end,
    K(L, []).

is_palindrome(L) ->
    L =:= lists:reverse(L).

flatten(L) ->
    case L of
        [] 		-> [];
        [X | XS] 	-> case X of
		       Y when not erlang:is_list(Y) -> [X | flatten(XS)];
		       _ 			-> flatten(X) ++ flatten(XS)
                           end
    end.

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
