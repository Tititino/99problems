%% ------------------ %%
-module(lists).
-export([last/1, last_two/1, at/2, lenght/1, rev/1, is_palindrome/1, flatten/1, compress/1, pack/1, encode/1]).
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
		{_, []}  		-> erlang:error(outOfBoundExcpetion);
		{1, [X | _]}  	-> X; 
		{_, [_ | XS]}  	-> at(N - 1, XS)
	end.

lenght(L) -> 
	K = fun Tail(X, ACC) ->
		case X of
			[] 		-> ACC;
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
        	[X | XS] 	-> 
			case X of
				Y when not erlang:is_list(Y) 	-> [X | flatten(XS)];
				_ 				-> flatten(X) ++ flatten(XS)
			end
    	end.

compress([]) -> [];
compress([X | XS]) -> 
	F = fun Tail(LS, ACC, LAST) -> 
		case LS of
			[] 				-> ACC;
			[Y | YS]  when Y =/= LAST 	-> Tail(YS, [Y | ACC], Y);
			[_ | YS]			-> Tail(YS, ACC, LAST)
		end
	end,
	lists:reverse(F(XS, [X | []], X)).
                                               
pack([]) -> [];
pack([X | XS]) -> 
	F = fun Tail(LS, ACC, LAST) -> 
		case LS of
			[] 				-> ACC;
			[Y | YS] when Y =/= LAST 	-> Tail(YS, [[Y | []] | ACC], Y);
			[_ | YS]	            	-> 
				case ACC of 
					[A | ASS] -> Tail(YS, [[LAST | A] | ASS], LAST)  
				end
		end
	end,
	lists:reverse(F(XS, [[X | []]], X)).
                                                         
encode() ->
    
