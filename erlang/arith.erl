-module(arith).
-compile(export_all).


%% Problem 31 %%
% Determine whether a given integer number is prime.
is_prime(N) when N >= 2 ->
            Sqrt = math:floor(math:sqrt(N)),
            F = fun Check(Prec) ->
                        if Prec > Sqrt     -> true          ;
                           N rem Prec == 0 -> false;
                           true            -> Check(Prec + 1)
                        end
                end,
            F(2);
is_prime(_) -> false.
            
%% Problem 32 %%
% Determine the greatest common divisor of two positive integer numbers.
% Use Euclid's algorithm.
gcd(N, 0) -> N;
gcd(N, M) when N > M ->
            R = N rem M,
            if R == 0 -> M;
               true   -> gcd(M, R)
            end;
gcd(M, N) -> gcd(N, M).
            
            
%% Problem 33 %%
% Determine whether two positive integer numbers are coprime.
% Two numbers are coprime if their greatest common divisor equals 1.
coprime(N, M) -> gcd(N, M) == 1.
