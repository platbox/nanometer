-spec join_digits(digit(X), some(X), digit(X)) -> some(node(X)) when X :: desc().
join_digits({_, A}, {}, {_, I}) ->
    {node(A, I)};
join_digits({_, A}, {}, {_, I, J}) ->
    {node(A, I, J)};
join_digits({_, A}, {}, {_, I, J, K}) ->
    {node(A, I), node(J, K)};
join_digits({_, A}, {}, {_, I, J, K, L}) ->
    {node(A, I, J), node(K, L)};
join_digits({_, A, B}, {}, {_, I}) ->
    {node(A, B, I)};
join_digits({_, A, B}, {}, {_, I, J}) ->
    {node(A, B), node(I, J)};
join_digits({_, A, B}, {}, {_, I, J, K}) ->
    {node(A, B, I), node(J, K)};
join_digits({_, A, B}, {}, {_, I, J, K, L}) ->
    {node(A, B, I), node(J, K, L)};
join_digits({_, A, B, C}, {}, {_, I}) ->
    {node(A, B), node(C, I)};
join_digits({_, A, B, C}, {}, {_, I, J}) ->
    {node(A, B, C), node(I, J)};
join_digits({_, A, B, C}, {}, {_, I, J, K}) ->
    {node(A, B, C), node(I, J, K)};
join_digits({_, A, B, C}, {}, {_, I, J, K, L}) ->
    {node(A, B, C), node(I, J), node(K, L)};
join_digits({_, A, B, C, D}, {}, {_, I}) ->
    {node(A, B, C), node(D, I)};
join_digits({_, A, B, C, D}, {}, {_, I, J}) ->
    {node(A, B, C), node(D, I, J)};
join_digits({_, A, B, C, D}, {}, {_, I, J, K}) ->
    {node(A, B, C), node(D, I), node(J, K)};
join_digits({_, A, B, C, D}, {}, {_, I, J, K, L}) ->
    {node(A, B, C), node(D, I, J), node(K, L)};
join_digits({_, A}, {E}, {_, I}) ->
    {node(A, E, I)};
join_digits({_, A}, {E}, {_, I, J}) ->
    {node(A, E), node(I, J)};
join_digits({_, A}, {E}, {_, I, J, K}) ->
    {node(A, E, I), node(J, K)};
join_digits({_, A}, {E}, {_, I, J, K, L}) ->
    {node(A, E, I), node(J, K, L)};
join_digits({_, A, B}, {E}, {_, I}) ->
    {node(A, B), node(E, I)};
join_digits({_, A, B}, {E}, {_, I, J}) ->
    {node(A, B, E), node(I, J)};
join_digits({_, A, B}, {E}, {_, I, J, K}) ->
    {node(A, B, E), node(I, J, K)};
join_digits({_, A, B}, {E}, {_, I, J, K, L}) ->
    {node(A, B, E), node(I, J), node(K, L)};
join_digits({_, A, B, C}, {E}, {_, I}) ->
    {node(A, B, C), node(E, I)};
join_digits({_, A, B, C}, {E}, {_, I, J}) ->
    {node(A, B, C), node(E, I, J)};
join_digits({_, A, B, C}, {E}, {_, I, J, K}) ->
    {node(A, B, C), node(E, I), node(J, K)};
join_digits({_, A, B, C}, {E}, {_, I, J, K, L}) ->
    {node(A, B, C), node(E, I, J), node(K, L)};
join_digits({_, A, B, C, D}, {E}, {_, I}) ->
    {node(A, B, C), node(D, E, I)};
join_digits({_, A, B, C, D}, {E}, {_, I, J}) ->
    {node(A, B, C), node(D, E), node(I, J)};
join_digits({_, A, B, C, D}, {E}, {_, I, J, K}) ->
    {node(A, B, C), node(D, E, I), node(J, K)};
join_digits({_, A, B, C, D}, {E}, {_, I, J, K, L}) ->
    {node(A, B, C), node(D, E, I), node(J, K, L)};
join_digits({_, A}, {E, F}, {_, I}) ->
    {node(A, E), node(F, I)};
join_digits({_, A}, {E, F}, {_, I, J}) ->
    {node(A, E, F), node(I, J)};
join_digits({_, A}, {E, F}, {_, I, J, K}) ->
    {node(A, E, F), node(I, J, K)};
join_digits({_, A}, {E, F}, {_, I, J, K, L}) ->
    {node(A, E, F), node(I, J), node(K, L)};
join_digits({_, A, B}, {E, F}, {_, I}) ->
    {node(A, B, E), node(F, I)};
join_digits({_, A, B}, {E, F}, {_, I, J}) ->
    {node(A, B, E), node(F, I, J)};
join_digits({_, A, B}, {E, F}, {_, I, J, K}) ->
    {node(A, B, E), node(F, I), node(J, K)};
join_digits({_, A, B}, {E, F}, {_, I, J, K, L}) ->
    {node(A, B, E), node(F, I, J), node(K, L)};
join_digits({_, A, B, C}, {E, F}, {_, I}) ->
    {node(A, B, C), node(E, F, I)};
join_digits({_, A, B, C}, {E, F}, {_, I, J}) ->
    {node(A, B, C), node(E, F), node(I, J)};
join_digits({_, A, B, C}, {E, F}, {_, I, J, K}) ->
    {node(A, B, C), node(E, F, I), node(J, K)};
join_digits({_, A, B, C}, {E, F}, {_, I, J, K, L}) ->
    {node(A, B, C), node(E, F, I), node(J, K, L)};
join_digits({_, A, B, C, D}, {E, F}, {_, I}) ->
    {node(A, B, C), node(D, E), node(F, I)};
join_digits({_, A, B, C, D}, {E, F}, {_, I, J}) ->
    {node(A, B, C), node(D, E, F), node(I, J)};
join_digits({_, A, B, C, D}, {E, F}, {_, I, J, K}) ->
    {node(A, B, C), node(D, E, F), node(I, J, K)};
join_digits({_, A, B, C, D}, {E, F}, {_, I, J, K, L}) ->
    {node(A, B, C), node(D, E, F), node(I, J), node(K, L)};
join_digits({_, A}, {E, F, G}, {_, I}) ->
    {node(A, E, F), node(G, I)};
join_digits({_, A}, {E, F, G}, {_, I, J}) ->
    {node(A, E, F), node(G, I, J)};
join_digits({_, A}, {E, F, G}, {_, I, J, K}) ->
    {node(A, E, F), node(G, I), node(J, K)};
join_digits({_, A}, {E, F, G}, {_, I, J, K, L}) ->
    {node(A, E, F), node(G, I, J), node(K, L)};
join_digits({_, A, B}, {E, F, G}, {_, I}) ->
    {node(A, B, E), node(F, G, I)};
join_digits({_, A, B}, {E, F, G}, {_, I, J}) ->
    {node(A, B, E), node(F, G), node(I, J)};
join_digits({_, A, B}, {E, F, G}, {_, I, J, K}) ->
    {node(A, B, E), node(F, G, I), node(J, K)};
join_digits({_, A, B}, {E, F, G}, {_, I, J, K, L}) ->
    {node(A, B, E), node(F, G, I), node(J, K, L)};
join_digits({_, A, B, C}, {E, F, G}, {_, I}) ->
    {node(A, B, C), node(E, F), node(G, I)};
join_digits({_, A, B, C}, {E, F, G}, {_, I, J}) ->
    {node(A, B, C), node(E, F, G), node(I, J)};
join_digits({_, A, B, C}, {E, F, G}, {_, I, J, K}) ->
    {node(A, B, C), node(E, F, G), node(I, J, K)};
join_digits({_, A, B, C}, {E, F, G}, {_, I, J, K, L}) ->
    {node(A, B, C), node(E, F, G), node(I, J), node(K, L)};
join_digits({_, A, B, C, D}, {E, F, G}, {_, I}) ->
    {node(A, B, C), node(D, E, F), node(G, I)};
join_digits({_, A, B, C, D}, {E, F, G}, {_, I, J}) ->
    {node(A, B, C), node(D, E, F), node(G, I, J)};
join_digits({_, A, B, C, D}, {E, F, G}, {_, I, J, K}) ->
    {node(A, B, C), node(D, E, F), node(G, I), node(J, K)};
join_digits({_, A, B, C, D}, {E, F, G}, {_, I, J, K, L}) ->
    {node(A, B, C), node(D, E, F), node(G, I, J), node(K, L)};
join_digits({_, A}, {E, F, G, H}, {_, I}) ->
    {node(A, E, F), node(G, H, I)};
join_digits({_, A}, {E, F, G, H}, {_, I, J}) ->
    {node(A, E, F), node(G, H), node(I, J)};
join_digits({_, A}, {E, F, G, H}, {_, I, J, K}) ->
    {node(A, E, F), node(G, H, I), node(J, K)};
join_digits({_, A}, {E, F, G, H}, {_, I, J, K, L}) ->
    {node(A, E, F), node(G, H, I), node(J, K, L)};
join_digits({_, A, B}, {E, F, G, H}, {_, I}) ->
    {node(A, B, E), node(F, G), node(H, I)};
join_digits({_, A, B}, {E, F, G, H}, {_, I, J}) ->
    {node(A, B, E), node(F, G, H), node(I, J)};
join_digits({_, A, B}, {E, F, G, H}, {_, I, J, K}) ->
    {node(A, B, E), node(F, G, H), node(I, J, K)};
join_digits({_, A, B}, {E, F, G, H}, {_, I, J, K, L}) ->
    {node(A, B, E), node(F, G, H), node(I, J), node(K, L)};
join_digits({_, A, B, C}, {E, F, G, H}, {_, I}) ->
    {node(A, B, C), node(E, F, G), node(H, I)};
join_digits({_, A, B, C}, {E, F, G, H}, {_, I, J}) ->
    {node(A, B, C), node(E, F, G), node(H, I, J)};
join_digits({_, A, B, C}, {E, F, G, H}, {_, I, J, K}) ->
    {node(A, B, C), node(E, F, G), node(H, I), node(J, K)};
join_digits({_, A, B, C}, {E, F, G, H}, {_, I, J, K, L}) ->
    {node(A, B, C), node(E, F, G), node(H, I, J), node(K, L)};
join_digits({_, A, B, C, D}, {E, F, G, H}, {_, I}) ->
    {node(A, B, C), node(D, E, F), node(G, H, I)};
join_digits({_, A, B, C, D}, {E, F, G, H}, {_, I, J}) ->
    {node(A, B, C), node(D, E, F), node(G, H), node(I, J)};
join_digits({_, A, B, C, D}, {E, F, G, H}, {_, I, J, K}) ->
    {node(A, B, C), node(D, E, F), node(G, H, I), node(J, K)};
join_digits({_, A, B, C, D}, {E, F, G, H}, {_, I, J, K, L}) ->
    {node(A, B, C), node(D, E, F), node(G, H, I), node(J, K, L)};
join_digits(Left, Some, Right) -> error(function_clause, [Left, Some, Right]).
