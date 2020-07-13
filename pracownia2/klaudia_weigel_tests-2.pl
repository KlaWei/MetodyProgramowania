:- module(klaudia_weigel_tests, [resolve_tests/5, prove_tests/4]).

:- op(200, fx, ~).
:- op(500, xfy, v).

resolve_tests(res_test_0, p, p v q v r, ~q v s v ~p, q v ~q v r v s).
resolve_tests(res_test_1, p, ~p v ~q, p v q, ~q v q).
resolve_tests(res_test_2, b, b v c v ~a, ~b v ~d v ~e v f, c v f v ~a v ~d v ~e).
resolve_tests(res_test_3, b, ~f v ~k v ~u v b, ~b v k, ~f v ~k v k v ~u).
resolve_tests(res_test_4, d, ~a v b v c v d v e, t v ~d v ~u v ~g v j, b v c v e v j v t v ~a v ~g v ~u).
resolve_tests(res_test_5, p, p v ~p, ~p v r, ~p v r).
resolve_tests(res_test_6, p, p, ~p v q v r, q v r).


prove_tests(empty_clause, validity, [], sat).

prove_tests(instant_contr, validity, [a, b, ~a, ~b], unsat).

prove_tests(validity_test_1, validity, [a, ~a v ~b v ~c, ~a v b v c, a v ~b v c, a v b v ~c, ~a v b, ~a v c], unsat).

prove_tests(validity_test_2, validity, [~a, a v ~b v ~c, b, c v ~d v ~e, e v f, d, ~f], unsat).

prove_tests(validity_test_3, validity, [a, ~b v ~c v ~a, ~b v c v a, b v ~c v a, b v c v ~a, ~p v ~q v c, q v ~c, p v ~c, ~p v ~q v b, q v ~b, p v ~b], unsat).

prove_tests(validity_test_4, validity, [a, ~b v ~c v ~a, b v c v ~a, ~a v d v e, ~b v ~d v ~e v f, f v g v h, ~d v ~f v ~g, ~g v f, ~f v e, ~e v h, ~h], unsat).

prove_tests(simple_tautology_test, validity, [p v ~p v q, ~q v q v p], sat).

prove_tests(tautology_test, validity, [p v ~p v q v r, q v ~q v s v t, r v ~r v t, ~t v t v s, ~s v s v p], sat).

prove_tests(validity_test_5, validity, [a v b, ~b v c v d, ~b v e, ~d v ~e v f, a v c v f, ~a v g, ~g v b, c v ~b, ~b], unsat).

prove_tests(validity_test_6, validity, [a v b v c v d v e v f v g v h, ~a, ~b, ~c, ~d, ~f, ~g, ~h, ~e], unsat).

prove_tests(validity_test_7, validity, [p v q v ~r, p v ~q, ~p, r, u], unsat).

prove_tests(validity_test_8, validity, [p v q, ~q, ~p v q v ~r], sat).

prove_tests(validity_test_9, validity, [p v q, p v ~q, ~p v q, ~p v ~r, ~q v r], unsat).

prove_tests(validity_test_10, validity, [p v q v r, ~p v ~q v r, ~p v q v ~r, p v ~q v ~r, ~p v q v r, ~p v ~q v ~r, p v ~q v r, p v q v ~r], unsat).

prove_tests(long_validity_1, validity, [p, p v r, q v r v t, p v r v ~r v s, s v ~q v r v q v t, ~q v ~p, p v q v ~p, p v t v ~p, r v s v ~r, p v s v ~q, p v t v ~t, a v p, a v ~a v r, a v p v ~a, a v q v ~a, r v a v s v ~r v ~a, b v s v p v q v ~b, b v a v ~b], unsat).

prove_tests(long_validity_2, validity, [~a, ~a v b v c v d v e, ~e v w v f v a v s, ~d v ~c v ~b v ~a v f, ~h, ~a v ~f, ~o v ~p, h v ~g, ~e v ~f v u, ~g v a v ~c v s v d, ~b v a v ~d v c v ~f, ~h v g v ~j v i v ~k, r v ~l v n v ~m v d, g v ~f v ~l v ~h v s, k v d v j v f v l, t v ~d v ~u v ~g v j, ~s v j v k v ~d v ~r, u v ~g v ~a v s v k, ~l v h v s v ~e v a, ~o v ~j v ~d v s v e, ~d v s v ~f v ~p v ~r, f v ~e v ~d v ~c, h v ~i v ~j, ~r v ~s v w, ~h v ~b v ~n v s v f, ~i v ~g v s v b v ~j, ~d v h v ~w v ~f v u, ~e v r v u v ~o v ~p, d v h v ~w v ~h v ~g, s v ~b v ~g v k v i, f v ~j v ~l v ~o v k v r, l v s v j v t v r, f v i v g v k v b, ~b v d v ~w v l v ~n, m v ~t v ~u v ~e v c, ~c v j v ~d v s v ~a, ~r v s v b v ~l v ~o, h v i, h v b, o v ~n v ~m, t v ~u v ~w, a v ~d v j v ~k, p v r, h v d, ~i v h], unsat).

prove_tests(perf_test_1, performance, [a, ~a v ~b v ~c, c v b v ~a, ~c v a v d, ~d v b, e v f v ~b, ~e v ~f v b, ~a v ~f, ~a v ~c, a v ~e v b, g v e v ~a, ~g v ~f v a v b, f v e v b v a v ~c, b v a v ~b, ~b v c v g, ~e v ~b], unsat).

prove_tests(perf_test_2, performance, [a, a v ~b, a v ~c, a v b v d, a v ~b v d, a v b v ~d, ~a v b v d, a v c v d, ~a v c v d, a v ~c v d, a v c v ~d, a v e, a v ~e v f, a v ~f v e, ~a v e v f, b v e v d, ~b v d v e, b v d v ~e, b v ~d v e, g v h v ~i, ~g v h v i, g v ~h v i, f v ~g, f v ~h, f v ~i, e v ~g, e v ~h, e v ~i, ~a v ~f, ~a v ~g, ~a v ~i, ~a v ~d v ~e, ~a v ~b v ~c], unsat).

prove_tests(perf_test_3, performance, [a, ~a v ~b v ~c, ~a v b v c, a v ~b v c, a v b v ~c, ~b v ~d v ~e, ~b v d v e, b v ~d v e, b v d v ~e, ~d v ~f v ~g, ~d v f v g, d v ~f v g, d v f v ~g, ~f v ~d v ~c, ~f v d v c, f v ~d v c, f v d v ~c, ~c v ~e v ~g, ~c v e v g, c v ~e v g, c v e v ~g, ~g v ~a v ~f, ~g v a v f, g v ~a v f, g v a v ~f, ~a v ~b v ~f], unsat).

prove_tests(perf_test_5, performance, [a1, a1 v a2, a1 v a2 v a3, a1 v a2 v a3 v a4, a1 v a2 v a3 v a4 v a5, a1 v a2 v a3 v a4 v a5 v a6, a1 v a2 v a3 v a4 v a5 v a6 v a7, ~a1 v ~a2, ~a1 v a2 v ~a3, a1 v ~a2 v a3 v ~a4, ~a1 v a3 v a5 v ~a6 v ~a7 v a8, ~a8 v ~a1, ~a1 v ~a2 v ~a8 v a3 v ~a5, ~a6 v a7 v ~a3, ~a6 v a1 v ~a2 v ~a8, a9 v ~a4 v ~a5 v ~a1, a4 v ~a9 v a10, ~a10 v a4 v ~a5, a5 v a4 v a10 v ~a9, ~a1 v ~a10 v ~a6 v ~a9, ~a4 v ~a5, ~a10 v ~a6 v a8 v ~a1, ~a1 v ~a4, a4 v a8 v a9, a2 v a3 v ~a9], unsat).

prove_tests(perf_test_6, performance, [~b, d v ~j v ~k, a v g v h v ~u, ~l v ~r v ~g v t, d v j v ~k, g v ~a v ~m, c v w v ~b v ~u, ~s v ~k v ~l, l v ~d, g v s v ~t v ~d, ~c v ~g v ~l v j, s v z v g v j, d v ~e v w v ~s, ~f v ~k v ~u v b, ~a v t v ~j v u, ~h v ~w v t v l, ~d v ~l v t v y, ~i v ~t v ~r v d, d v g v s v r, ~s v b v c v j, ~f v ~s v ~j, l v ~f v r v ~d, b v d v g v d, s v f v u v w, ~f v ~l v ~t, ~g v ~d v s, ~l v ~d v g v b, ~m v r v t v ~k, ~k v ~g v ~j v ~i, s v ~p, s v d v r v ~f, ~c v z v ~a v d, p v ~u v f, ~f v ~d v ~f v a, ~k v ~a v s v ~l, g v u v ~l v ~c, e v ~d v a v ~k v ~l, ~d v g v ~b v ~m, ~d v b v f v ~l, s v ~g v ~d v ~a, ~l v ~w v ~h v a, ~j v ~h v s v l, ~a v b v h v ~m, ~p v b v m v ~h, ~o v b v r v w, ~u v ~j v ~s, h v r, ~s v k, g v ~h v ~i, ~l v m, d v ~a, t v ~y, ~b v ~c, b v s], unsat).

prove_tests(perf_test_7, performance, [a1, ~a1 v ~a2 v ~a3, ~a1 v a2 v a3, a1 v ~a2 v a3, a1 v a2 v ~a3, ~a4 v ~a5, ~a4 v a5, a4 v ~a5, a1 v a4 v a6 v a7, ~a1 v ~a4 v a6 v a7, ~a1 v a4 v ~a6 v a7, ~a1 v a4 v a6 v ~a7, a1 v ~a4 v ~a6 v a7, a1 v ~a4 v a6 v ~a7, a1 v a4 v ~a6 v ~a7, ~a1 v ~a4 v ~a6 v a7, ~a1 v ~a4 v a6 v ~a7, a1 v ~a4 v ~a6 v ~a7, ~a2 v ~a6 v ~a8, ~a2 v a6 v a8, a2 v ~a6 v a8, a2 v a6 v ~a8, a3 v a5 v a7 v a8, ~a3 v ~a5 v a7 v a8, ~a3 v a5 v ~a7 v a8, ~a3 v a5 v a7 v ~a8, a3 v ~a5 v ~a7 v a8, a3 v ~a5 v a7 v ~a8, a3 v a5 v ~a7 v ~a8, ~a3 v ~a5 v ~a7 v a8, ~a3 v ~a5 v a7 v ~a8, a3 v ~a5 v ~a7 v ~a8, ~a6 v ~a7 v ~a8, ~a6 v a7 v a8, a6 v ~a7 v a8, a6 v a7 v ~a8, ~a2 v ~a3 v ~a6, ~a2 v a3 v a6, a2 v ~a3 v a6, a2 v a3 v ~a6, ~a1 v ~a3 v ~a8, ~a1 v a3 v a8, a1 v ~a3 v a8, a1 v a3 v ~a8], unsat).

prove_tests(perf_test_4, performance, [a1 v ~a2 v a3, ~a4 v a5 v a1, ~a6 v ~a7 v ~a8, ~a9 v a10 v a11, ~a12 v a13 v a14, a15 v ~a16 v a3, a17 v ~a18 v a19, ~a20 v ~a21 v a22, ~a23 v ~a4 v a24, a16 v a15 v a25, ~a26 v a27 v ~a28, a29 v a30 v a14, a25 v ~a26 v a31, ~a19 v ~a7 v ~a32, a29 v a20 v a19, ~a15 v ~a18 v a23, ~a30 v ~a31 v a22, a4 v a14 v ~a24, a28 v a27 v ~a21, ~a25 v ~a27 v a30, ~a25 v ~a27 v ~a8, ~a7 v ~a29 v ~a25, ~a7 v ~a8 v ~a25, ~a6 v ~a7 v a4, a26 v a25 v a1, a28 v ~a12 v a24, a1 v a15 v ~a16, ~a2 v a32 v ~a13, a19 v ~a21 v ~a22, ~a23 v ~a13 v a10, ~a1 v ~a32, ~a1 v a32, a1 v ~a32, ~a32 v ~a2, ~a32 v a2, a32 v ~a2, ~a2 v ~a31, ~a2 v a31, a2 v ~a31, ~a31 v ~a3, ~a31 v a3, a31 v ~a3, ~a3 v ~a30, ~a3 v a30, a3 v ~a30, ~a30 v ~a4, ~a30 v a4, a30 v ~a4, ~a4 v ~a29, ~a4 v a29, a4 v ~a29, ~a29 v ~a5, ~a29 v a5, a29 v ~a5, ~a5 v ~a28, ~a5 v a28, a5 v ~a28, ~a28 v ~a6 v ~a27, ~a28 v a6 v a27, a28 v ~a6 v a27, a28 v a6 v ~a27, ~a6 v ~a27 v ~a7, ~a6 v a27 v a7, a6 v ~a27 v a7, a6 v a27 v ~a7, ~a27 v ~a8 v ~a26, ~a27 v a8 v a26, a27 v ~a8 v a26, a27 v a8 v ~a26, ~a8 v ~a25 v ~a9, ~a8 v a25 v a9, a8 v ~a25 v a9, a8 v a25 v ~a9, ~a25 v ~a10, ~a25 v a10, a25 v ~a10, ~a10 v ~a24, ~a10 v a24, a10 v ~a24, ~a24 v ~a11, ~a24 v a11, a24 v ~a11, ~a11 v ~a23, ~a11 v a23, a11 v ~a23, ~a23 v ~a12, ~a23 v a12, a23 v ~a12, ~a12 v ~a22, ~a12 v a22, a12 v ~a22, ~a22 v ~a13, ~a22 v a13, a22 v ~a13, ~a13 v ~a21, ~a13 v a21, a13 v ~a21, ~a21 v ~a14, ~a21 v a14, a21 v ~a14, ~a14 v ~a20, ~a14 v a20, a14 v ~a20, ~a20 v ~a15 v ~a19, ~a20 v a15 v a19, a20 v ~a15 v a19, a20 v a15 v ~a19], unsat).
