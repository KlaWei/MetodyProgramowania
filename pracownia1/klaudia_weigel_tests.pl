:- module(klaudia_weigel_tests, [tests/5]).

:- op(200, fx, ~).
:- op(500, xfy, v).

tests(simple_validity_test_1, validity, [p v ~q, p v q], 500, solution([(p, t), (q, t)])).
tests(simple_validity_test_2, validity, [p, q, ~q], 500, count(0)).
tests(simple_validity_test_3, validity, [p, q, r v ~q], 700, solution([(p, t), (q, t), (r, t)])).
tests(simple_validity_test_4, validity, [p v ~q, ~p v q, ~p v ~q], 900, solution([(p, f), (q, f)])).
tests(simple_validity_test_5, validity, [p v ~p v q, q v p v ~p], 800, count(4)).
tests(simple_validity_test_6, validity, [p v ~q v ~r, ~p v ~r v s, ~p v ~q v s], 1200, solution([(p, f), (q, f), (r, f), (s, f)])).
tests(simple_validity_test_7, validity, [p, ~p], 500, count(0)).


tests(validity_test_8, validity, [~p v ~q v r, ~p v q v ~s, p v ~q v ~s, ~p v ~r v ~s, p v q v ~r, p v ~r v ~s],
      2500, count(7)).
tests(validity_test_9, validity, [p v ~q, q v ~r, r v ~p, p v q v r, ~p v ~q v ~r], 2000, count(0)).
tests(validity_test_10, validity, [p v q v r, p v ~q v ~s, q v ~r v s, ~p v r v s, ~p v q v ~s, p v ~q v ~r,
      ~p v ~q v s, ~p v ~r v ~s], 4000, solution([(p, f), (q, f), (r, t), (s, t)])).
    
tests(validity_test_11, validity, [p v q, p v s, p v r, ~q v ~r, ~q v ~s, ~p v ~r, ~s v ~r, ~p v ~q,
      ~p v ~s], 2500, solution([(p, t), (q, f), (s, f), (r, f)])).
tests(semi_difficult_test_5, validity, [p v ~q, ~p v q, ~q v r, p v r, p v ~r, ~q v ~r], 2200, count(0)).
    
tests(validity_test_12, validity, [p v ~p v q v s, ~q v s v r, ~s v q v r, ~r v s v q, ~s v w v u, ~w v q v s, 
      ~u v r v s, p v u v w], 6000, count(26)).
tests(validity_test_13, validity, [p v ~p v s, ~s v s v r, ~r v r v q, ~q v q v w, ~w v w v u,
      ~u v u v t, ~t v t v p], 10000, count(128)).


tests(performance_test_1, performance, [p, q, p v r v s, ~p v q, ~p v w, q v s, s v ~r], 500, 
     solution([(p, t), (q, t), (s, t), (w, t), (r, t)])).
    