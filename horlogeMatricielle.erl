-module(horlogeMatricielle).
-export([create_procs/1, setup/1, test/3, finish/0, requester/1, test_random/2, safe_keeping/0, test_fifo/3]).


% Find index. Retourne 0 si V n'est pas dans la table
find_index_aux(_, _, N) when N =< 0 -> error;
find_index_aux([], _, _) -> 0;
find_index_aux([H|_], V, N) when H == V -> N;
find_index_aux([H|T], V, N) when H =/= V -> find_index_aux(T, V, N+1);
find_index_aux(_, _, _) -> not_treated.

find_index(R, V) -> find_index_aux(R, V, 1).


% !!!Removing the nth element of a list!!!
remove(R, N) when N =< 0; N > length(R) -> error;
remove([], _) -> [];
remove([_|T], 1) -> T;
remove([H|T], N) -> lists:append([H], remove(T, N-1));
remove(_, _) -> not_treated.

% Creation d'un tableau de longeur N avec tous les valeurs initialisé a 0.
create_a_row(N) when N < 0 -> error;
create_a_row(0) -> lists:append([],[]);
create_a_row(N) when N > 0 -> lists:append([0], create_a_row(N-1));
create_a_row(_) -> not_treated.


% Creation d'un matrice NxM avec tous les valeurs initialisé a 0.
create_matrice_aux(N, _) when N =< 0 -> error;
create_matrice_aux(_, M) when M < 0 -> error;
create_matrice_aux(_, 0) -> lists:append([],[]);
create_matrice_aux(N, M) when M > 0 -> lists:append([create_a_row(N)], create_matrice_aux(N, M-1));
create_matrice_aux(_, _) -> not_treated.


% Creation d'un matrice carré NxN avec tous les valeurs initialisé a 0.
create_matrice(N) when N =< 0 -> error;
create_matrice(N) -> create_matrice_aux(N, N).


% Retourne la valeur de I'eme ligne J'ieme colonne de matrice M.
get_m(M, I, J) when I =< 0; J =< 0; I > length(M); J > length(M) -> error;
get_m(M, I, J) -> lists:nth(J, lists:nth(I, M)).


% Changer la valeur de J'ieme element d'un tableau R a V.
set_m_row_aux(R, J, _, N) when N < 0; J < 0; J > length(R); N > length(R) -> error;
set_m_row_aux(_, _, _, 0) -> lists:append([], []);
set_m_row_aux(R, 1, V, N) -> lists:append([V], set_m_row_aux(R, 0, V, N-1));
set_m_row_aux(R, 0, V, N) -> lists:append([lists:nth(length(R)-N+1, R)], set_m_row_aux(R, 0, V, N-1));
set_m_row_aux(R, J, V, N) -> lists:append([lists:nth(length(R)-N+1, R)], set_m_row_aux(R, J-1, V, N-1)).

set_m_row(R, J, V) -> set_m_row_aux(R, J, V, length(R)).


% Changer la valeur de I'eme ligne J'ieme colonne du matrice M a V.
set_m_aux(_, _, _, _, 0) -> lists:append([], []);
set_m_aux([H|T], 1, J, V, N) -> lists:append([set_m_row(H, J, V)], set_m_aux(T, 0, 0, V, N-1));
set_m_aux([H|T], 0, 0, V, N) -> lists:append([set_m_row(H, 0, V)], set_m_aux(T, 0, 0, V, N-1));
set_m_aux([H|T], I, J, V, N) -> lists:append([set_m_row(H, 0, V)], set_m_aux(T, I-1, J, V, N-1));
set_m_aux(_, _, _, _, _) -> not_treated.

set_m(M, I, J, V) -> set_m_aux(M, I, J, V, length(M)).


% creating a matrice using max values of two other matrices
max_m_aux(M1, _, I, J) when I =< 0; J =< 0; I > length(M1); J > length(M1) -> error;
max_m_aux(M1, M2, I, J) when length(M1) > J ->
   case get_m(M1, I, J) < get_m(M2, I, J) of
      true -> M_aux = set_m(M1, I, J, (get_m(M2, I, J))), max_m_aux(M_aux, M2, I, J+1);
      false -> M_aux = set_m(M2, I, J, (get_m(M1, I, J))), max_m_aux(M1, M_aux, I, J+1)
   end;
max_m_aux(M1, M2, I, J) when length(M1) == J, length(M1) > I ->
   case get_m(M1, I, J) < get_m(M2, I, J) of
      true -> M_aux = set_m(M1, I, J, (get_m(M2, I, J))), max_m_aux(M_aux, M2, I+1, 1);
      false -> M_aux = set_m(M2, I, J, (get_m(M1, I, J))), max_m_aux(M1, M_aux, I+1, 1)
   end;
max_m_aux(M1, M2, I, J) when length(M1) == J, length(M1) == I ->
   case get_m(M1, I, J) < get_m(M2, I, J) of
      true -> set_m(M1, I, J, (get_m(M2, I, J)));
      false -> set_m(M2, I, J, (get_m(M1, I, J)))
   end;
max_m_aux(_, _, _, _) -> not_treated.

max_m(M1, M2) -> max_m_aux(M1, M2, 1, 1).


% Incrementé I'eme ligne J'ieme colonne du matrice M
increment(M, I, J) when I =< 0; J =< 0; I > length(M); J > length(M) -> error;
increment(M, I, J) -> set_m(M, I, J, (get_m(M, I, J) + 1)).


% Printing a row of integers (avec ~n)
print_r_aux([], 1) -> io:format("[]", []);
print_r_aux([H|[]], 1) -> io:format("[~w]", [H]);
print_r_aux([H|[]], 0) -> io:format("~w]", [H]);
print_r_aux([H|T], 1) -> io:format("[~w, ", [H]), print_r_aux(T, 0);
print_r_aux([H|T], 0) -> io:format("~w, ", [H]), print_r_aux(T, 0).

print_r(R) -> print_r_aux(R, 1).


% Printing a matrix of integers (avec ~n)
print_m_aux([], 1) -> io:format("[]~n", []);
print_m_aux([H|[]], 1) -> io:format("[", []), print_r(H), io:format("]~n", []);
print_m_aux([H|[]], 0) -> io:format(" ", []), print_r(H), io:format("]~n", []);
print_m_aux([H|T], 1) -> io:format("[", []), print_r(H), io:format(",~n", []), print_m_aux(T, 0);
print_m_aux([H|T], 0) -> io:format(" ", []), print_r(H), io:format(",~n", []), print_m_aux(T, 0).

print_m(M) -> print_m_aux(M, 1).


% Modifier horloge H quand L on envoi un message au S.
horloge_update_send(H, L, S) when L =< 0; S =< 0; L > length(H); S > length(H) -> error;
horloge_update_send(H, L, S) -> M1 = increment(H, L, S), increment(M1, L, L).


% Modifier horloge H quand L reçois un message de S qui a envoyé horloge D.
horloge_update_recv(H, _, L) when L =< 0; L > length(H) -> error;
horloge_update_recv(H, D, L) -> M1 = max_m(H, D), increment(M1, L, L).


% Test FIFO avec horloge Locale L et horloge distante D avec sender S et receiver R
fifo_aux(L, D, S, R, N) when N == S -> fifo_aux(L, D, S, R, N+1);
fifo_aux(L, _, _, _, N) when N > length(L) -> true;
fifo_aux(L, D, S, R, N) when N =< length(L), N =/= S ->
   case get_m(D, N, R) > get_m(L, N, R) of
      true -> false;
      false -> fifo_aux(L, D, S, R, N+1)
   end.

fifo(L, D, S, R) ->
   case get_m(D, S, R) =/= (get_m(L, S, R) + 1) of
      true -> false;
      false -> fifo_aux(L, D, S, R, 1)
   end.


% regarde apres qu'on recois un message test pour voir si il y a un message dans PB qui a commencer a respecter FIFO
pb_check_aux(PB, [], HU, _, _) -> {PB, HU};
pb_check_aux(PB, [H|T], HU, MyI, N) ->
   I = element(1, H), D = element(2, H), Message = element(3, H),
   case fifo(HU, D, I, MyI) of
      true ->
         io:format("(~w) Recieved '~s' from ~w (FIFO RESPECTED)~n", [MyI, Message, I]),
         HU2 = horloge_update_recv(HU, D, MyI),
         print_m(HU2),
         PB2 = remove(PB, N),
         pb_check_aux(PB2, PB2, HU2, MyI, 1);
      false->
         pb_check_aux(PB, T, HU, MyI, N+1)
   end.

pb_check(PB, HU, MyI) -> pb_check_aux(PB, PB, HU, MyI, 1).


% Creer N processus qui peux communiquer 
create_procs_aux(N, _) when N < 0-> error;
create_procs_aux(0, []) ->  ok;
create_procs_aux(0, [H|T]) ->  H ! T, create_procs_aux(0, T);
create_procs_aux(N, Pids) -> Pid = spawn(horlogeMatricielle, setup, [Pids]), create_procs_aux(N-1, lists:append(Pids, [Pid])).

create_procs(N) -> create_procs_aux(N, []).


% Chaque processus va executer la fonctionne start.
start(Pids, H, PB) ->
   MyI = find_index(Pids, self()),
   receive
      % Cette partie est pour tester fifo
      {fifo, I, Message} ->
         P = lists:nth(I, Pids),
         HU = horloge_update_send(H, MyI, I),
         io:format("(~w) Sending '~s' to ~w~n", [MyI, Message, I]),
         print_m(HU),
         safe ! {P, MyI, HU, Message},
         start(Pids, HU, PB);
      
      % Cette partie est pour recuperé des les send requests
      {send_req, I, Message} ->
         P = lists:nth(I, Pids),
         HU = horloge_update_send(H, MyI, I),
         io:format("(~w) Sending '~s' to ~w~n", [MyI, Message, I]),
         print_m(HU),
         P ! {MyI, HU, Message},
         start(Pids, HU, PB);

      {I, D, Message} ->
         case fifo(H, D, I, MyI) of
            true ->
               io:format("(~w) Recieved '~s' from ~w~n", [MyI, Message, I]),
               HU = horloge_update_recv(H, D, MyI),
               print_m(HU),
               PBC = pb_check(PB, HU, MyI),
               HU2 = element(2, PBC),
               PB2 = element(1, PBC),
               start(Pids, HU2, PB2);
            false -> start(Pids, H, lists:append(PB, [{I, D, Message}]))
         end;

      kill ->
         io:format("(~w) Finished~n", [MyI])
   end.


% Envoyer un commande a tous les proccessus pour les terminer
finish_1by1([]) -> ok;
finish_1by1([H|[]]) -> H ! kill;
finish_1by1([H|T]) -> H ! kill, finish_1by1(T);
finish_1by1(_) -> not_treated.


% C'est pour dire au processus d'envoyer un message
requester(All_Pids) ->
   io:format("Requester Ready!~n", []),
   receive
      {I, D, Message} ->
         Pid = lists:nth(I, All_Pids),
         Pid ! {send_req, D, Message},
         requester(All_Pids);

      % c'est pour testé les messages retardée
      {fifo, I, D, Message} ->
         Pid = lists:nth(I, All_Pids),
         Pid ! {fifo, D, Message},
         requester(All_Pids); 

      kill -> finish_1by1(All_Pids)
   end.


% Cette fonction est pour retardée un message. Au lieu de enoyer le message directement on va l'envoyer ici. C'est seulement pour test des messages retardée.
safe_keeping() ->
   receive
      {P, MyI, HU, Message} ->
         timer:sleep(2000),
         P ! {MyI, HU, Message}
   end.


% Echange des pids entre processus
setup(Pids) ->
   receive
      P_rest -> All_Pids = lists:append([Pids, [self()], P_rest])
   end,
   Horloge = create_matrice(length(All_Pids)),
   I = find_index(All_Pids, self()),
   io:format("(~w) Ready!~n", [I]),
   case I == 1 of
      true ->
         register(safe, spawn(horlogeMatricielle, safe_keeping, [])),
         register(req_reg, spawn(horlogeMatricielle, requester, [All_Pids]));
      false -> pass
   end,
   start(All_Pids, Horloge, []).


% End all processus
finish() -> req_reg ! kill.

% Fonctions des tests
test(S, D, M) -> req_reg ! {S, D, M}.

test_fifo(S, D, M) -> req_reg ! {fifo, S, D, M}.

test_random_list(M, 1) ->
   S = rand:uniform(M),
   R = rand:uniform(M),
   [{S, R}];

test_random_list(M, N) ->
   S = rand:uniform(M),
   R = rand:uniform(M),
   lists:append([{S, R}], test_random_list(M, N-1)).

test_random_aux([H|[]]) ->
   S = element(1, H), R = element(2, H),
   timer:sleep(10),
   test(S, R, "random");

test_random_aux([H|T]) ->
   S = element(1, H), R = element(2, H),
   timer:sleep(10),
   test(S, R, "random"),
   test_random_aux(T).

test_random(M, N) ->
   List = test_random_list(M, N),
   test_random_aux(List).
