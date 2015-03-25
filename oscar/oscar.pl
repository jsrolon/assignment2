/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */


candidate_number(17655).

solve_task(Task,Cost):-
	agent_current_position(oscar,P),
	solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos), % sends the task 
    !, % prune choice point for efficiency
	reverse(R,[_Init|Path]), % Removes first path element because agent is already there
	agent_do_moves(oscar,Path).

%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
	achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
	Current = [c(F,P)|RPath], % Current is a list with first element c(F,P) where P is the current node position p(X,Y)
				  % and F is the cost from the first node of the list to that position. RPath is the Reverse
				  % Path from the first node to the goal, that is, the path from the goal to the first node.
				  % Its members are of the form p(X,Y).
	search(P,P1,R,C), % returns P1 = R, the next position to be visited. C is always 1 (the weight between graph nodes).
	\+ memberchk(R,RPath),
	D1 is D+1, % search depth calculation
	F1 is F+C, % next node cost calculation
	solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos). % This adds the next node to the list both as the c(F,P)
								  % structure and the position itself (P). As RR (ReversePath),
								  % Cost and NewPos are constant, they must be used when a goal
								  % is found.

%% agenda-based A* search
solve_task_a([Goal|Tail], Goal).
solve_task_a([Current|Tail], Goal) :-
    children(Current, Children),
    add_to_agenda(Children, Tail, NewAgenda),
    solve_task_a(NewAgenda, Goal).
children(Current, Children) :-.
eval_heuristic.

%% agenda-based A* search
% best-first search
% goal/1, children/2 and eval/2 depend on
% the search problem at hand
search_bstf([Goal|Rest],Goal):-
	goal(Goal).

search_bstf([Current|Rest],Goal):-
	children(Current,Children),
	add_bstf(Children,Rest,NewAgenda),
	search_bstf(NewAgenda,Goal).



add_bstf([],Agenda,Agenda).

add_bstf([Child|Children],OldAgenda,NewAgenda):-
	add_one(Child,OldAgenda,TmpAgenda),
	add_bstf(Children,TmpAgenda,NewAgenda).


% add_one(S,A,B) <- B is A with S inserted acc. to eval/2
add_one(Child,OldAgenda,NewAgenda):-
	eval(Child,Value),
	add_one(Value,Child,OldAgenda,NewAgenda).

add_one(Value,Child,[],[Child]).

add_one(Value,Child,[Node|Rest],[Child,Node|Rest]):-
	eval(Node,V),
	Value<V.

add_one(Value,Child,[Node|Rest],[Node|NewRest]):-
	eval(Node,V),
	Value>=V,
	add_one(Value,Child,Rest,NewRest).

eval(Child,V) :-







% Exit is the genius name for the goal position, p(X,Y)
achieved(go(Exit),Current,RPath,Cost,NewPos) :-
	Current = [c(Cost,NewPos)|RPath],
	( Exit=none -> true % if there is no node to look for, stop the search
	; otherwise -> RPath = [Exit|_] % check if Exit is the first element of RPath
	).
achieved(find(O),Current,RPath,Cost,NewPos) :-
	Current = [c(Cost,NewPos)|RPath],
	( O=none    -> true
	; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
	).

% Search takes a position F (p(X,Y)) and returns adjacent position N (p(X,Y)) with an arc weight of 1 between them
search(F,N,N,1):-
	map_adjacent(F,N,empty).


%%% command shell %%%

shell:-
	get_input(Input),
	handle_input(Input).

handle_input(Input):-
	( Input = stop -> true
	; Input = reset -> ailp_reset,shell
	; Input = [H|T] -> handle_input(H),handle_input(T),shell
	; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
	; otherwise -> show_response('Unknown command, please try again.'),shell
	).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_response(R):-
	( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
	; R=console(Response) -> term_to_atom(Response,A),do_command([oscar,console,A])
	; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
	; R=agent(Response)   -> term_to_atom(Response,A),do_command([oscar,say,A])
	; R=[H|T]             -> show_response(H),show_response(T)
	; R=[]                -> true
	; otherwise           -> writes(['! ',R])
	).

writes(A):-
	( A=[]      -> nls
	; A=nl      -> nl
	; A=[H|T]   -> writes(H),writes(T)
	; A=term(T) -> write(T)
	; otherwise -> write(A)
	).

% callable(+Command, +Goal, ?Response)
callable(call(G),call(G),G).
callable(topup(S),agent_topup_energy(oscar,S),agent(topup)).
callable(energy,agent_current_energy(oscar,E),both(current_energy(E))).
callable(position,agent_current_position(oscar,P),both(current_position(P))).
callable(ask(S,Q),agent_ask_oracle(oscar,S,Q,A),A).
callable(Task,solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)
