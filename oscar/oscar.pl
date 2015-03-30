/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */


candidate_number(17655).

solve_task(Task,Cost):-
	b_setval(goal_position, Task),
	agent_current_position(oscar,P),
	solve_task_a(Task,[state(0,0,P,[P])],R,[],Cost,_NewPos), % sends the task 
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
% best-first search
% goal/1, children/2 and eval/2 depend on
% the search problem at hand
solve_task_a(Task,Agenda,RPath,_,[cost(Cost),depth(Depth)],NewPos) :-
	achieved(Task,Agenda,RPath,Cost,NewPos).
solve_task_a(Task,Agenda,RR,OldHistory,Cost,NewPos) :-
    Agenda = [state(_,_,Pos,_)|Rest],
    children(Agenda, OldHistory, History, Children),
    bestf_add(Children, Rest, NewAgenda),
    solve_task_a(Task, NewAgenda, RR, History, Cost, NewPos).

children(Agenda, OldHistory, History, Children) :-
	Agenda = [state(F,G,Pos,RPath)|Rest],
	bagof(N, map_adjacent(Pos, N, empty), Unfiltered_Children_pos),
	exclude(a_star_memberchk(OldHistory),Unfiltered_Children_pos,Children_pos), % filter
	union(OldHistory,Unfiltered_Children_pos,History),
	maplist(state_from_pos(Agenda, History), Children_pos, Children).

a_star_memberchk(List, Element) :-
	memberchk(Element, List).

state_from_pos(Agenda, History, ChildPos, State) :-
	Agenda = [state(F,G,Pos,RPath)|Rest],
	ChildG is G + 1,
	eval(ChildPos,ChildG,ChildF),
	State = state(ChildF, ChildG, ChildPos, [ChildPos|RPath]).

bestf_add([],Agenda,Agenda).
bestf_add([Child|Children],OldAgenda,NewAgenda):-
	add_state(Child,OldAgenda,TmpAgenda),
	bestf_add(Children,TmpAgenda,NewAgenda).

% add_one(S,A,B) <- B is A with S inserted acc. to eval/2
add_state(Child,[],[Child]).
add_state(Child,[Node|Rest],[Child,Node|Rest]):-
	Child = state(ChildF, _, _, _),
	Node = state(NodeF, _, _, _),
	ChildF<NodeF.
add_state(Child,[Node|Rest],[Node|NewRest]):-
	Child = state(ChildF, _, _, _),
	Node = state(NodeF, _, _, _),
	ChildF>=NodeF,
	add_state(Child,Rest,NewRest).

eval(ChildPos,ChildG,ChildF) :-
	b_getval(goal_position, go(Goal)),
	map_distance(ChildPos,Goal, H),
	ChildF is H + ChildG.

% Exit is the genius name for the goal position, p(X,Y)
achieved(go(Exit),[state(F,G,Pos,RPath)|Rest],RPath,Cost,NewPos) :-
	( Exit=none -> true % if there is no node to look for, stop the search
	; otherwise -> Exit = Pos % check if Exit is the first element of RPath
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
	( A=[]      -> nl
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
