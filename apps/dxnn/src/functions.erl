-module(functions).
-compile(export_all).

avg(Xs) ->
	lists:sum(Xs) / length(Xs).

std(Xs) ->
	Avg = avg(Xs),
	std(Xs, Avg, []).
std([X|Xs], Avg, Acc) ->
	std(Xs, Avg, [math:pow(Avg-X, 2)|Acc]);
std([], _Avg, Acc) ->
	Variance = lists:sum(Acc) / length(Acc),
	math:sqrt(Variance).
%avg/1 and std/1 calculate the average and the standard deviation values of the lists passed to them.