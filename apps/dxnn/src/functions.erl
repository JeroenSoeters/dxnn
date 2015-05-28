-module(functions).
-compile(export_all).

saturation(Val) ->
	case Val > 1000 of
		true ->
			1000;
		false ->
			case Val < -1000 of
				true ->
					-1000;
				false ->
					Val
			end
	end.

saturation(Val, Spread) ->
	case Val > Spread of
		true ->
			Spread;
		false ->
			case Val < -Spread of
				true ->
					-Spread;
				false ->
					Val
			end
	end.

scale(Xs, Min, Max) ->
	[scale_val(X, Min, Max) || X <- Xs]. 
scale_val(X, Min, Max) ->
	case Max == Min of
		true ->
			0;
		false ->
			(X*2-(Max+Min))/(Max-Min)
	end.

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

tanh(Val) ->
	math:tanh(Val).
