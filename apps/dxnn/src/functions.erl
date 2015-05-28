-module(functions).
-compile(export_all).

saturation(X) ->
	case X > 1000 of
		true ->
			1000;
		false ->
			case X < -1000 of
				true ->
					-1000;
				false ->
					X
			end
	end.

saturation(X, Spread) ->
	case X > Spread of
		true ->
			Spread;
		false ->
			case X < -Spread of
				true ->
					-Spread;
				false ->
					X
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

scale_dzone(X, Min, Max, DZMin, DZMax) ->
	case (X > DZMin) and (X < DZMax) of
		true ->
			0;
		false ->
			scale_val(X, Min, Max)
	end.

tanh(X) ->
	math:tanh(X).

cos(X) ->
	math:cos(X).

sin(X) ->
	math:sin(X).

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

