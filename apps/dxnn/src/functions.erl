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

sgn(0) ->
	0;
sgn(X) ->
	case X > 0 of
		true ->
			1;
		false ->
			-1
	end.

bin(X) ->
	case X > 0 of
		true ->
			1;
		false ->
			0
	end.

multiquadratic(X) ->
	math:pow(X * X + 0.01, 0.5).

absolute(X) ->
	abs(X).

linear(X) ->
	X.

quadratic(X) ->
	sgn(X) * X * X.

gaussian(X) ->
	gaussian(2.71828183,X).

gaussian(Const,X)->
	V = case X > 10 of
		true ->
			10;
		false ->
			case X < -10 of
				true ->
					-10;
				false ->
					X
			end
	end,
	math:pow(Const,-V*V).

sqrt(X) ->
	sgn(X) * math:sqrt(abs(X)).

std(Xs) ->
	Avg = avg(Xs),
	std(Xs, Avg, []).
std([X|Xs], Avg, Acc) ->
	std(Xs, Avg, [math:pow(Avg-X, 2)|Acc]);
std([], _Avg, Acc) ->
	Variance = lists:sum(Acc) / length(Acc),
	math:sqrt(Variance).
%avg/1 and std/1 calculate the average and the standard deviation values of the lists passed to them.

