-module(crcuine).
-export([start/1, hash_master/2, hash_miner/3]).

hash_master(String, Checked) ->
    receive
        {checked, Hash, PID} ->
            case sets:is_element(Hash, Checked) of
                true ->
                    io:format("Ran into a loop, relocating~n", []),
                    PID ! restart;
                false ->
                    Checked = sets:add_element(Hash, Checked),
                    case sets:size(Checked) rem 1000 of
                        0 ->
                            io:format("Checked ~p hashes~n", [sets:size(Checked)]);
                        _ ->
                            ok
                    end,
                    PID ! continue
            end;
        {eureka, Hash} ->
            io:format("Found a CRCuine!: ~p~n", [Hash]),
            exit(0)
    end,
    hash_master(String, Checked).

hash_miner(String, Hash, PID) ->
    FullString = String ++ string:to_lower(integer_to_list(Hash, 16)),
    NewHash = erlang:crc32(FullString),
    case Hash == NewHash of
        true ->
            PID ! {eureka, Hash, self()};
        false ->
            % io:format("Checked ~p~n", [Hash]),
            PID ! {checked, Hash}
    end,
    hash_miner(String, NewHash, PID).

start(String) ->
    Master_PID = spawn(crcuine, hash_master, [String, sets:new()]),
    spawn(crcuine, hash_miner, [String, 0, Master_PID]).
    % Crc = string:to_lower(integer_to_list(erlang:crc32(String), 16)),
    % io:format("~p~n", [Crc]).