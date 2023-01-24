-module(crcuine).
-export([haiku/0, start/1, hash_master/4, hash_miner/3, rand_hash_miner/2, spawn_rand_miner/2, spawn_rand_miners/3, miner_spawner/3]).

hash_master(String, Checked, Continues, Restarts) ->
    receive
        {checked, Hash, PID} ->
            case sets:is_element(Hash, Checked) of
                true ->
                    PID ! restart,
                    hash_master(String, Checked, Continues, Restarts+1);
                false ->
                    PID ! continue,
                    hash_master(String, sets:add_element(Hash, Checked), Continues+1, Restarts)
            end;
        {eureka, Hash} ->
            io:format("Found a CRCuine!: ~p~n", [Hash]),
            io:format("End time: ~p~n", [os:timestamp()]);
        {status, PID} ->
            PID ! {status, sets:size(Checked), Continues/Restarts},
            hash_master(String, Checked, Continues, Restarts)
    end.

hash_miner(String, Hash, PID) ->
    FullString = String ++ string:to_lower(integer_to_list(Hash, 16)),
    NewHash = erlang:crc32(FullString),
    case Hash == NewHash of
        true ->
            io:format("Found a CRCuine!: ~p~n", [Hash]),
            PID ! {eureka, Hash};
        false ->
            PID ! {checked, Hash, self()}
    end,
    receive
        continue ->
            hash_miner(String, NewHash, PID);
        restart ->
            rand_hash_miner(String, PID)
    end.

rand_hash_miner(String, PID) ->
    hash_miner(String, rand:uniform(trunc(math:pow(2, 32))-1), PID).

spawn_rand_miner(String, PID) ->
    spawn(crcuine, rand_hash_miner, [String, PID]).

spawn_rand_miners(String, PID, N) ->
    io:format("Spawning miner... ~p left~n", [N]),
    case N of
        0 ->
            ok;
        _ ->
            spawn_rand_miner(String, PID),
            spawn_rand_miners(String, PID, N-1)
    end.

miner_spawner(PID, PrevChecked, MinerCount) ->
    erlang:send_after(1000, PID, {status, self()}),
    receive
        {status, N, AvgLength} ->
            HPS = N - PrevChecked,
            display_status(N, HPS, MinerCount, AvgLength),
            miner_spawner(PID, N, MinerCount)
    end.

display_status(Checked, HPS, MinerCount, AvgLength) ->
    io:format("############################################~n", []),
    io:format("# Hashes checked: ~p~n", [Checked]),
    io:format("# Hashes per second: ~p~n", [HPS]),
    io:format("# Miners: ~p~n", [MinerCount]),
    io:format("# Average length (Continues/restarts): ~p~n", [AvgLength]),
    io:format("############################################~n", []).

start(String) ->
    io:format("Starting CRCuine with string ~p~n", [String]),
    io:format("This will take a while...~n", []),
    io:format("Start time: ~p~n", [os:timestamp()]),

    StartingMiners = 100,
    Master_PID = spawn(crcuine, hash_master, [String, sets:new(), 0, 0]),
    io:format("Master PID: ~p~n", [Master_PID]),
    spawn_rand_miners(String, Master_PID, StartingMiners),
    spawn(crcuine, miner_spawner, [Master_PID, 0, StartingMiners]).

haiku() ->
    start("Data's true form sought\nCRC-32 ensures this haiku's heart\n").