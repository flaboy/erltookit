-module(launcher).
-export([init/3]).
-export([cmd/1, start_link/1, start_link/2]).
-export([send/2, controlling_process/2, close/1, wait/1]).

cmd(Command)->
    os:cmd(Command).

start_link(Command)-> start_link(Command,[]).
start_link(Command,Options)->
    Pid = spawn_link(?MODULE,init,[Command,Options,self()]),
    {ok, Pid}.

init(Command,Options,Ctl)->
    
    Kill = case lists:keyfind(kill,1,Options) of {kill,K} -> integer_to_list(K); _->"15" end,
    Env = case lists:keyfind(env,1,Options) of {env,E} -> E; _->[] end,
    Log = case lists:keyfind(log,1,Options) of {log,L} -> L; _->"disabled" end,
    
    Args = ["-k",Kill,"-l",Log,Command] ++ case lists:keyfind(args,1,Options) of
        {args,A} -> A;
        _->[]
    end,
    
    Port = open_port( {spawn_executable, binpath()}, 
        [in, out, use_stdio, stderr_to_stdout, binary, exit_status, {args,Args} , {env,Env}] ),
    loop(Port,Ctl).

loop(Port,Ctl)->
    receive
        {controlling_process, Pid}->
            loop(Port,Pid);
        {send, Data}->
            port_command(Port, Data),
            loop(Port,Ctl);
        {Port, {exit_status, Code}}->
            Ctl ! {self(), exit, Code};
        {Port, {data, Message}}->
            Ctl ! {self(), data, Message},
            loop(Port,Ctl);
        close->
            port_close(Port);
        M->
            io:format("~p\n",[M]),
            loop(Port,Ctl)
    end.

binpath()->
    {_,_,Path} = code:get_object_code(?MODULE),
    dirname(dirname(Path)) ++ "/priv/launcher".

dirname(Path)-> string:substr(Path,1,string:rchr(Path,$/)-1).

controlling_process(Proc, Pid) when is_pid(Proc) ->
    true = is_process_alive(Proc),
    Proc ! {controlling_process, Pid}.
    
send(Proc, Data) when is_pid(Proc)->
    true = is_process_alive(Proc),
    Proc ! {send, Data}.

wait(Proc) when is_pid(Proc)->
    true = is_process_alive(Proc),
    receive
        {Proc, data, Message}->
            io:format("~s\n",[Message]),
            wait(Proc);
        {Proc, exit, Code}->
            io:format("exit: ~p\n",[Code])
    end.

close(Proc) when is_pid(Proc)->
    true = is_process_alive(Proc),
    Proc ! close.