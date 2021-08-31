% "Generic Server" program

-module(gs).
-compile(export_all).


start(F,InitialState) ->
    spawn(?MODULE,generic_server,[F,InitialState]).


generic_server(F,State) ->
    receive
	{From,req,Ref,Data} ->
	    case catch(F(Data,State)) of
		{'EXIT',_} -> %% error and exit exceptions
		    From!{self(),err,Ref},
		    generic_server(F,State);
		{Reply,NewState} -> %% all good!
		    From!{self(),ans,Ref,Reply},
		    generic_server(F,NewState);
		_ -> %% throw exception
		    From!{self(),err,Ref},
		    generic_server(F,State)
	    end;
	{From,update,Ref,G} ->
	    From!{self(),ok,Ref},
	    generic_server(G,State);
	{From,read,Ref} ->
	    From!{self(),Ref,State},
	    generic_server(F,State)
end.

request(S,N) ->
    R=make_ref(),
    S!{self(),req,R,N},
    receive
	{S,ans,R,Reply} ->
	    Reply;
	{S,err,R} ->
	    error
    end.
