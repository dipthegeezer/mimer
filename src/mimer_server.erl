-module(mimer_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(mserver, {
    mime_glob, % List of {file_ext, mime_type}
    mime_magic %
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([FileName]) ->
    case mimeglob_mod:parse_mime_glob(FileName) of
        {ok, MimeGlob} ->
            {ok, #mserver{mime_glob = MimeGlob}};
        Error ->
            {stop, Error}
    end.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @spec mimetype_from_file_extension(S::string()) -> string() | undefined
%% @doc Given a filename extension (e.g. ".html") return a guess for the MIME
%%      type such as "text/html". Will return the atom undefined if no good
%%      guess is available.

mimetype_from_file_extension( MimeFileExtList, Filename ) ->
    case lists:keyfind(filename:extension(Filename), 1, MimeFileExtList) of
        false ->
            undefined;
        {_, MimeType} ->
            MimeType
    end.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

mimetype_from_file_extension_test() ->
    FileInfo = [{".html","text/html"}],
    ?assertEqual("text/html",
                 mimetype_from_file_extension(FileInfo, "monkey.html")),
    ?assertEqual(undefined,
                 mimetype_from_file_extension(FileInfo,"")),
    ?assertEqual(undefined,
                 mimetype_from_file_extension(FileInfo,"magic_people.voodoo_people")),
    ok.

-endif.
