%% @doc Gives a good MIME type guess based on file extension.

-module(mimeheader_mod).

-export([mime_header_from_filename/1]).

-include("mimer.hrl").

%% @spec type_from_file_extension(S::string()) -> string() | undefined
%% @doc Given a filename extension (e.g. ".html") return a guess for the MIME
%%      type such as "text/html". Will return the atom undefined if no good
%%      guess is available.
type_from_file_extension(".html") ->
    "text/html";
type_from_file_extension(".xhtml") ->
    "application/xhtml+xml";
type_from_file_extension(".xml") ->
    "application/xml";
type_from_file_extension(".css") ->
    "text/css";
type_from_file_extension(".js") ->
    "application/x-javascript";
type_from_file_extension(".jpg") ->
    "image/jpeg";
type_from_file_extension(".gif") ->
    "image/gif";
type_from_file_extension(".png") ->
    "image/png";
type_from_file_extension(".swf") ->
    "application/x-shockwave-flash";
type_from_file_extension(".zip") ->
    "application/zip";
type_from_file_extension(".bz2") ->
    "application/x-bzip2";
type_from_file_extension(".gz") ->
    "application/x-gzip";
type_from_file_extension(".tar") ->
    "application/x-tar";
type_from_file_extension(".tgz") ->
    "application/x-gzip";
type_from_file_extension(".txt") ->
    "text/plain";
type_from_file_extension(".doc") ->
    "application/msword";
type_from_file_extension(".pdf") ->
    "application/pdf";
type_from_file_extension(".xls") ->
    "application/vnd.ms-excel";
type_from_file_extension(".rtf") ->
    "application/rtf";
type_from_file_extension(".mov") ->
    "video/quicktime";
type_from_file_extension(".mp3") ->
    "audio/mpeg";
type_from_file_extension(".z") ->
    "application/x-compress";
type_from_file_extension(".wav") ->
    "audio/x-wav";
type_from_file_extension(".ico") ->
    "image/x-icon";
type_from_file_extension(".bmp") ->
    "image/bmp";
type_from_file_extension(".m4a") ->
    "audio/mpeg";
type_from_file_extension(".m3u") ->
    "audio/x-mpegurl";
type_from_file_extension(".exe") ->
    "application/octet-stream";
type_from_file_extension(".csv") ->
    "text/csv";
type_from_file_extension(_) ->
    undefined.

%% @spec mime_header_from_filename(string()) -> header
%% @doc  Guess the mime header of a file by the extension of its filename.
mime_header_from_filename(File) ->
    case type_from_file_extension(filename:extension(File)) of
        undefined ->
            #header{
		        content_type = "text/plain"
		    };
        Mime ->
            #header{
                content_type = Mime
		    }
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

type_from_file_extension_test() ->
    ?assertEqual("text/html",
                 type_from_file_extension("monkey.html")),
    ?assertEqual(undefined,
                 type_from_file_extension("")),
    ?assertEqual(undefined,
                 type_from_file_extension(".voodoo_people")),
    ok.

-endif.

