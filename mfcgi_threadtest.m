% FCGI Multi threaded demo
%
:- module mfcgi_threadtest.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module mfcgi.
:- import_module mfcgi.basic.
:- import_module mfcgi.multi.

main(!IO) :-
  fcgx_init(_, !IO),
  fcgx_init_request(_, Request, 0, 0, !IO),
  fcgx_accept_r(_, Request, !IO),
  fcgx_write(header, Request, _, !IO),
  fcgx_get_param_r("QUERY_STRING", Request, MaybeQuery, !IO),
  (MaybeQuery = yes(Query) ->
     fcgx_write(Query, Request, _, !IO)
     ;
     fcgx_write("No Query\n", Request, _, !IO)
  ),
  fcgx_finish_r(Request, !IO),
  main(!IO).



:- func header = string.

header = "Content-Type: text/plain\n\n".

