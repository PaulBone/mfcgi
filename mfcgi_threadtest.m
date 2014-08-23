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
  init_and_accept(procedure, !IO),
  spawn_threads(20, procedure, !IO).

:- pred procedure(c_pointer::in, io::di, io::uo) is det.

procedure(Request, !IO) :-
  fcgx_write(header, Request, _, !IO),
  get_param_r("QUERY_STRING", Request, String, _, !IO),
  fcgx_write(String, Request, _, !IO).


:- func header = string.

header = "Content-Type: text/plain\n\n".