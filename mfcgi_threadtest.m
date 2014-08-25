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
:- import_module mfcgi.threads.

% spawn N (in this case 20) threads with procedure defined below
main(!IO) :-
  init_and_accept(procedure, !IO),
  spawn_threads(20, procedure, !IO).  

% This is the main procedure user must create to inject into fcgi loop
:- pred procedure(c_pointer::in, io::di, io::uo) is det.

procedure(Request, !IO) :-
  fcgx_write(header, Request, _, !IO),
  fcgx_get_param_r("QUERY_STRING", Request, MaybeQuery, !IO),
  (
     MaybeQuery = yes(Query),
     fcgx_write(format("Your query was ""%s""\n", [s(Query)]),
       Request, _, !IO)
  ;
     MaybeQuery = no,
     fcgx_write("No query string\n", Request, _, !IO)
  ).

:- func header = string.

header = "Content-Type: text/plain\n\n".
