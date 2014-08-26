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
  fcgx_init(Success, !IO),
  (
     Success = yes ->
       init_and_accept(procedure, !IO),
       spawn_threads(20, procedure, !IO)
     ;
       true
  ).

% This is the main procedure user must create to inject into fcgi loop
:- pred procedure(c_pointer::in, io::di, io::uo) is det.

procedure(Request, !IO) :-
  fcgx_puts_t(header, Request, _, !IO),
  fcgx_get_param_t("QUERY_STRING", Request, MaybeQuery, !IO),
  (
     MaybeQuery = yes(Query),
     fcgx_puts_t(format("Your query was ""%s""\n", [s(Query)]),
       Request, _, !IO)
  ;
     MaybeQuery = no,
     fcgx_puts_t("No query string\n", Request, _, !IO)
  ).

:- func header = string.

header = "Content-Type: text/plain\n\n".
