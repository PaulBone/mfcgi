% Mercury-fcgi library binding
% Contains thread-safe interface
%
% This binding is a simple wrapping of the C interface.
%
% Authors: Ebrahim Azarisooreh, Paul Bone

:- module threads.

:- interface.

:- import_module bool.
:- import_module maybe.
:- import_module io.

%--------------------------------------------------------------------------%
% Thread safe interface.
%--------------------------------------------------------------------------%

% finish request accepted by previous call to fcgx_accept
% also frees any storage allocated by previous call
:- pred fcgx_finish_r(c_pointer::in, io::di, io::uo) is det.

% initialize FCGX library. This is called by fcgx_accept
% but should be caleld when using fcgx_accept_r
:- pred fcgx_init(bool::out, io::di, io::uo) is det.

% accept new request from HTTP server, 0 is success, -1 is error (thread-safe)
:- pred fcgx_accept_r(bool::out, c_pointer::in, io::di, io::uo) is det.

% Initialize an FCGX_Request for use with FCGX_Accept_r(), 0 is success
:- pred fcgx_init_request(bool::out, c_pointer::out, int::in, int::in,
  io::di, io::uo) is det.

% writing string to buffer in FCGX_Request struct 
:- pred fcgx_puts_t(string::in, c_pointer::in, bool::out, io::di, io::uo) is det.

% get the parameters, and return result as maybe string
:- pred fcgx_get_param_t(string::in, c_pointer::in, maybe(string)::uo,
  io::di, io::uo) is det.

% spawn N number of threads, passing request pointer and a 'subroutine'
% predicate
:- pred spawn_threads(int::in,
  pred(c_pointer, io, io)::in(pred(in, di, uo) is det),
  io::di, io::uo) is det.

% initialize and accept request, then execute procedure loop
:- pred init_and_accept(pred(c_pointer, io, io)::in(pred(in, di, uo) is det),
  io::di, io::uo) is det.

% This is a deterministic procedure that acts as a wrapper for any
% mercury predicates to run inside the run_proc loop. This predicate
% requires access to the struct FCGX_Request pointer.
:- pred proc(pred(c_pointer, io, io)::in(pred(in, di, uo) is det),
  c_pointer::in, io::di, io::uo) is det.

:- implementation.

%--------------------------------------------------------------------------%

:- pragma foreign_decl("C",
  "
   #include ""fcgiapp.h""
   #include <pthread.h>		      
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_finish_r(ReqDataPtr::in, _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Finish_r((FCGX_Request *)ReqDataPtr);
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_init(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Init() == 0 ? MR_YES : MR_NO;
  ").		

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_accept_r(Success::out, ReqDataPtr::in,
  _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Accept_r((FCGX_Request *)ReqDataPtr) >= 0 ? MR_YES : MR_NO;
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_init_request(Success::out, Request::out,
  Sock::in, Flags::in, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Request *r = MR_GC_malloc_uncollectable(sizeof(FCGX_Request));

   Success = FCGX_InitRequest(r, Sock, Flags) == 0 ? MR_YES : MR_NO;
   Request = (MR_Word)r;
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_puts_t(Str::in, Request::in, Success::out,
  _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_PutStr(Str, strlen(Str),
       ((FCGX_Request *)Request)->out) >= 0 ? MR_YES : MR_NO;
  ").

%--------------------------------------------------------------------------%

fcgx_get_param_t(Name, Request, MaybeParam, !IO) :-
    get_param_t(Name, Request, String, Result, !IO),
    (
        Result = yes,
        MaybeParam = yes(String)
    ;
        Result = no,
        MaybeParam = no
    ).

:- pred get_param_t(string::in, c_pointer::in, string::uo, bool::uo,
  io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_param_t(Name::in, Request::in, String::uo, Result::uo, _IO2::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    char *temp;

    temp = FCGX_GetParam(Name, ((FCGX_Request *)Request)->envp);
    if (temp != NULL) {
        Result = MR_YES;
        /*
         * Duplicate the string so we can use it after calling fcgi_finish()
         */
        MR_make_aligned_string_copy(String, temp);
    } else {
        Result = MR_NO;
        String = NULL;
    }
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", spawn_threads(Count::in,
  Subroutine::in(pred(in, di, uo) is det), _IO0::di, _IO::uo),
  [promise_pure, may_call_mercury, tabled_for_io],
  "
   int i;
   pthread_t id[Count];

   for(i = 1; i <= Count; ++i)
       pthread_create(&id[i], NULL,
           (void *)init_and_accept, (void *)Subroutine);
		       
   init_and_accept(Subroutine);
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_export("C",
  init_and_accept(in(pred(in, di, uo) is det), di, uo), "init_and_accept").

init_and_accept(Subroutine, !IO) :-
  fcgx_init_request(Success, Request, 0, 0, !IO),
  (
     Success = yes ->
       run_proc(Request, Subroutine, !IO)
     ;
       true
  ).
    
%--------------------------------------------------------------------------%

:- pred run_proc(c_pointer, pred(c_pointer, io, io), io, io) is det.
:- mode run_proc(in, in(pred(in, di, uo) is det), di, uo) is det.
		
:- pragma foreign_proc("C", run_proc(Request::in,
  Subroutine::in(pred(in, di, uo) is det), _IO0::di, _IO::uo),
  [promise_pure, may_call_mercury, tabled_for_io],
  "
   FCGX_Request *request = (FCGX_Request *)Request;

   while(FCGX_Accept_r(request) >= 0) {

       proc(Subroutine, Request);

       FCGX_Finish_r(request);
   }   		      
  ").

%--------------------------------------------------------------------------%

:- pragma foreign_export("C", proc(in(pred(in,di,uo) is det),
  in, di, uo), "proc").

proc(Procedure, Request, !IO) :-
  Procedure(Request, !IO).
