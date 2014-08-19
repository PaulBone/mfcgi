% Mercury-fcgi library binding
% contains single thread and multi-thread parts of api
%
% This binding is a simple wrapping of the C interface.
%
% Authors: Ebrahim Azarisooreh, Paul Bone

:- module mfcgi_multi.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.
:- import_module string.

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

:- implementation.

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_finish_r(ReqDataPtr::in, _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Finish_r((FCGX_Request *)ReqDataPtr);
  ").

:- pragma foreign_proc("C", fcgx_init(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Init() == 0 ? MR_YES : MR_NO;
  ").		

:- pragma foreign_proc("C", fcgx_accept_r(Success::out, ReqDataPtr::in,
  _IO0::di, _IO::uo),
  [thread_safe, promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_Accept_r((FCGX_Request *)ReqDataPtr) >= 0 ? MR_YES : MR_NO;
  ").

:- pragma foreign_proc("C", fcgx_init_request(Success::out, Request::out,
  Sock::in, Flags::in, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   FCGX_Request *r;
   Success = FCGX_InitRequest(r, Sock, Flags) == 0 ? MR_YES : MR_NO;
   Request = (MR_Word)r;
  ").
