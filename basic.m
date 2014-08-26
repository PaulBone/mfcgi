% Mercury-fcgi library binding
% Contains basic interface
%
% This binding is a simple wrapping of the C interface.
%
% Authors: Ebrahim Azarisooreh, Paul Bone

:- module mfcgi.basic.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.
:- import_module string.

%--------------------------------------------------------------------------%
% Basic interface.
%--------------------------------------------------------------------------%

:- type is_fcgi
    --->    is_fcgi
    ;       is_cgi.

    % Tests whether application is CGI or FCGI
    %
:- pred fcgx_is_cgi(is_fcgi::out, io::di, io::uo) is det.

    % Accept a request from the client
    %
    % If "no" is returned the caller should exit.
    %
:- pred fcgx_accept(bool::out, io::di, io::uo) is det.

    % Finish the current request.
    %
:- pred fcgx_finish(io::di, io::uo) is det.

%--------------------------------------------------------------------------%

:- type fcgi_stream.

    % Input output and error streams are associated with each connection.
    % The stream returned by any of these calls is invalid after a call to
    % fcgx_accept or fcgx_finish.
    %
:- pred fcgx_get_input_stream(fcgi_stream::out, io::di, io::uo) is det.
:- pred fcgx_get_output_stream(fcgi_stream::out, io::di, io::uo) is det.
:- pred fcgx_get_error_stream(fcgi_stream::out, io::di, io::uo) is det.

    % Set exit status
    %
    % Set the exit status for the current request.  The underlying API
    % associates exit status with streams.
    %
:- pred fcgx_set_exit_status(fcgi_stream::in, int::in, io::di, io::uo) is det.

    % Try to get a parameter from the environment array.
    %
:- pred fcgx_get_param(string::in, maybe(string)::uo, io::di, io::uo) is det.

    % A read can either be successful, partial (EOF occured after reading at
    % least one byte) or unsuccessful (EOF).
    %
:- type fcgi_read_result(T)
    --->    ok(T)
    ;       partial(T)
    ;       eof.

:- inst fcgi_read_result_no_partial
    --->    ok(ground)
    ;       eof.

    % Attempt to read a byte from the input stream and returns it.  %
    % NOTE that this wraps the FCGX_GetChar function of the FCGI API.
    % However its comment describes a byte, so in this API we name the
    % predicate using "Byte' and hope that this reduces confusion.  We hope
    % that the FCGI API code comment is correct and the function name is
    % incorrect.
    %
:- pred fcgx_get_byte(fcgi_stream::in,
    fcgi_read_result(int)::out(fcgi_read_result_no_partial),
    io::di, io::uo) is det.

:- type fcgi_unget_result
    --->    ok
    ;       error.

    % Attempt to putback a byte from the input stream.
    %
    % As above, this predicate wraps FCGX_UnGetChar.
    %
:- pred fcgx_unget_byte(fcgi_stream::in, int::in, fcgi_unget_result::out,
    io::di, io::uo) is det.

% We do not implement FCGX_GetStr at the moment as there is no convenient
% and fast translation into Mercury.  Use repeated calls to fcgx_get_byte
% instead.

% XXX: FCGX_GetLine.  We have to check behaviour regarding null bytes before
% implementing this.

    % Write a string to the output buffer
    %
:- pred fcgx_puts(string::in, fcgi_stream::in, bool::out, io::di, io::uo) is det.

%
% Note that the filter related functions are not implemented.
%

:- implementation.

:- pragma foreign_decl("C",
"
   #include ""fcgiapp.h""
").

%--------------------------------------------------------------------------%

fcgx_is_cgi(IsCGI, !IO) :-
    is_cgi(IsCGI0, !IO),
    (
        IsCGI0 = yes,
        IsCGI = is_cgi
    ;
        IsCGI0 = no,
        IsCGI = is_fcgi
    ).

:- pred is_cgi(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", is_cgi(Result::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
"
   Result = FCGX_IsCGI() ? MR_YES : MR_NO;
").		

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_accept(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int result;

    result = FCGX_Accept(&in, &out, &err, &envp);
    
    Success = result == 0 ? MR_YES : MR_NO;
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_finish(_IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
"
   FCGX_Finish();
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    fcgx_set_exit_status(Stream::in, Status::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    FCGX_SetExitStatus(Status, Stream);
").

%--------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    static FCGX_Stream *in, *out, *err;
    static FCGX_ParamArray envp;
").

:- pragma foreign_type("C", fcgi_stream, "FCGX_Stream*",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    fcgx_get_input_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
     will_not_throw_exception],
    "Stream = in;").
:- pragma foreign_proc("C",
    fcgx_get_output_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
     will_not_throw_exception],
    "Stream = out;").
:- pragma foreign_proc("C",
    fcgx_get_error_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
     will_not_throw_exception],
    "Stream = err;").

%--------------------------------------------------------------------------%

fcgx_get_param(Name, MaybeParam, !IO) :-
    get_param(Name, String, Result, !IO),
    (
        Result = yes,
        MaybeParam = yes(String)
    ;
        Result = no,
        MaybeParam = no
    ).

:- pred get_param(string::in, string::uo, bool::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_param(Name::in, String::uo, Result::uo, _IO2::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    char *temp;

    temp = FCGX_GetParam(Name, envp);
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

fcgx_get_byte(Stream, Result, !IO) :-
    get_byte(Stream, Byte, Result0, !IO),
    (
        Result0 = yes,
        Result = ok(Byte)
    ;
        Result0 = no,
        Result = eof
    ).

:- pred get_byte(fcgi_stream::in, int::out, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_byte(Stream::in, Byte::out, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Byte = FCGX_GetChar(Stream);
    if (Byte == EOF) {
        Result = MR_NO;
    } else {
        Result = MR_YES;
    }
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    fcgx_unget_byte(Stream::in, Byte::in, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    int result = FCGX_UnGetChar(Byte, Stream);
    if (result == EOF) {
        Result = MR_NO;
    } else {
        Result = MR_YES;
    }
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_puts(Str::in, Stream::in, Success::out,
  _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
  "
   Success = FCGX_PutStr(Str, strlen(Str), Stream) >= 0 ? MR_YES : MR_NO;
  ").
