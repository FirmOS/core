program con_ssl_test;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, FRE_FCOM_SSL,FOS_FCOM_INTERFACES,FOS_FCOM_TYPES,FOS_FCOM_HANDLES, FRE_SYSTEM,FOS_FCOM_DEFAULT,sysutils;

var G_SSL_CTX:PSSL_CTX;

  procedure SetupSSL_Ctx;
  var fre_ssl_i     : TFRE_SSL_INFO;
  begin
    fre_ssl_i.ssl_type := fssl_TLSv1;
    fre_ssl_i.cerifificate_file := '/fre/ssl/server_files/server_cert.pem';
    fre_ssl_i.private_key_file  := '/fre/ssl/server_files/server_key.pem';
    fre_ssl_i.root_ca_file      := '/fre/ssl/server_files/ca_cert.pem';
    fre_ssl_i.fail_no_peer_cert := false;
    fre_ssl_i.verify_peer       := false;
    fre_ssl_i.verify_peer_cert_once:= false;
    fre_ssl_i.IsServer          := true;
    frE_ssl_i.cipher_suites     := 'DEFAULT';
    G_SSL_CTX := FRE_Setup_SSL_Context(@fre_ssl_i);
  end;

  procedure Server;
  var sock              : IFCOM_SOCK;
      res               : EFOS_OS_ERROR;
      ai                : IFCOM_AI;
      new_sock          : IFCOM_SOCK;
      done              : boolean;
      want              : TFRE_FCOM_SSL_WANTS;
      amount,new_amount : integer;
      buffer            : string;
      continue          : integer;

      function MyGetPW:string;
      begin
        result := '0000';
      end;

      procedure Handle_SSL_RW;
      var want        : TFRE_FCOM_SSL_WANTS;
          amount      : integer;
          amount_done : integer;
          s           : string;
          done        : boolean;
      begin
//        repeat
          new_sock.SSL_Wants(want,amount);
          done:=false;
          case want of
            fsw_BAD: abort;
            fsw_NOTHING: done:=true;
            fsw_READING: begin
                            sleep(10);
                            new_sock.Datacount(amount);
                            writeln('WANT READ, AMOUNT=',amount);
                            if amount>0 then begin
                              SetLength(buffer,amount);
                              fre_fcom_error_check('recv',new_sock.Receive(Pchar(buffer),length(buffer),amount));
                              setlength(buffer,amount);
                              new_sock.SSL_WriteToSSL(Pchar(buffer),amount,new_amount);
                              writeln('RECVD - ',new_amount);
                            end
                            //else
                            //  break;
                         end;
            fsw_WRITING: begin
                            writeln('WRITING - ',amount);
                            setlength(s,amount);
                            new_sock.SSL_ReadFromSSL(pchar(s),length(s),amount);
                            new_sock.Send(pchar(s),amount,amount_done);
                         end;
            fsw_x509: abort;
          end;
          //if not done then begin
          //  writeln('--AGAIN--');
          //end;
        //until done;
      end;

  begin
    SetupSSL_Ctx;
    sock := GFRE_FF.New_FCOM_NETSOCK(fil_IPV4,fsp_TCP,res);
    fre_fcom_error_check('get new socket',res);
    ai   := GFRE_FF.New_FCOM_AI;
    ai.ResolveandSet('0.0.0.0',44443);
    fre_fcom_error_check('reuse',sock.SetListenerReuse(true));
    fre_fcom_error_check('bind',sock.Bind(ai));
    fre_fcom_error_check('listen',sock.Listen(10));
    repeat
      writeln('ACCEPT');
      fre_fcom_error_check('accept',sock.Accept(new_sock));
      writeln('ACCEPTED ',new_sock.GetHandleKey);
      done := false;
      repeat
        write('SSL ACCEPT ');
        Handle_SSL_RW;
        res:=new_sock.SSL_Accept;
        writeln(CFOS_OS_ERROR[res]);
        case res of
          EFOS_OS_OK          : begin
                                  continue:=1;
                                  break;
                                end;
          EFOS_OS_WOULD_BLOCK : begin
                                  Handle_SSL_RW;
                                end;
          EFOS_OS_SSL_ERROR : begin
                                writeln('SSL ACCEPT ERROR: ',new_sock.Get_SSL_ErrorString);
                                new_sock.SockClose;
                                continue := 0;
                                break;
                              end;
          end;
      until done;
      if continue=1 then begin
        writeln('ACCEPTED OK');
        repeat
          Handle_SSL_RW;
          //writeln('SSL PENDING ',new_sock.SSL_Pending);
          new_sock.Datacount(amount);
          if amount>0 then break;
          sleep(10);
        until false;
        SetLength(buffer,amount);
        fre_fcom_error_check('recv',new_sock.Receive(Pchar(buffer),length(buffer),amount));
        setlength(buffer,amount);
        new_sock.SSL_WriteToSSL(Pchar(buffer),amount,new_amount);
        writeln('RECVD - ',new_amount);
        writeln('SSL PENDING ',new_sock.SSL_Pending);
      end;
    until false;
  end;

  procedure Client;
  begin

  end;

var x:integer;
begin
  Setup_FRE_SSL;
  case ParamStr(1) of
    'server' : Server;
    'client' : Client;
    else writeln('user server or client');
  end;
end.

