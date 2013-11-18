unit fre_feed_client;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2013, FirmOS Business Solutions GmbH
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
      * Neither the name of the <FirmOS Business Solutions GmbH> nor the names
        of its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(§LIC_END)
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fre_base_client,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,fre_dbtest,fre_system;

type


  { TFRE_SAMPLE_FEED_CLIENT }

  TFRE_SAMPLE_FEED_CLIENT=class(TFRE_BASE_CLIENT)
  private
    FEED_Timer            : IFRE_APSC_TIMER;
    FFeeding              : Boolean;
    FFeedAppClass         : TFRE_DB_String;
    FFeedAppUid           : TGUid;
  public
    procedure  MySessionEstablished    (const chanman : IFRE_APSC_CHANNEL_MANAGER); override;
    procedure  MySessionDisconnected   (const chanman : IFRE_APSC_CHANNEL_MANAGER); override;
    procedure  QueryUserPass           (out user, pass: string); override;
    procedure  RegisterRemoteMethods   (var remote_method_array : TFRE_DB_RemoteReqSpecArray); override;
    procedure  MyInitialize            ; override;
    procedure  MyFinalize              ; override;
    procedure  GenerateFeedDataTimer   (const TIM : IFRE_APSC_TIMER ; const flag1,flag2 : boolean); // Timout & CMD Arrived & Answer Arrived
    procedure  MyConnectionTimer       ; override;

    procedure  WorkRemoteMethods       (const rclassname,rmethodname : TFRE_DB_NameType ; const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE); override;
    function   ListDirLevel            (const basepath : string):IFRE_DB_Object;
    function   GetFileDirInfo          (const fileid : string):IFRE_DB_Object;
  published
    procedure  REM_TestMethod          (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure  REM_TestTimeout         (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure  REM_TestError           (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
  end;


implementation

procedure TFRE_SAMPLE_FEED_CLIENT.MySessionEstablished(const chanman: IFRE_APSC_CHANNEL_MANAGER);
begin
  inherited; // Create and Activate Session Channel Timer
  if Get_AppClassAndUid('TFRE_DB_TEST_APP',FFeedAppClass,FFeedAppUid) then begin
    FFeeding := True;
  end;
  FEED_Timer := chanman.AddTimer(2000); // Beside the "normal 1 sec" Timer a 5 sec timer in the channel context
  FEED_Timer.TIM_SetID('FEED_'+inttostr(chanman.GetID));
  writeln('Generated Feedtimer ',FEED_Timer.TIM_GetID);
  FEED_Timer.TIM_SetCallback(@GenerateFeedDataTimer);
  FEED_Timer.TIM_Start;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MySessionDisconnected(const chanman: IFRE_APSC_CHANNEL_MANAGER);
begin
  writeln('FINALIZING FEED FROM CM_'+inttostr(chanman.GetID));
  FEED_Timer.Finalize;
  FFeeding   := false;
  inherited;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.QueryUserPass(out user, pass: string);
begin
  user := cFRE_Feed_User;
  pass := cFRE_Feed_Pass;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.RegisterRemoteMethods(var remote_method_array: TFRE_DB_RemoteReqSpecArray);
var base_idx : NativeInt;
begin
  inherited;

  // THIS IS A SAMPLE FOR NON-STANDARD REMOTE REGISTRATIONS (non REM_* methods)
  // add virtual methods
  base_idx := Length(remote_method_array);
  SetLength(remote_method_array,base_idx+2);
  with remote_method_array[base_idx] do
    begin
      classname       := 'SAMPLEFEEDER';
      methodname      := 'BROWSEPATH';
      invokationright := ''; //unsafe
    end;
  with remote_method_array[base_idx+1] do
    begin
      classname       := 'SAMPLEFEEDER';
      methodname      := 'GETFILEDIRINFO';
      invokationright := ''; //unsafe
    end;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MyInitialize;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_FILEDIR);
  GFRE_DBI.Initialize_Extension_Objects;
  FREDB_LoadMimetypes('');
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MyFinalize;
begin

end;

procedure TFRE_SAMPLE_FEED_CLIENT.GenerateFeedDataTimer(const TIM: IFRE_APSC_TIMER; const flag1, flag2: boolean);
var vmo : IFRE_DB_Object;
begin
  writeln('SEND SUPPRESSED');
  exit;
  if FFeeding then
    begin
      try
        vmo := GFRE_DBI.NewObject;
        vmo.Field('LIVE STATUS FEED').AsString := 'LSF_0.0.1';
        vmo.Field('TIMESTAMP').AsDateTimeUTC   := GFRE_DT.Now_UTC;
        vmo.Field('SAMPLE_VALUE').AsInt32      := Random(1000)-500;
        SendServerCommand(FFeedAppClass,'RAW_DATA_FEED',TFRE_DB_GUIDArray.Create(FFeedAppUid),vmo);
        writeln('LIVE UPDATE SENT! ' , GFRE_DT.Now_UTC);
      except on e:exception do begin
        writeln('FEED EXCEPTION : ',e.Message);
      end;end;
    end;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MyConnectionTimer;
begin
  writeln('-> CONNECTION TIMER ACTIVE');
end;

procedure TFRE_SAMPLE_FEED_CLIENT.WorkRemoteMethods(const rclassname, rmethodname: TFRE_DB_NameType; const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
var reply_data : IFRE_DB_Object;
begin
  // THIS IS A SAMPLE FOR NON-STANDARD REMOTE REGISTRATIONS (non REM_* methods)
  GFRE_LOG.Log('>TRY INVOKE '+rclassname+'.'+rmethodname,' CID='+inttostr(command_id));
  if (rclassname='SAMPLEFEEDER') and (rmethodname='BROWSEPATH') then
    begin
      reply_data := ListDirLevel(input.Field('level').AsString);
      input.Finalize;
      GFRE_LOG.Log('REPLY ON REQUEST SAMPLEFEEDER.BROWSEPATH ','FEEDER');
      AnswerSyncCommand(command_id,reply_data);
    end
  else
  if (rclassname='SAMPLEFEEDER') and (rmethodname='GETFILEDIRINFO') then
    begin
      reply_data := GetFileDirInfo(input.Field('fileid').AsString);
      input.Finalize;
      GFRE_LOG.Log('REPLY ON REQUEST SAMPLEFEEDER.GETFILEDIRINFO ','FEEDER');
      AnswerSyncCommand(command_id,reply_data);
    end
  else
    inherited;
  GFRE_LOG.Log('<TRY INVOKE '+rclassname+'.'+rmethodname,' CID='+inttostr(command_id));
end;

function TFRE_SAMPLE_FEED_CLIENT.ListDirLevel(const basepath: string): IFRE_DB_Object;
var Info  : TSearchRec;
    entry : TFRE_DB_TEST_FILEDIR;
    count : NativeInt;
begin
  result := GFRE_DBI.NewObject;
  count  := 0;
  If FindFirst (basepath+'*',faAnyFile and faDirectory,Info)=0 then
    Repeat
      With Info do
        begin
          if (name='.') or (name='..') then
            Continue;
          entry := TFRE_DB_TEST_FILEDIR.CreateForDB;
          entry.SetProperties(name,(Attr and faDirectory) <> faDirectory,Size,mode,Time);
          result.Field(inttostr(count)).AsObject := entry;
          inc(count);
        end;
    Until FindNext(info)<>0;
  FindClose(Info);
end;

function TFRE_SAMPLE_FEED_CLIENT.GetFileDirInfo(const fileid: string): IFRE_DB_Object;
var
  Info  : TSearchRec;
  entry : TFRE_DB_TEST_FILEDIR;
begin
  result := GFRE_DBI.NewObject;
  If FindFirst(fileid,faAnyFile and faDirectory,Info)=0 then
    With Info do
      begin
        entry := TFRE_DB_TEST_FILEDIR.CreateForDB;
        entry.SetProperties(name,(Attr and faDirectory) <> faDirectory,Size,mode,Time);
        result.Field('info').AsObject := entry;
      end;
  FindClose(Info);
end;

procedure TFRE_SAMPLE_FEED_CLIENT.REM_TestMethod(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
var reply_data : IFRE_DB_Object;
begin
  reply_data := GFRE_DBI.NewObject;
  reply_data.Field('Hello').AsString := 'World!';
  reply_data.Field('SampleField').AsInt16 := 12345;
  AnswerSyncCommand(command_id,reply_data);
  input.Finalize;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.REM_TestTimeout(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  writeln('>> A METHOD THAT TIMES OUT WITHOUT SENDING ANSWER');
  input.Finalize;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.REM_TestError(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  writeln('CALLED TestError Method, excepting ...');
  raise EFRE_DB_Exception.Create(edb_ERROR,'ARBITRARY TEST FAILURE');
end;

initialization

end.
