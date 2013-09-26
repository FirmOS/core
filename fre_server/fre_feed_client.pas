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
    FEED_Timer            : IFRE_APS_TIMER;
    FFeeding              : Boolean;
    FFeedAppClass         : TFRE_DB_String;
    FFeedAppUid           : TGUid;
  public
    procedure  MySessionEstablished    ; override;
    procedure  MySessionDisconnected   ; override;
    procedure  QueryUserPass           (out user, pass: string); override;
    procedure  RegisterRemoteMethods   (var remote_method_array : TFRE_DB_RemoteReqSpecArray); override;
    procedure  MyInitialize            ; override;
    procedure  MyFinalize              ; override;
    procedure  GenerateFeedDataTimer   (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
    procedure  WorkRemoteMethods       (const rclassname,rmethodname : TFRE_DB_NameType ; const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE); override;
    function   ListDirLevel            (const basepath : string):IFRE_DB_Object;
    function   GetFileDirInfo          (const fileid : string):IFRE_DB_Object;
  end;


implementation

procedure TFRE_SAMPLE_FEED_CLIENT.MySessionEstablished;
begin
  if Get_AppClassAndUid('testapp',FFeedAppClass,FFeedAppUid) then begin
    FFeeding := True;
  end;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MySessionDisconnected;
begin
  FFeeding   := false;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.QueryUserPass(out user, pass: string);
begin
  user := cFRE_Feed_User;
  pass := cFRE_Feed_Pass;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.RegisterRemoteMethods(var remote_method_array: TFRE_DB_RemoteReqSpecArray);
begin
  SetLength(remote_method_array,2);
  with remote_method_array[0] do
    begin
      classname       := 'SAMPLEFEEDER';
      methodname      := 'BROWSEPATH';
      invokationright := ''; //unsafe
    end;
  with remote_method_array[1] do
    begin
      classname       := 'SAMPLEFEEDER';
      methodname      := 'GETFILEDIRINFO';
      invokationright := ''; //unsafe
    end;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MyInitialize;
begin
  FEED_Timer      := GFRE_S.AddPeriodicTimer (5000,@GenerateFeedDataTimer);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_FILEDIR);
  GFRE_DBI.Initialize_Extension_Objects;
end;
procedure TFRE_SAMPLE_FEED_CLIENT.MyFinalize;
begin
  FEED_Timer.FinalizeIt;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.GenerateFeedDataTimer(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var vmo : IFRE_DB_Object;
begin
  if FFeeding then
    begin
      try
        vmo := GFRE_DBI.NewObject;
        vmo.Field('LIVE STATUS FEED').AsString := 'LSF_0.0.1';
        vmo.Field('TIMESTAMP').AsDateTimeUTC   := GFRE_DT.Now_UTC;
        vmo.Field('SAMPLE_VALUE').AsInt32      := Random(1000)-500;
        writeln('SEND SUPPRESSED (COMMENTED)');
        //SendServerCommand(FFeedAppClass,'RAW_DATA_FEED',TFRE_DB_GUIDArray.Create(FFeedAppUid),vmo);
        writeln('LIVE UPDATE SENT! ' , GFRE_DT.Now_UTC);
      except on e:exception do begin
        writeln('FEED EXCEPTION : ',e.Message);
      end;end;
    end;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.WorkRemoteMethods(const rclassname, rmethodname: TFRE_DB_NameType; const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
var reply_data : IFRE_DB_Object;
begin
  if (rclassname='SAMPLEFEEDER') and (rmethodname='BROWSEPATH') then
    begin
      reply_data := ListDirLevel(input.Field('level').AsString);
      input.Finalize;
      writeln('REPLY ON REQUEST SAMPLEFEEDER.BROWSEPATH ',reply_data.DumpToString());
      AnswerSyncCommand(command_id,reply_data);
    end;
  if (rclassname='SAMPLEFEEDER') and (rmethodname='GETFILEDIRINFO') then
    begin
      reply_data := GetFileDirInfo(input.Field('fileid').AsString);
      input.Finalize;
      writeln('REPLY ON REQUEST SAMPLEFEEDER.GETFILEDIRINFO ',reply_data.DumpToString());
      AnswerSyncCommand(command_id,reply_data);
    end;
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

initialization

end.
