program fpmake_packages;

{$Mode objfpc}
{$H+}
{$inline on}

 uses fpmkunit,classes,sysutils,fos_buildtools;

 function EnableOldRTLStructure: boolean;
 var env: string;
 begin
   env := {$I %FOSOLDFPCRTL%};
   writeln('FOSOLDFPCRTL:',env);
   result := env<>'';
 end;

 Var
   P   : TPackage;

 begin
   with Installer(TFOSInstaller) do begin
     P := AddPackage('FRE_INTF');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       Directory:=cFOS_BUILD_PREFIX+'fos_interfaces';
       Dependencies.Add('fcl-json');
       Dependencies.Add('fcl-xml');
       if not EnableOldRTLStructure then 
         begin
           Dependencies.Add('rtl-extra');
         end;
       with targets do begin
         addunit('jsonparser.pp');
         addunit('fpjson.pp');
         addunit('fpjsonrtti.pp');
         AddUnit('fos_tool_interfaces.pas');
         AddUnit('fos_fcom_interfaces.pas');
         addunit('fos_redblacktree_gen.pas');
         addunit('fos_fcom_types.pas');
         Addunit('fre_system.pas');
         addunit('fos_locking.pas');
         addunit('fos_interlocked.pas');
         AddUnit('fre_db_common.pas');
         addunit('fre_db_interface.pas');
         AddUnit('fre_aps_interface.pas');
         AddUnit('fre_openssl_interface.pas');
       end;
     end;
     P := AddPackage('FRE_CORE');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       Directory:=cFOS_BUILD_PREFIX+'fre_core';
       Dependencies.Add('FRE_INTF');
       Dependencies.Add('paszlib');
       Dependencies.Add('fcl-base');
       Dependencies.Add('fcl-db');
       Dependencies.Add('syslog');
       with targets do begin
         addunit('fos_arraygen.pas');
         addunit('fos_arraysetgen.pas');
         addunit('fos_basis_tools.pas');
         case Defaults.OS of
           freebsd,
           netbsd,
           openbsd,
           macos,
           darwin: // enter add unit here
           ;
         end;
         addunit('fos_cpu_tools.pas');
         addunit('fos_default_implementation.pas');
         addunit('fos_default_strings.pas');
         addunit('fos_file_logger.pas');
         addunit('fos_generic_sort.pas');
         addunit('fos_genq.pas');
         addunit('fos_lfq.pas');
         addunit('fos_listarray.pas');
         addunit('fos_nps.pas');
         addunit('fos_partvarq.pas');
         addunit('fos_tool_factory.pas');
         addunit('fre_date_tools.pas');
         addunit('fos_alignedarray.pas');
         addunit('upascaltz.pas');
         addunit('upascaltz_types.pas');
         addunit('usorters.pas');
         addunit('cres_fos_timezones.pp');
         addunit('fre_binary_buffer.pas');
         addunit('fos_sparelistgen.pas');
         addunit('fos_art_tree.pas');
         addunit('fre_mysql_ll.pas');
         addunit('fos_strutils.pas');


         //TODO: CORE PACKAGE KILL
         //p.targets.addunit('fos_jobscheduler.pas');
         //p.targets.addunit('fos_ng_appsyncer.pas');
         //p.targets.addunit('fos_ng_appsyncer_console.pas');
         //p.targets.addunit('fos_ng_worker.pas');
         //p.targets.addunit('fos_null_logger.pas');
       end;
    end;
    P := AddPackage('FRE_SYNAPSE');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      Directory:=cFOS_BUILD_PREFIX+'fre_external/synapse/';
      Dependencies.Add('fcl-base');
      Dependencies.Add('fcl-net');
      with targets do begin
        AddUnit('asn1util.pas');
        AddUnit('blcksock.pas');
        AddUnit('clamsend.pas');
        AddUnit('dnssend.pas');
        AddUnit('ftpsend.pas');
        AddUnit('ftptsend.pas');
        AddUnit('httpsend.pas');
        AddUnit('imapsend.pas');
        AddUnit('ldapsend.pas');
        AddUnit('mimeinln.pas');
        AddUnit('mimemess.pas');
        AddUnit('mimepart.pas');
        AddUnit('nntpsend.pas');
        AddUnit('pingsend.pas');
        AddUnit('pop3send.pas');
        AddUnit('slogsend.pas');
        AddUnit('smtpsend.pas');
        AddUnit('snmpsend.pas');
        AddUnit('sntpsend.pas');
        AddUnit('synachar.pas');
        AddUnit('synacode.pas');
        AddUnit('synacrypt.pas');
        AddUnit('synadbg.pas');
        AddUnit('synafpc.pas');
        AddUnit('synaicnv.pas');
        AddUnit('synaip.pas');
        AddUnit('synamisc.pas');
        AddUnit('synautil.pas');
        AddUnit('synsock.pas');
        AddUnit('tlntsend.pas');
        AddUnit('ssl_openssl.pas');
        AddUnit('ssl_openssl_lib.pas');
      end
    end;
    P := AddPackage('FRE_FCOM');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      Directory:=cFOS_BUILD_PREFIX+'fre_fcom';
      Dependencies.Add('FRE_INTF');
      Dependencies.Add('FRE_CORE');
      Dependencies.Add('pthreads');

      with targets do begin
        AddUnit('fre_fcom_ssl.pas');
        AddUnit('fre_sys_base_cs.pas');
        AddUnit('../fre_server/fre_http_tools.pas');
        AddUnit('fre_http_client.pas');
        //TODO: FCOM PACKAGE CHECK
        //AddUnit('fos_fcom_bsdnet.pas');
        //AddUnit('fos_fcom_engine.pas');
        //AddUnit('fos_fcom_pcap.pas');
        //AddUnit('fos_gc_replicator.pas');
        //AddUnit('fos_http_proxy.pas');
        //AddUnit('fre_base_cs.pas');
        //AddUnit('fos_gc_messages.pas');
        //AddUnit('fos_gc_types.pas');
        //AddUnit('fos_session_engine.pas');
        //AddUnit('fre_fcom_cmds.pas');
      end;
    end;
    P := AddPackage('FRE_APS');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      Directory:=cFOS_BUILD_PREFIX+'fre_aps';
      Dependencies.Add('FRE_INTF');
      Dependencies.Add('FRE_CORE');
      Dependencies.Add('FRE_FCOM');
      with targets do begin
        //AddUnit('fos_partitiontree.pas');
        AddUnit('fre_aps_comm_impl.pas');
        AddUnit('fre_libevent_core.pas');
      end;
    end;

    P := AddPackage('FRE_DB');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      Directory:=cFOS_BUILD_PREFIX+'fre_db';
      Dependencies.Add('FRE_INTF');
      Dependencies.Add('FRE_CORE');
      Dependencies.Add('FRE_FCOM');
      Dependencies.Add('FRE_APS');
      Dependencies.Add('fcl-process');
      with Targets do begin
        AddUnit('fre_db_persistance_fs_simple.pas');
        AddUnit('fre_db_persistance_common.pas');
        AddUnit('fre_db_core.pas');
        AddUnit('fre_db_graph.pas');
        AddUnit('fre_db_embedded_impl.pas');
        AddUnit('fre_db_web_styling.pas');
        AddUnit('fre_db_login.pas');
        AddUnit('fre_db_tasker.pas');
        AddUnit('fre_wapp_dojo.pas');
        AddUnit('fre_json_action.pas');
        AddUnit('../fre_apps/fre_dbbase.pas');
        AddUnit('../fre_apps/fre_dbbusiness.pas');
        AddUnit('fre_configuration.pas');
        AddUnit('../fre_core/fre_process.pas');
        addunit('fre_db_core_transdata.pas');
     end;
    end;
    P := AddPackage('FRE_BLKCOM');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      Directory:=cFOS_BUILD_PREFIX+'fre_blockcom/';
      Dependencies.Add('fcl-base');
      Dependencies.Add('fcl-net');
      Dependencies.Add('fcl-net');
      Dependencies.Add('FRE_SYNAPSE');
      Dependencies.Add('FRE_CORE');
      with targets do begin
        AddUnit('fre_mail.pas');
        AddUnit('fre_http.pas');
      end
    end;
    P := AddPackage('FRE_APPS');
    with p do begin
       OSes      := cFOS_BUILD_OSes;
       Directory := cFOS_BUILD_PREFIX+'fre_apps';
       Dependencies.Add('FRE_CORE');
       Dependencies.Add('FRE_DB');
       Dependencies.Add('FRE_INTF');
       Dependencies.Add('FRE_BLKCOM');
       Dependencies.Add('fcl-process');
       with Targets do begin
         AddUnit('../fre_db/fre_dbtest.pas');
         AddUnit('../fre_server/fre_base_server.pas');
         AddUnit('../fre_server/fre_base_client.pas');
         AddUnit('../fre_server/fre_webssocket_baseserver.pas');
         AddUnit('../fre_server/fre_http_srvhandler.pas');
//         AddUnit('../fre_server/fre_http_tools.pas');
         AddUnit('../fre_server/fre_feed_client.pas');
         AddUnit('../fre_server/fre_basecli_app.pas');
         AddUnit('../fre_server/fre_basefeed_app.pas');
         AddUnit('../fre_server/fre_basedbo_server.pas');
         AddUnit('../fre_server/fre_basesubfeed_app.pas');
         AddUnit('../fre_server/fre_openssl_cmd.pas');
         AddUnit('fre_accesscontrol_common.pas');
         AddUnit('fre_translationapp.pas');
         AddUnit('../fre_server/fre_net_pl_client.pas');

       end;
    end;
    Run;
   end;
 end.
