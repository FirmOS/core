unit fre_db_web_styling;

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
  Classes, SysUtils,FRE_DB_INTERFACE,FRE_DB_COMMON;

function STYLE_Get_SVG_Definitions: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY;


implementation

function STYLE_Get_SVG_Definitions: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY;
var
  elem,subelem,subsubelem: TFRE_DB_SVG_DEF_ELEM_DESC;
begin
  SetLength(Result,9);

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('linearGradient');
  elem.AddAttribute.Describe('id','connectorgradient');
  elem.AddAttribute.Describe('x1','0%');
  elem.AddAttribute.Describe('y1','0%');
  elem.AddAttribute.Describe('x2','0%');
  elem.AddAttribute.Describe('y2','100%');
  elem.AddAttribute.Describe('gradientUnits','objectBoundingBox');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_connectorgradient_stop1');
  subelem.AddAttribute.Describe('offset','0');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_connectorgradient_stop2');
  subelem.AddAttribute.Describe('offset','1');

  Result[0]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('filter');
  elem.AddAttribute.Describe('id','shadowActive');

  subelem:=elem.AddElement.Describe('feGaussianBlur');
  subelem.AddAttribute.Describe('in','SourceGraphic');
  subelem.AddAttribute.Describe('stdDeviation','5');
  subelem.AddAttribute.Describe('result','blur');

  subelem:=elem.AddElement.Describe('feOffset');
  subelem.AddAttribute.Describe('dx','0');
  subelem.AddAttribute.Describe('dy','0');
  subelem.AddAttribute.Describe('result','offsetblur');

  subelem:=elem.AddElement.Describe('feFlood');
  subelem.AddAttribute.Describe('id','sitemap_shadowActive_feFlood');

  subelem:=elem.AddElement.Describe('feComposite');
  subelem.AddAttribute.Describe('in2','offsetblur');
  subelem.AddAttribute.Describe('operator','in');
  subelem.AddAttribute.Describe('result','whiteOff');


  subelem:=elem.AddElement.Describe('feComponentTransfer');
  subsubelem:=subelem.AddElement.Describe('feFuncA');
  subsubelem.AddAttribute.Describe('type','linear');
  subsubelem.AddAttribute.Describe('slope','0.5');

  subelem:=elem.AddElement.Describe('feMerge');
  subsubelem:=subelem.AddElement.Describe('feMergeNode');

  subsubelem:=subelem.AddElement.Describe('feMergeNode');
  subsubelem.AddAttribute.Describe('in','whiteOff');

  subsubelem:=subelem.AddElement.Describe('feMergeNode');
  subsubelem.AddAttribute.Describe('in','SourceGraphic');

  Result[1]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('linearGradient');
  elem.AddAttribute.Describe('id','sm_disabled1');
  elem.AddAttribute.Describe('x1','-30%');
  elem.AddAttribute.Describe('y1','30%');
  elem.AddAttribute.Describe('x2','60%');
  elem.AddAttribute.Describe('y2','130%');
  elem.AddAttribute.Describe('gradientUnits','objectBoundingBox');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop1');
  subelem.AddAttribute.Describe('offset','0');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop2');
  subelem.AddAttribute.Describe('offset','0.2216');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop3');
  subelem.AddAttribute.Describe('offset','0.3014');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop4');
  subelem.AddAttribute.Describe('offset','0.3583');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop5');
  subelem.AddAttribute.Describe('offset','0.4043');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop6');
  subelem.AddAttribute.Describe('offset','0.4437');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop7');
  subelem.AddAttribute.Describe('offset','0.4786');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop8');
  subelem.AddAttribute.Describe('offset','0.5093');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled1_stop9');
  subelem.AddAttribute.Describe('offset','0.5273');

  Result[2]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('linearGradient');
  elem.AddAttribute.Describe('id','sm_disabled2');
  elem.AddAttribute.Describe('x1','0%');
  elem.AddAttribute.Describe('y1','0%');
  elem.AddAttribute.Describe('x2','0%');
  elem.AddAttribute.Describe('y2','100%');
  elem.AddAttribute.Describe('gradientUnits','objectBoundingBox');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled2_stop1');
  subelem.AddAttribute.Describe('offset','0.289');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled2_stop2');
  subelem.AddAttribute.Describe('offset','0.5');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_disabled2_stop3');
  subelem.AddAttribute.Describe('offset','0.7606');

  Result[3]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('linearGradient');
  elem.AddAttribute.Describe('id','sm_enabled1');
  elem.AddAttribute.Describe('x1','-30%');
  elem.AddAttribute.Describe('y1','30%');
  elem.AddAttribute.Describe('x2','60%');
  elem.AddAttribute.Describe('y2','130%');
  elem.AddAttribute.Describe('gradientUnits','objectBoundingBox');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop1');
  subelem.AddAttribute.Describe('offset','0');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop2');
  subelem.AddAttribute.Describe('offset','0.2068');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop3');
  subelem.AddAttribute.Describe('offset','0.2813');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop4');
  subelem.AddAttribute.Describe('offset','0.3344');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop5');
  subelem.AddAttribute.Describe('offset','0.3774');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop6');
  subelem.AddAttribute.Describe('offset','0.4142');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop7');
  subelem.AddAttribute.Describe('offset','0.4467');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop8');
  subelem.AddAttribute.Describe('offset','0.4762');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop9');
  subelem.AddAttribute.Describe('offset','0.5023');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled1_stop10');
  subelem.AddAttribute.Describe('offset','0.5165');

  Result[4]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('linearGradient');
  elem.AddAttribute.Describe('id','sm_enabled2');
  elem.AddAttribute.Describe('x1','0%');
  elem.AddAttribute.Describe('y1','0%');
  elem.AddAttribute.Describe('x2','0%');
  elem.AddAttribute.Describe('y2','100%');
  elem.AddAttribute.Describe('gradientUnits','objectBoundingBox');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled2_stop1');
  subelem.AddAttribute.Describe('offset','0.098');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled2_stop2');
  subelem.AddAttribute.Describe('offset','0.5562');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_enabled2_stop3');
  subelem.AddAttribute.Describe('offset','1');

  Result[5]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('radialGradient');
  elem.AddAttribute.Describe('id','sc_enabled1');
  elem.AddAttribute.Describe('cx','14.7974');
  elem.AddAttribute.Describe('cy','10.7773');
  elem.AddAttribute.Describe('r','14.0126');
  elem.AddAttribute.Describe('gradientUnits','userSpaceOnUse');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sc_enabled1_stop1');
  subelem.AddAttribute.Describe('offset','0');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sc_enabled1_stop2');
  subelem.AddAttribute.Describe('offset','0.3132');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sc_enabled1_stop3');
  subelem.AddAttribute.Describe('offset','1');

  Result[6]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('radialGradient');
  elem.AddAttribute.Describe('id','sc_disabled1');
  elem.AddAttribute.Describe('cx','14.7974');
  elem.AddAttribute.Describe('cy','10.7773');
  elem.AddAttribute.Describe('r','14.0126');
  elem.AddAttribute.Describe('gradientUnits','userSpaceOnUse');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sc_disabled1_stop1');
  subelem.AddAttribute.Describe('offset','0');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sc_disabled1_stop2');
  subelem.AddAttribute.Describe('offset','1');

  Result[7]:=elem;

  elem:=TFRE_DB_SVG_DEF_ELEM_DESC.create.Describe('linearGradient');
  elem.AddAttribute.Describe('id','sm_pressed1');
  elem.AddAttribute.Describe('x1','-30%');
  elem.AddAttribute.Describe('y1','30%');
  elem.AddAttribute.Describe('x2','60%');
  elem.AddAttribute.Describe('y2','130%');
  elem.AddAttribute.Describe('gradientUnits','objectBoundingBox');

  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop1');
  subelem.AddAttribute.Describe('offset','0.4835');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop2');
  subelem.AddAttribute.Describe('offset','0.4977');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop3');
  subelem.AddAttribute.Describe('offset','0.5238');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop4');
  subelem.AddAttribute.Describe('offset','0.5543');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop5');
  subelem.AddAttribute.Describe('offset','0.5858');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop6');
  subelem.AddAttribute.Describe('offset','0.6226');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop7');
  subelem.AddAttribute.Describe('offset','0.6656');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop8');
  subelem.AddAttribute.Describe('offset','0.7187');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop9');
  subelem.AddAttribute.Describe('offset','0.7932');
  subelem:=elem.AddElement.Describe('stop');
  subelem.AddAttribute.Describe('id','sitemap_sm_pressed1_stop10');
  subelem.AddAttribute.Describe('offset','1');

  Result[8]:=elem;
end;

end.

