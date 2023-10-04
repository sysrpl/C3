(********************************************************)
(*                                                      *)
(*  Codebot Cloud Client                                *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

program C3;

{$i c3.inc}

uses
  Codebot.System,
  Interfaces, Forms,
  ExploreFrm, DialogTools, S3Actions, S3Graph, S3Objects;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'C3';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TExploreForm, ExploreForm);
  Application.Run;
end.

