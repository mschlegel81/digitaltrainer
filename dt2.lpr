PROGRAM dt2;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}
  cthreads,
  {$endif}
  {$IFDEF HASAMIGA}
  athreads,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, dtMain,visuals,workspaces;

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.title:='Digitaltrainer';
  initializeWorkspaces;
  initializeVisuals;
  Application.Scaled:=true;
  Application.initialize;
  Application.CreateForm(TDigitaltrainerMainForm, DigitaltrainerMainForm);
  Application.run;
end.

