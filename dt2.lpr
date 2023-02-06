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
  Forms, dtMain
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.Scaled:=true;
  Application.initialize;
  Application.CreateForm(TDigitaltrainerMainForm, DigitaltrainerMainForm);
  Application.run;
end.

