PROGRAM digitaltrainer;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, digitaltrainerMain, baseGate
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.Scaled:=true;
  Application.initialize;
  Application.CreateForm(TDigitaltrainerMainForm, DigitaltrainerMainForm);
  Application.run;
end.

